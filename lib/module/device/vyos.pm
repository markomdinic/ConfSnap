#
# module::device::vyos.pm
#
# Copyright (c) 2018 Marko Dinic. All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

package module::device::vyos;

##############################################################################################

use strict;
use warnings;

##############################################################################################

use Config::ContextSensitive qw(:macros);

##############################################################################################

use api::module;

##############################################################################################

our @ISA = qw(api::module);

##############################################################################################

our $CONF_TEMPLATE = SECTION(
    DIRECTIVE('timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'timeout' => '$VALUE' } }), DEFAULT '10')),
    DIRECTIVE('protocol', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'protocol' => '$VALUE' } }), DEFAULT 'ssh')),
    DIRECTIVE('username', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'username' => '$VALUE' } }))),
    DIRECTIVE('password', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'password' => '$VALUE' } }))),
    DIRECTIVE('filter', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'filter' => '$VALUE' } })))
);

##############################################################################################

use constant {
    RE_LOGIN	=> '(?:[Uu]ser(?:name)?|[Ll]ogin):',
    RE_PASSWD	=> '(?:[^\n\r]+\s+)?[Pp]ass(?:word)?:',
    RE_PROMPT	=> '(?:\[edit\][\n\r]+)?[^\n\r\@\#\$]+\@[^\@\#\$]+[\#\$]',
    RE_FAILED	=> '[Ll]ogin\s+[Ii]ncorrect\s*'
};

##############################################################################################

sub register()
{
    return $CONF_TEMPLATE;
}

##############################################################################################

sub protocol($)
{
    my $self = shift;

    return defined($self->{'protocol'}) ?
		    $self->{'protocol'}:'ssh';
}

##############################################################################################

sub connect($$)
{
    my ($self, $host) = @_;

    return undef unless(defined($host) && $host ne "");

    my $conn;

    # Safely load Net::Telnet on demand
    $self->api->load_module('Net::Telnet')
	or return undef;

    # Connect using SSH ?
    if($self->protocol eq 'ssh') {
	# Safely load Net::OpenSSH on demand
	$self->api->load_module('Net::OpenSSH')
	    or return undef;
	# Get login credentials
	my $user = $self->{'username'};
	return undef unless defined($user);
	my $pass = $self->{'password'};
	return undef unless defined($pass);
	# Create new SSH client and connect to VyOS device
	my $ssh = Net::OpenSSH->new($host,
				    'user' => $user,
				    'password' => $pass,
				    'master_stderr_discard' => 1);
	# Abort on error
	return undef if $ssh->error;
	# Use SSH client as terminal slave
	my ($pty, $pid) = $ssh->open2pty({'stderr_to_stdout' => 1});
	# Use telnet module as terminal handler
	$conn = Net::Telnet->new('Fhopen' => $pty,
				 'Telnetmode' => 0,
				 'Binmode' => 1,
				 'Cmd_remove_mode' => 1,
				 'Timeout' => $self->{'timeout'});
    # Connect using telnet ?
    } elsif($self->protocol eq 'telnet') {
	# Create new telnet client
	$conn = Net::Telnet->new('Timeout' => $self->{'timeout'});
	# Telnet to VyOS device
	$conn->open($host);
    }

    return $conn;
}

sub prompt($$)
{
    my ($self, $conn) = @_;

    return $conn->prompt('/'.&RE_PROMPT.'/');
}


sub auth($$)
{
    my ($self, $conn) = @_;

    return 0 unless(defined($conn));

    # Wait for login, password or command prompt
    my ($p, $m) = $conn->waitfor('/'.&RE_LOGIN.'|'.&RE_PASSWD.'|'.&RE_PROMPT.'/');
    # If we got login prompt ...
    if($m =~ /@{[RE_LOGIN]}/) {
	my $user = $self->{'username'};
	return 0 unless defined($user);
	# ... send username
	$conn->put($user."\n");
	# ... wait for password or command prompt
	($p, $m) = $conn->waitfor('/'.&RE_PASSWD.'|'.&RE_PROMPT.'/');
    }
    # If we got password prompt ...
    if($m =~ /@{[RE_PASSWD]}/) {
	my $pass = $self->{'password'};
	return 0 unless defined($pass);
	# ... send password
	$conn->put($pass."\n");
	# ... wait for command prompt or login failed message
	($p, $m) = $conn->waitfor('/'.&RE_FAILED.'|'.&RE_PROMPT.'/');
	# If login failed, abort
	return 0 if($m =~ /@{[RE_FAILED]}/);
    }
    # At this point we are at the command prompt
    # one way or another (unless login failed).
    return 1;
}

sub collect($$)
{
    my ($self, $conn) = @_;

    # Disable pagination
    $conn->cmd("terminal length 0");
    # Enter configuration mode
    $conn->cmd("configure");
    # Collect configuration
    my @cfg = $conn->cmd("show");
    # If we got something ...
    if(@cfg) {
	# If filter regexp is defined ...
	if(defined($self->{'filter'}) && $self->{'filter'} ne "") {
	    # ... remove all matching lines
	    @cfg = grep(!/$self->{'filter'}/, @cfg);
	}
    }
    # Exit configuration mode
    $conn->cmd("exit");
    # If we got config, return it as string.
    # Otherwise, return undef
    return (scalar(@cfg) > 0) ? join('', @cfg):undef;
}

sub remote($$$;$)
{
    my ($self, $remote, $conn, $host, $vrf) = @_;

    my $proto = $remote->protocol;

    if($proto eq 'ssh') {
	# Get remote device's login username
	my $user = $remote->{'username'};
	return 0 unless defined($user);
	# SSH to the router on the remote end
	$conn->put("ssh ".((defined($vrf) && $vrf ne "") ? "vrf ".$vrf." ":"").$user.'@'.$host."\n");
    } elsif($proto eq 'telnet') {
	# Telnet to the device on the remote end
	$conn->put("telnet ".((defined($vrf) && $vrf ne "") ? "vrf ".$vrf." ":"").$host."\n");
    } else {
	$self->api->logging('LOG_ERR', "Protocol %s is not supported by %s for indirect device access", $proto, ref($self));
	return 0;
    }

    return 1;
}

sub end($$)
{
    my ($self, $conn) = @_;

    $conn->cmd("exit");
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    $conn->close;
}

1;
