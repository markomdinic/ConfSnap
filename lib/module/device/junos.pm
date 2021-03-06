#
# module::device::junos.pm
#
# Copyright (c) 2019 Marko Dinic. All rights reserved.
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

package module::device::junos;

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
    DIRECTIVE('connect_timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'connect_timeout' => '$VALUE' } }))),
    DIRECTIVE('read_timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'read_timeout' => '$VALUE' } }), DEFAULT '10')),
    DIRECTIVE('timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'timeout' => '$VALUE' } }), DEFAULT '10')),
    DIRECTIVE('protocol', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'protocol' => '$VALUE' } }), DEFAULT 'ssh')),
    DIRECTIVE('port', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'port' => '$VALUE' } }))),
    DIRECTIVE('username', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'username' => '$VALUE' } }))),
    DIRECTIVE('password', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'password' => '$VALUE' } }))),
    DIRECTIVE('filter', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'filter' => '$VALUE' } })))
);

##############################################################################################

use constant {
    RE_PROMPT	=> '(?:\{[^\{\}\r\n]+\})?(?:\[edit\])?[\n\r]+[\w\-\+\.\,\;\:]+\@[^\n\r\s\@\#\>\$\/\"\'\`]+[>#]',
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

sub port($)
{
    my $self = shift;

    return $self->{'port'} if defined($self->{'port'});

    my $port;

    my $proto = $self->protocol;
    if($proto eq 'ssh') {
	$port = 22;
    } elsif($proto eq 'telnet') {
	$port = 23;
    }

    return $port;
}

sub username($)
{
    my $self = shift;

    return defined($self->{'username'}) ? $self->{'username'}:undef;
}

sub password($)
{
    my $self = shift;

    return defined($self->{'password'}) ? $self->{'password'}:'';
}


##############################################################################################

sub connect($$)
{
    my ($self, $host) = @_;

    return undef unless(defined($host) && $host ne '');

    my $proto = $self->protocol;
    return undef unless(defined($proto) && $proto ne '');

    my $port = $self->port;
    return undef unless(defined($port) && $port > 0 && $port < 65536);

    my $conn;

    # Connect using SSH ?
    if($proto eq 'ssh') {

	# Safely load Net::SSH::Expect on demand
	$self->api->load_module('Net::SSH::Expect')
	    or return undef;
	# Get login credentials
	my $user = $self->username;
	return undef unless defined($user);
	my $pass = $self->password;
	return undef unless defined($pass);
	eval {
	    # Create new SSH client and connect to JunOS device
	    $conn = Net::SSH::Expect->new('host' => $host,
					  'port' => $port,
					  'user' => $user,
					  'password' => $pass,
					  'ssh_option' => '-q -oStrictHostKeyChecking=no',
					  'raw_pty' => 1,
					  'timeout' => $self->{'connect_timeout'});
	};
	# Abort on error
	return undef if($@ || !defined($conn));

    # Connect using telnet ?
    } elsif($proto eq 'telnet') {

	# Safely load Net::Telnet on demand
	$self->api->load_module('Net::Telnet')
	    or return undef;
	# Create new telnet client
	$conn = Net::Telnet->new('Port' => $port,
				 'Timeout' => $self->{'connect_timeout'});
	# Telnet to JunOS device
	$conn->open($host);

    # Other protocols are not supported
    } else {

	$self->api->logging('LOG_ERR', "Protocol %s is not supported by %s", $proto, ref($self));
	return undef;

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

    # If selected protocol is SSH ...
    if($self->protocol eq 'ssh') {

	# ... log in ...
	my $m = $conn->login();
	# ... check for prompt ...
	return 0 unless(defined($m) && $m =~ /@{[RE_PROMPT]}/);

    # If selected protocol is telnet ...
    } elsif($self->protocol eq 'telnet') {

	# Get credentials
	my $user = $self->username;
	return 0 unless defined($user);
	my $pass = $self->password;
	return 0 unless defined($pass);

	# Login to JunOS device
	$conn->login($user, $pass)
	    or return 0;

    }

    return 1;
}

sub collect($$)
{
    my ($self, $conn) = @_;
    my @cfg = ();

    # Get conversation timeout
    my $timeout = $self->{'read_timeout'};

    # If protocol is set to SSH ...
    if($self->protocol eq 'ssh') {

	# ... disable pagination
	$conn->send("set cli screen-length 0");
	$conn->waitfor(&RE_PROMPT, $timeout)
	    or return undef;
	# ... flush buffer
	if($conn->eat($conn->after()) eq '') {
	    $conn->waitfor(&RE_PROMPT, $timeout);
	}
	# ... collect configuration
	$conn->send("show configuration");
	if($conn->waitfor(&RE_PROMPT, $timeout)) {
	    my $dump = $conn->before();
	    if(defined($dump) && $dump ne '') {
		$dump =~ s/[\r]//g;
		@cfg = ($dump =~ /([^\n]*\n)/g);
	    }
	}
    # If protocol is telnet ...
    } elsif($self->protocol eq 'telnet') {

	# ... disable pagination
	$conn->cmd('String' => "set cli screen-length 0",
		   'Timeout' => $timeout);
	# ... collect running config
	@cfg = $conn->cmd('String' => "show configuration",
			  'Timeout' => $timeout);

    }

    # If we got something ...
    if(@cfg) {
	# ... skip leading trash
	for(;scalar(@cfg) > 0 && $cfg[0] !~ /^\#+/; shift @cfg) {}
	# ... skip trailing trash
	for(;scalar(@cfg) > 0 && $cfg[$#cfg] =~ /^(?:@{[RE_PROMPT]})?$/; pop @cfg) {}
	# If filter regexp is defined ...
	if(scalar(@cfg) > 0 && defined($self->{'filter'}) && $self->{'filter'} ne "") {
	    # ... remove all matching lines
	    @cfg = grep(!/$self->{'filter'}/, @cfg);
	}
    }

    # If we got config, return it as string.
    # Otherwise, return undef
    return (scalar(@cfg) > 0) ? join('', @cfg):undef;
}

sub remote($$$;$)
{
    my ($self, $remote, $conn, $host, $vrf) = @_;

    return 0 unless defined($conn);

    my $proto = $self->protocol;
    my $remote_proto = $remote->protocol;

    # If remote device's protocol is SSH,
    # SSH to the device on the remote end
    if($remote_proto eq 'ssh') {

	my $user = $remote->username;
	return 0 unless defined($user);
	if($proto eq 'ssh') {
	    $conn->send("ssh ".((defined($vrf) && $vrf ne "") ? "routing-instance ".$vrf." ":"").$user.'@'.$host);
	} elsif($proto eq 'telnet') {
	    $conn->put("ssh ".((defined($vrf) && $vrf ne "") ? "routing-instance ".$vrf." ":"").$user.'@'.$host."\n");
	}

    # If remote device's protocol is telnet,
    # telnet to the device on the remote end
    } elsif($remote_proto eq 'telnet') {

	if($proto eq 'ssh') {
	    $conn->send("telnet ".((defined($vrf) && $vrf ne "") ? "routing-instance ".$vrf." ":"").$host);
	} elsif($proto eq 'telnet') {
	    $conn->put("telnet ".((defined($vrf) && $vrf ne "") ? "routing-instance ".$vrf." ":"").$host."\n");
	}

    # Other remote protocols are not supported
    } else {

	$self->api->logging('LOG_ERR', "Protocol %s is not supported by %s for indirect device access", $remote_proto, ref($self));
	return 0;

    }

    return 1;
}

sub end($$)
{
    my ($self, $conn) = @_;

    return unless defined($conn);

    # If protocol is set to SSH ...
    if($self->protocol eq 'ssh') {

	# ... send quit
	$conn->exec("exit");

    # If protocol is set to telnet ...
    } elsif($self->protocol eq 'telnet') {

	# ... send quit
	$conn->cmd("exit");

    }
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    return unless defined($conn);

    $conn->close;
}

1;
