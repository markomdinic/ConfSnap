#
# module::device::mikrotik.pm
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

package module::device::mikrotik;

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
    RE_LOGIN	=> '(?:[Uu]ser(?:name)?|[Ll]ogin):',
    RE_PASSWD	=> '[Pp]ass(?:word)?:',
    RE_PROMPT	=> '(?:\r+[^\n\r]+)?\[[^\[\]]+\]\s+>\s+',
    RE_FAILED	=> '[Ll]ogin\s+[Ff]ailed\s*,?'
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

    # Append '+ct' to username to disable colors
    return (defined($self->{'username'}) &&
	    $self->{'username'} ne '') ?
		    $self->{'username'}.'+ct':undef,
}

sub password($)
{
    my $self = shift;

    return defined($self->{'password'}) ?
		    $self->{'password'}:'';
}

##############################################################################################

sub connect($$)
{
    my ($self, $host) = @_;

    return undef unless(defined($host) && $host ne "");

    my $proto = $self->protocol;
    return undef unless defined($proto);

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
	    # Create new SSH client and connect to RouterOS device
	    $conn = Net::SSH::Expect->new('host' => $host,
					  'port' => $port,
					  'user' => $user,
					  'password' => $pass,
					  'ssh_option' => '-q -oStrictHostKeyChecking=no',
					  'terminator' => "\r\n",
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
	# Telnet to RouterOS device
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

    return $conn->prompt('/'.&RE_PROMPT.'$/');
}


sub auth($$)
{
    my ($self, $conn) = @_;

    return 0 unless(defined($conn));

    # If selected protocol is SSH ...
    if($self->protocol eq 'ssh') {

	# ... log in ...
	my $m = $conn->login();
	# ... check for prompt ...
	return defined($m) && $m =~ /@{[RE_PROMPT]}/;

    # If selected protocol is telnet ...
    } elsif($self->protocol eq 'telnet') {

	# Get connection timeout
	my $timeout = $self->{'connect_timeout'};

	# Wait for login, password or command prompt
	my ($p, $m) = $conn->waitfor('Match' => '/'.&RE_LOGIN.'|'.&RE_PASSWD.'|'.&RE_PROMPT.'/',
				     'Timeout' => $timeout);
	# If we got login prompt ...
	if($m =~ /@{[RE_LOGIN]}/) {
	    my $user = $self->username;
	    return 0 unless defined($user);
	    # ... send username
	    $conn->put($user."\n");
	    # ... wait for password or command prompt
	    ($p, $m) = $conn->waitfor('Match' => '/'.&RE_PASSWD.'|'.&RE_PROMPT.'/',
				      'Timeout' => $timeout);
	}
	# If we got password prompt ...
	if($m =~ /@{[RE_PASSWD]}/) {
	    my $pass = $self->password;
	    return 0 unless defined($pass);
	    # ... send password
	    $conn->put($pass."\n");
	    # ... wait for command prompt or login failed message
	    ($p, $m) = $conn->waitfor('Match' => '/'.&RE_FAILED.'|'.&RE_PROMPT.'/',
				      'Timeout' => $timeout);
	    # If login failed, abort
	    return 0 if($m =~ /@{[RE_FAILED]}/);
	}
        # At this point we are at the command prompt
	# one way or another (unless login failed).
	return 1;

    }

    return 0;
}

sub collect($$)
{
    my ($self, $conn) = @_;
    my @cfg = ();

    # Get conversation timeout
    my $timeout = $self->{'read_timeout'};

    # If protocol is set to SSH ...
    if($self->protocol eq 'ssh') {

	# ... collect running config
	$conn->send("/export verbose terse");
	if($conn->waitfor(&RE_PROMPT, $timeout)) {
	    my $dump = $conn->before();
	    if(defined($dump) && $dump ne '') {
		$dump =~ s/[\r]//g;
		@cfg = ($dump =~ /([^\n]*\n)/g);
	    }
	}

    # If selected protocol is telnet ...
    } elsif($self->protocol eq 'telnet') {

	# Collect running config
	@cfg = $conn->cmd('String' => "/export verbose terse",
			  'Timeout' => $timeout);

    }

    # If we got something ...
    if(@cfg) {
	# Skip leading trash
	for(;scalar(@cfg) > 0 && $cfg[0] =~ /^[\e\r\n\#]|\/export/i; shift @cfg) {}
	# Skip trailing trash
	for(;scalar(@cfg) > 0 && $cfg[$#cfg] =~ /^[\e\r\n\#]|interrupt/i; pop @cfg) {}
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
	    $conn->send("/system ssh user=".$user." ".((defined($vrf) && $vrf ne "") ? "routing-table=".$vrf." ":"").$host);
	} elsif($proto eq 'telnet') {
	    $conn->put("/system ssh user=".$user." ".((defined($vrf) && $vrf ne "") ? "routing-table=".$vrf." ":"").$host."\n");
	}

    # If remote device's protocol is telnet,
    # telnet to the device on the remote end
    } elsif($remote_proto eq 'telnet') {

	if($proto eq 'ssh') {
	    $conn->send("/system telnet ".((defined($vrf) && $vrf ne "") ? "routing-table=".$vrf." ":"").$host);
	} elsif($proto eq 'telnet') {
	    $conn->put("/system telnet ".((defined($vrf) && $vrf ne "") ? "routing-table=".$vrf." ":"").$host."\n");
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
	$conn->send("/quit");

    # If protocol is set to telnet ...
    } elsif($self->protocol eq 'telnet') {

	# ... send quit
	$conn->put("/quit\n");

    }
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    return unless defined($conn);

    $conn->close;
}

1;
