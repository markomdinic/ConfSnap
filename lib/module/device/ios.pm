#
# module::device::ios.pm
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

package module::device::ios;

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
    DIRECTIVE('enable', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'enable' => '$VALUE' } }))),
    DIRECTIVE('filter', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'filter' => '$VALUE' } })))
);

##############################################################################################

use constant {
    RE_LOGIN	=> '(?:[Uu]ser(?:name)?|[Ll]ogin):',
    RE_PASSWD	=> '[Pp]ass(?:word)?:',
    RE_PROMPT	=> '[\r\n][\w\-\+\.\:\@]+[>#]',
    RE_FAILED	=> '(?:%\s*)?(?:[Aa]uthentication|[Ll]ogin|[Aa]ccess)\s+(?:[Ff]ailed|[Dd]enied)'
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

sub admin_password($)
{
    my $self = shift;

    return defined($self->{'enable'}) ? $self->{'enable'}:'';
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
	    # Create new SSH client and connect to IOS device
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
				 'Errmode' => 'return',
				 'Timeout' => $self->{'connect_timeout'});
	# Telnet to IOS device
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

    my $proto = $self->protocol;
    return undef unless(defined($proto) && $proto ne '');

    # Get connect timeout
    my $timeout = $self->{'connect_timeout'};

    # Get enable password
    my $enable = $self->admin_password;

    # If selected protocol is SSH ...
    if($proto eq 'ssh') {

	# ... log in ...
	my $m = $conn->login();
	# ... check for prompt ...
	return 0 unless(defined($m) && $m =~ /@{[RE_PROMPT]}/);

	# ... change to enable mode, if configured
	if(defined($enable) && $enable ne '') {
	    $conn->send("enable");
	    $conn->waitfor(&RE_PASSWD, $timeout)
		or return 0;
	    $conn->send($enable);
	    $conn->waitfor('(?:'.&RE_PROMPT.'|'.&RE_FAILED.')', $timeout)
		or return 0;
	    # ... again, check for prompt
	    $m = $conn->match();
	    return 0 unless(defined($m) && $m =~ /@{[RE_PROMPT]}/);
	}

    # If selected protocol is telnet ...
    } elsif($proto eq 'telnet') {

	# At least password must be defined
	my $pass = $self->password;
	return 0 unless defined($pass);
	# Get username
	my $user = $self->username;
	# Send username, if defined
	if(defined($user) && $user ne '') {
	    $conn->waitfor('Match' => '/'.&RE_LOGIN.'/',
			   'Timeout' => $timeout)
		or return 0;
	    $conn->put($user."\n");
	}

	# Send password
	$conn->waitfor('Match' => '/'.&RE_PASSWD.'/',
		       'Timeout' => $timeout)
	    or return 0;
	$conn->put($pass."\n");
	# Wait for command prompt or auth failed message
	my ($p, $m) = $conn->waitfor('Match' => '/'.&RE_FAILED.'|'.&RE_PROMPT.'/',
				     'Timeout' => $timeout);
	# If login failed, abort
	return 0 if($m =~ /@{[RE_FAILED]}/);

	# ... change to enable mode, if configured
	if(defined($enable) && $enable ne '') {
	    $conn->put("enable\n");
	    $conn->waitfor('Match' => '/'.&RE_PASSWD.'/',
			   'Timeout' => $timeout)
		or return 0;
	    $conn->put($enable."\n");
	    ($p, $m) = $conn->waitfor('Match' => '/(?:'.&RE_PROMPT.'|'.&RE_FAILED.')/',
				      'Timeout' => $timeout);
	    # ... check for prompt
	    return 0 unless(defined($m) && $m =~ /@{[RE_PROMPT]}/);
	}

    }

    return 1;
}

sub collect($$)
{
    my ($self, $conn) = @_;
    my @cfg = ();

    my $proto = $self->protocol;
    return undef unless(defined($proto) && $proto ne '');

    # Get conversation timeout
    my $timeout = $self->{'read_timeout'};

    # If protocol is set to SSH ...
    if($proto eq 'ssh') {

	# ... disable pagination
	$conn->send("terminal length 0");
	$conn->waitfor(&RE_PROMPT, $timeout)
	    or return undef;
	# ... collect configuration
	$conn->send("show running-config");
	if($conn->waitfor(&RE_PROMPT, $timeout)) {
	    my $dump = $conn->before();
	    if(defined($dump) && $dump ne '') {
		# Strip CRs ...
		$dump =~ s/[\r]//g;
		# ... and split content without stripping LFs
		@cfg = ($dump =~ /([^\n]*\n)/g);
	    }
	}

    # If protocol is telnet ...
    } elsif($proto eq 'telnet') {

	# ... disable pagination
	$conn->cmd('String' => "terminal length 0",
		   'Timeout' => $timeout)
	    or return undef;
	# ... collect configuration
	@cfg = $conn->cmd('String' => "show running-config",
			  'Timeout' => $timeout);

    }

    # If we got something ...
    if(@cfg) {
	# ... skip leading trash
	for(;scalar(@cfg) > 0 && $cfg[0] !~ /^Current\s+configuration/i; shift @cfg) {}
	for(shift @cfg; scalar(@cfg) > 0 && $cfg[0] eq ''; shift @cfg) {}
	# ... skip trailing trash
	for(;scalar(@cfg) > 0 && $cfg[$#cfg] =~ /^(?:@{[RE_PROMPT]})?$/; pop @cfg) {}
	# ... and if filter regexp is defined ...
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
	    $conn->send("ssh -l ".$user." ".((defined($vrf) && $vrf ne "") ? "-vrf ".$vrf." ":"").$host);
	} elsif($proto eq 'telnet') {
	    $conn->put("ssh -l ".$user." ".((defined($vrf) && $vrf ne "") ? "-vrf ".$vrf." ":"").$host."\n");
	}

    # If remote device's protocol is telnet,
    # telnet to the device on the remote end
    } elsif($remote_proto eq 'telnet') {

	if($proto eq 'ssh') {
	    $conn->send("telnet ".$host.((defined($vrf) && $vrf ne "") ? " /vrf ".$vrf:""));
	} elsif($proto eq 'telnet') {
	    $conn->put("telnet ".$host.((defined($vrf) && $vrf ne "") ? " /vrf ".$vrf:"")."\n");
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
	$conn->send("exit");

    # If protocol is set to telnet ...
    } elsif($self->protocol eq 'telnet') {

	# ... send quit
	$conn->put("exit\n");
    
    }
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    return unless defined($conn);

    $conn->close;
}

1;
