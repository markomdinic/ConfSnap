#
# module::device::asa.pm
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

package module::device::asa;

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
    RE_PASSWD	=> '[Pp]ass(?:word)?:',
    RE_PROMPT	=> '[\r\n][\w\-\+\.\:\@]+[>#]\s*',
    RE_INVALID	=> '[Ii]nvalid\s+[Pp]assword'
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

    return defined($self->{'protocol'}) ? $self->{'protocol'}:'ssh';
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
    } elsif($proto eq 'https') {
	$port = 443;
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

    return undef unless(defined($host) && $host ne "");

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
	# Prepare credentials
	my $user = $self->username;
	return undef unless defined($user);
	my $pass = $self->password;
	return undef unless defined($pass);
	eval {
	    # Create new SSH client and connect to ASA device
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
				 'Errmode' => 'return',
				 'Timeout' => $self->{'connect_timeout'});
	# Telnet to ASA device
	$conn->open($host);

    # Connect using https ?
    } elsif($proto eq 'https') {

	# Safely load Net::NetSSLeay on demand
	$self->api->load_module('Net::SSLeay', 'get_https', 'make_headers')
	    or return undef;
	# Safely load MIME::Base64 on demand
	$self->api->load_module('MIME::Base64')
	    or return undef;
	# There's nothing to do here for Net::SSLeay,
	# but we can pass host as the connection handle,
	# since we must return something other than undef
	# anyway
	$conn = $host;

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

    return unless(defined($conn) && $self->protocol =~ /^ssh|telnet$/);

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

    # If selected protocol is HTTPS ...
    if($proto eq 'https') {

	# ... we are authenticating via HTTP(S),
	# so we can safely return true here
	return 1;

    # If selected protocol is SSH ...
    } elsif($proto eq 'ssh') {

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
	    $conn->waitfor('(?:'.&RE_PROMPT.'$|'.&RE_INVALID.')', $timeout)
		or return 0;
	    # ... again, check for prompt
	    $m = $conn->match();
	    return 0 unless(defined($m) && $m =~ /@{[RE_PROMPT]}/);
	}

    # If selected protocol is telnet ...
    } elsif($proto eq 'telnet') {

	my $user = $self->username;
	return 0 unless defined($user);
	my $pass = $self->password;
	return 0 unless defined($pass);
	# Authenticate to ASA device
	$conn->login($user, $pass)
	    or return 0;

	# ... change to enable mode, if configured
	if(defined($enable) && $enable ne '') {
	    $conn->put("enable\n");
	    $conn->waitfor('Match' => '/'.&RE_PASSWD.'/',
			   'Timeout' => $timeout)
		or return 0;
	    $conn->put($enable."\n");
	    my ($p, $m) = $conn->waitfor('Match' => '/(?:'.&RE_PROMPT.'|'.&RE_INVALID.')/',
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

    my $port = $self->port;
    return undef unless(defined($port) && $port > 0 && $port < 65536);

    # Get conversation timeout
    my $timeout = $self->{'read_timeout'};

    # If connected via SSH ...
    if($proto eq 'ssh') {

	# ... disable pagination
	$conn->send("terminal pager 0");
	$conn->waitfor(&RE_PROMPT, $timeout)
	    or return undef;
	# ... flush buffer
	if($conn->eat($conn->after()) eq '') {
	    $conn->waitfor(&RE_PROMPT, $timeout);
	}
	# ... collect configuration
	$conn->send("more system:running-config");
	if($conn->waitfor(&RE_PROMPT, $timeout)) {
	    my $dump = $conn->before();
	    if(defined($dump) && $dump ne '') {
		# Strip CRs ...
		$dump =~ s/[\r]//g;
		# ... and split content without stripping LFs
		@cfg = ($dump =~ /([^\n]*\n)/g);
	    }
	}

    # If connected via telnet ...
    } elsif($proto eq 'telnet') {

	# ... disable pagination
	$conn->cmd('String' => "terminal pager 0",
		   'Timeout' => $timeout)
	    or return undef;
	# ... collect configuration
	@cfg = $conn->cmd('String' => "more system:running-config",
			  'Timeout' => $timeout);

    # If connected via https ...
    } elsif($proto eq 'https') {

	# ... prepare credentials
	my $user = $self->username;
	return undef unless defined($user);
	my $pass = $self->password;
	return undef unless defined($pass);
	# ... generate authentication hash
	my $auth = 'Basic '.MIME::Base64::encode($user.":".$pass);
	# ... request configuration over HTTPS
	my ($dump, $response, %headers) = get_https($conn, $port, "/config",
						    make_headers('Authorization' => $auth));
	# ... and if GET was successful ...
	if($response =~ /HTTP\/1\.[0-1]\s+200\s+(?:OK)?$/i) {
	    # ... strip CRs ...
	    $dump =~ s/[\r]//g;
	    # ... and split content without stripping LFs
	    @cfg = ($dump =~ /([^\n]*\n)/g);
	}

    }

    # If we got something ...
    if(@cfg) {
	#  ... skip leading trash
	for(;scalar(@cfg) > 0 && $cfg[0] !~ /ASA\s+Version\s+\S+/i; shift @cfg) {}
	for(shift @cfg; scalar(@cfg) > 0 && $cfg[0] eq ''; shift @cfg) {}
	# ... skip trailing trash
	for(;scalar(@cfg) > 0 && $cfg[$#cfg] !~ /^Cryptochecksum:/i; pop @cfg) {}
	for(pop @cfg; scalar(@cfg) > 0 && $cfg[$#cfg] eq ''; pop @cfg) {}
	# ... and if filter regexp is defined ...
	if(defined($self->{'filter'}) && $self->{'filter'} ne "") {
	    # ... remove all matching lines
	    @cfg = grep(!/$self->{'filter'}/, @cfg);
	}
    }

    # If we got config, return it as string.
    # Otherwise, return undef
    return (scalar(@cfg) > 0) ? join('', @cfg):undef;
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

    return unless(defined($conn) && $self->protocol =~ /^ssh|telnet$/);

    $conn->close;
}

1;
