#
# module::device::mikrotik.pm
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

package module::device::mikrotik;

##############################################################################################

use strict;
use warnings;

##############################################################################################

use Net::Telnet;
use Net::OpenSSH;
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
    RE_PASSWD	=> '[Pp]ass(?:word)?:',
    RE_PROMPT	=> '\[[^\[\]]+\]\s+>\s+',
    RE_FAILED	=> '[Ll]ogin\s+[Ff]ailed\s*,?'
};

##############################################################################################

sub register()
{
    return $CONF_TEMPLATE;
}

##############################################################################################

sub connect($$)
{
    my ($self, $host) = @_;

    return undef unless(defined($host) && $host ne "");

    my $conn;

    # Connect using SSH ?
    if($self->{'protocol'} eq "ssh") {
	# Create new SSH client and connect to RouterOS device
	# (append '+ct' to the username to disable colors)
	my $ssh = Net::OpenSSH->new($host,
				    'user' => defined($self->{'username'}) ? $self->{'username'}.'+ct':"",
				    'password' => defined($self->{'password'}) ? $self->{'password'}:"");
	# Use SSH client as terminal slave
	my ($pty, $pid) = $ssh->open2pty({stderr_to_stdout => 1});
	# Use telnet module as terminal handler
	$conn = Net::Telnet->new(Fhopen => $pty,
				 Telnetmode => 0,
				 Timeout => $self->{'timeout'});
    # Connect using telnet ?
    } elsif($self->{'protocol'} eq "telnet") {
	# Create new telnet client
	$conn = Net::Telnet->new(Timeout => $self->{'timeout'});
	# Telnet to RouterOS device
	$conn->open($host);
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

    # Wait for login, password or command prompt
    my ($p, $m) = $conn->waitfor('/'.&RE_LOGIN.'|'.&RE_PASSWD.'|'.&RE_PROMPT.'/');
    # If we got login prompt ...
    if($m =~ /@{[RE_LOGIN]}/) {
	# ... send username (and append '+ct' to disable colors)
	$conn->put((defined($self->{'username'}) ? $self->{'username'}.'+ct':"")."\n");
	# ... wait for password or command prompt
	($p, $m) = $conn->waitfor('/'.&RE_PASSWD.'|'.&RE_PROMPT.'/');
    }
    # If we got password prompt ...
    if($m =~ /@{[RE_PASSWD]}/) {
	# ... send password
	$conn->put((defined($self->{'password'}) ? $self->{'password'}:"")."\n");
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

    # Collect running config
    my @cfg = $conn->cmd("/export verbose");
    # If we got something ...
    if(@cfg) {
	# Skip leading trash
	for(;scalar(@cfg) > 0 && $cfg[0] =~ /^[\e\r\n\#]/; shift @cfg) {}
	# Skip trailing trash
	for(;scalar(@cfg) > 0 && $cfg[$#cfg] =~ /^[\e\r\n\#]/; pop @cfg) {}
	# If filter regexp is defined ...
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

    $conn->cmd("/quit");
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    $conn->close;
}

1;
