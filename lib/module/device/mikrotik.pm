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
	# Prepare credentials
	my $user = (defined($self->{'username'}) &&
		    $self->{'username'} ne "") ?
			$self->{'username'}:"";
	my $pass = (defined($self->{'password'}) &&
		    $self->{'password'} ne "") ?
			$self->{'password'}:"";
	# Create new SSH connection
	$conn = Net::OpenSSH->new($host, 'user' => $user, 'password' => $pass);
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

    return unless(defined($conn) && ref($conn) eq 'Net::Telnet');

    return $conn->prompt('/\[[^\[\]]+\]\s+>\s+$/');
}

sub auth($$)
{
    my ($self, $conn) = @_;

    return 0 unless(defined($conn));

    # Connected via SSH ?
    if(ref($conn) eq 'Net::OpenSSH') {
	return 1 unless $conn->error;
    # Connected via telnet ?
    } elsif((ref($conn) eq 'Net::Telnet')) {
	# Send username, if defined
	if(defined($self->{'username'})) {
	    $conn->waitfor('/(?:[Uu]ser(?:name)?|[Ll]ogin):/');
	    $conn->put($self->{'username'}."+ct\n");
	}
	# Send password, if defined
	$conn->waitfor('/[Pp]ass(?:word)?:/');
	$conn->cmd(defined($self->{'password'}) ? $self->{'password'}:"");
	# Success
	return 1;
    }

    return 0;
}

sub collect($$)
{
    my ($self, $conn) = @_;

    my @cfg = ();

    # Connected via SSH ?
    if(ref($conn) eq 'Net::OpenSSH') {
	# Collect running config
	@cfg = $conn->capture("/export verbose\n");
    # Connected via telnet ?
    } elsif(ref($conn) eq 'Net::Telnet') {
	# Collect running config
	@cfg = $conn->cmd("/export verbose");
    }
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

    return unless(defined($conn) && ref($conn) eq 'Net::Telnet');

    $conn->cmd("/quit");
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    return unless(defined($conn) && ref($conn) eq 'Net::Telnet');

    $conn->close;
}

1;
