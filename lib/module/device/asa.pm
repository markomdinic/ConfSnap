#
# module::device::asa.pm
#
# Copyright (c) 2017 Marko Dinic. All rights reserved.
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

use Net::Telnet;
use Net::OpenSSH;
use MIME::Base64;
use Net::SSLeay qw(get_https make_headers);
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
    DIRECTIVE('enable', ARG(CF_STRING, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'enable' => '$VALUE' } }))),
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
	my $user = $self->{'username'};
	return undef unless(defined($user) && $user ne "");
	my $pass = $self->{'password'};
	return undef unless(defined($pass) && $pass ne "");
	# Create new SSH connection
	$conn = Net::OpenSSH->new($host, 'user' => $user, 'password' => $pass);
    # Connect using telnet ?
    } elsif($self->{'protocol'} eq "telnet") {
	# Create new telnet client
	$conn = Net::Telnet->new(Timeout => $self->{'timeout'});
	# Telnet to ASA device
	$conn->open($host);
    # Connect using https ?
    } elsif($self->{'protocol'} eq "https") {
	# There's nothing to do here for Net::SSLeay,
	# but we can pass host as connection handle,
	# since we must return something other than undef
	# anyway
	$conn = $host;
    }

    return $conn;
}

sub prompt($$)
{
    my ($self, $conn) = @_;

    return unless(defined($conn) && $self->{'protocol'} eq "telnet");

    return $conn->prompt('/[a-zA-Z0-9\_\-\+]+[>#]$/');
}

sub auth($$)
{
    my ($self, $conn) = @_;

    return 1 unless(defined($conn) && $self->{'protocol'} eq "telnet");

    my $user = $self->{'username'};
    return 0 unless(defined($user) && $user ne "");
    my $pass = $self->{'password'};
    return 0 unless(defined($pass) && $pass ne "");
    my $enable = $self->{'enable'};
    return 0 unless(defined($enable) && $enable ne "");

    # Authenticate to ASA device
    $conn->login($user, $pass);

    # Change to privileged mode
    $conn->cmd("q\benable\n".$enable."\n");

    return 1;
}

sub collect($$)
{
    my ($self, $conn) = @_;

    my @cfg = ();

    # Connected via SSH ?
    if($self->{'protocol'} eq "ssh") {
	my $enable = $self->{'enable'};
	return undef unless(defined($enable) && $enable ne "");
	# Collect running config
	@cfg = $conn->capture("q\benable\n".$enable."\nterminal pager 0\nshow running-config\nexit\n");
    # Connected via telnet ?
    } elsif($self->{'protocol'} eq "telnet") {
	# Disable pagination
	$conn->cmd("terminal pager 0");
	# Collect running config
	@cfg = $conn->cmd("show running-config");
    # Connected via https ?
    } elsif($self->{'protocol'} eq "https") {
	# Prepare credentials
	my $user = $self->{'username'};
	return undef unless(defined($user) && $user ne "");
	my $pass = $self->{'password'};
	return undef unless(defined($pass) && $pass ne "");
	# Generate authentication hash
	my $auth = 'Basic '.MIME::Base64::encode($user.":".$pass);
	# Request configuration over HTTPS
	my ($content, $response, %headers) = get_https($conn, 443, "/config",
						       make_headers('Authorization' => $auth));
	# GET successful ?
	if($response =~ /HTTP\/1\.[0-1]\s+200\s+(?:OK)?$/i) {
	    # Split content into separate lines,
	    # without stripping newlines
	    @cfg = ($content =~ /([^\n\r]+[\n\r]+)/g);
	}
    }
    # If we retrieved anything ...
    if(@cfg) {
	# Skip leading trash
	while((my $l = shift @cfg)) {
	    last if($l =~ /ASA\s+Version\s+\S+/i);
	}
	# Skip trailing trash
	while((my $l = pop @cfg)) {
	    last if($l =~ /^Cryptochecksum:/);
	}
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

    return unless(defined($conn) && $self->{'protocol'} eq "telnet");

    $conn->cmd("exit");
}

sub disconnect($$)
{
    my ($self, $conn) = @_;

    return unless(defined($conn) && $self->{'protocol'} eq "telnet");

    $conn->close;
}

1;
