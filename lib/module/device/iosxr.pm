#
# module::device::iosxr.pm
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

package module::device::iosxr;

##############################################################################################

use strict;
use warnings;

##############################################################################################

use Net::Telnet;
use Config::ContextSensitive qw(:macros);

##############################################################################################

use api::module;

##############################################################################################

our @ISA = qw(api::module);

##############################################################################################

our $CONF_TEMPLATE = SECTION(
    DIRECTIVE('timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'timeout' => '$VALUE' } }), DEFAULT '10')),
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

    $host = $self->{'host'} unless(defined($host) && $host ne "");

    # Create new telnet client
    my $conn = Net::Telnet->new(Timeout => $self->{'timeout'});
    # Telnet to Cisco IOS XR device
    $conn->open($host);

    return $conn;
}

sub prompt($$)
{
    my ($self, $conn) = @_;

    return $conn->prompt('/RP\/\d+\/RSP\d+\/CPU\d+:[^#]+#/');
}

sub auth($$)
{
    my ($self, $conn) = @_;

    # Get credentials
    my $user = $self->{'username'};
    return 0 unless(defined($user) && $user ne "");
    my $pass = $self->{'password'};
    return 0 unless(defined($pass) && $pass ne "");

    # Login to IOS XR device
    $conn->login($user, $pass);

    return 1;
}

sub collect($$)
{
    my ($self, $conn) = @_;

    # Disable pagination
    $conn->cmd("terminal length 0");
    # Collect running config
    my @cfg = $conn->cmd("show running-conf");
    # Skip leading trash
    while((my $l = shift @cfg)) {
	last if($l =~ /^[Bb]uilding configuration/);
    }
    # If filter regexp is defined ...
    if(defined($self->{'filter'}) && $self->{'filter'} ne "") {
	# ... remove all matching lines
	@cfg = grep(!/$self->{'filter'}/, @cfg);
    }
    # If we got config, return it as string.
    # Otherwise, return undef
    return (scalar(@cfg) > 0) ? join('', @cfg):undef;
}

sub remote($$$;$)
{
    my ($self, $conn, $host, $vrf) = @_;

    # Telnet to router on remote end
    $conn->put("telnet vrf ".$vrf." ".$host."\n");
}

sub end($$)
{
    my ($self, $conn) = @_;
    return unless defined($conn);

    $conn->cmd("exit");
}

sub disconnect($$)
{
    my ($self, $conn) = @_;
    return unless defined($conn);

    $conn->close;
}

1;
