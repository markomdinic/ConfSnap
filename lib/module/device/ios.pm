#
# module::device::ios.pm
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
    DIRECTIVE('timeout', ARG(CF_INTEGER|CF_POSITIVE, STORE(TO 'DEVICE', KEY { '$SECTION' => { 'timeout' => '$VALUE' } }), DEFAULT '10')),
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

sub protocol($)
{
    return 'telnet';
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

    $host = $self->{'host'} unless(defined($host) && $host ne "");

    # Safely load Net::Telnet on demand
    $self->api->load_module('Net::Telnet')
	or return undef;
    # Create new telnet client
    my $conn = Net::Telnet->new(Timeout => $self->{'timeout'});
    # Telnet to Cisco IOS device
    $conn->open($host);

    return $conn;
}

sub prompt($$)
{
    my ($self, $conn) = @_;

    return $conn->prompt('/[a-zA-Z0-9\_\-\+]+[>#]$/');
}

sub auth($$)
{
    my ($self, $conn) = @_;

    # At least password must be defined
    my $pass = $self->password;
    return 0 unless defined($pass);

    # Get username
    my $user = $self->username;
    # Send username, if defined
    if(defined($user) && $user ne '') {
	$conn->waitfor('/[Uu]sername:/');
	$conn->put($user."\n");
    }

    # Send password
    $conn->waitfor('/[Pp]assword:/');
    $conn->cmd($pass);

    # Get enable password
    my $enable = $self->admin_password;
    # Change to enable mode, if defined
    if(defined($enable) && $enable ne '') {
	$conn->cmd("enable\n".$enable);
    }

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
	last if($l =~ /^[Cc]urrent\s+[Cc]onfiguration/);
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
    my ($self, $remote, $conn, $host, $vrf) = @_;

    my $proto = $remote->protocol;

    unless($proto eq 'telnet') {
	$self->api->logging('LOG_ERR', "Protocol %s is not supported by %s for indirect device access", $proto, ref($self));
	return 0;
    }

    # Telnet to the device on the remote end
    $conn->put("telnet ".$host.((defined($vrf) && $vrf ne "") ? " /vrf ".$vrf:"")."\n");

    return 1;
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
