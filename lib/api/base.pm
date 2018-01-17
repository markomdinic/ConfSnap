#
# api::base.pm
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

package api::base;

##########################################################################################

use strict;
use warnings;

##########################################################################################

#use Socket;
#use IO::Handle;
use Sys::Syslog;
#use POSIX qw(:signal_h :sys_wait_h);

##########################################################################################

use api::base::logger;
use api::base::storage;
use api::base::vcs;

##########################################################################################

our @ISA = qw(
    api::base::logger
    api::base::storage
    api::base::vcs
);

##########################################################################################

our $AUTOLOAD;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. hashref to the global configuration
#		3. hashref to the devices' parameters
#
#   Output:	1. api::base object reference
#
sub new($$$)
{
    my ($class, $conf, $devs) = @_;

    # Hashref to the global configuration is mandatory
    return undef unless(defined($conf) && ref($conf) eq "HASH");

    # Invoke parent constructors
    my $logger = api::base::logger->new($conf);
    unless(defined($logger)) {
	print STDERR "Failed to initialize logger\n";
	return undef;
    }
    my $storage = api::base::storage->new($conf);
    unless(defined($storage)) {
	print STDERR "Failed to initialize local data storage\n";
	return undef;
    }
    my $vcs = api::base::vcs->new($conf);
    unless(defined($vcs)) {
	print STDERR "Failed to initialize VCS repository. Perhaps datadir is missing.\n";
	return undef;
    }

    # Initialize base API object
    my $self = bless({
	%{$logger},
	%{$storage},
	%{$vcs},
	'conf' => $conf,
	'devs' => $devs
    }, $class);

    openlog($self->get_progname(), "nodelay,pid", $conf->{'syslog_facility'});

    return $self;
}
#
# Format path to the device configuration
#
#  This method produces path to the given device's
#  configuration, relative to the repository base dir.
#  This relative path depends on whether the device is
#  a infrastructure device or a CPE inside a VRF.
#
#   Input:	1. self object reference
#		2. device name
#
#   Output:	1. path to the device's config file,
#		   undef, if failed
#
sub device_config_path($$)
{
    my ($self, $device_name) = @_;

    return undef unless(defined($device_name) && 
			$device_name ne "" &&
			defined($self->{'devs'}->{$device_name}));

    my $vrf = $self->{'devs'}->{$device_name}->{'vrf'};
    my $host = $self->{'devs'}->{$device_name}->{'host'};

    # Format path relative to the base repository dir
    return (defined($vrf) && $vrf ne "") ?
		    "vrfs/".$vrf."/".$device_name."/".$host.".config":
		    "devices/".$device_name."/".$host.".config";
}
#
# Store device configuration
#
#   Input:	1. self object reference
#		2. device name
#		3. device configuration
#
#   Output:	1. TRUE, if succeeded,
#		   FALSE, if failed
#
sub store_device_config($$$)
{
    my ($self, $device_name, $config) = @_;

    return 0 unless(defined($config) && $config ne "");

    # Format path relative to the base repository dir
    my $path = $self->device_config_path($device_name);
    return 0 unless(defined($path) && $path ne "");

    # Store device configuration into VCS repository
    return $self->store($path, $config);
}
#
# Retrieve device configuration
#
#   Input:	1. self object reference
#		2. device name
#
#   Output:	1. device configuration, if succeeded,
#		   undef, if failed
#
sub retrieve_device_config($$$)
{
    my ($self, $device_name) = @_;

    # Format path relative to the base repository dir
    my $path = $self->device_config_path($device_name);
    return 0 unless(defined($path) && $path ne "");

    # Retrieve device configuration from VCS repository
    return $self->retrieve($path);
}
#
# Proxy function calls to the main program.
#
#  Purpose of this function is to provide modules 
#  the clean way of accessing functions defined
#  inside the main code. After creating an instance
#  of this module, any $api->function(...) call
#  that doesn't find a function in this module
#  will be proxied to &main::function(...).
#
sub AUTOLOAD
{
    my $self = shift;
    my $res;

    # Unqualify function name
    my ($func_name) = ($AUTOLOAD =~ /^(?:.*::)?([^:]+)/);
    unless(defined($func_name) && $func_name ne "") {
	$self->logging('LOG_ERR', "Invalid function %s called by %s",
				  $func_name,
				  caller());
	return undef;
    }

    # Wrap the call inside eval to gracefully
    # catch errorneous function calls.
    eval {
	# Disable strict references locally
	no strict qw(refs);
	# Call the function inside the main context
	$res = &{'main::__'.$func_name}(@_);
    };

    if($@) {
	$self->logging('LOG_ERR', "Function %s called by %s is not defined by SnapConf API",
				  $func_name,
				  caller());
	return undef;
    }

    return $res;
}
#
# Module destructor
#
#  This function is automatically called
#  when module is unloaded.
#
sub DESTROY
{
    closelog();
}

1;
