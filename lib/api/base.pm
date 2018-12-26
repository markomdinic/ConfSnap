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

use Sys::Syslog;

##########################################################################################

use api::component::logger;
use api::component::storage;
use api::component::email;
use api::component::vcs;

##########################################################################################

our @ISA = qw(
    api::component::logger
    api::component::storage
    api::component::email
    api::component::vcs
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

    # Invoke base components' constructors
    my $logger = api::component::logger->new($conf);
    unless(defined($logger)) {
	print STDERR "Failed to initialize logger\n";
	return undef;
    }
    my $storage = api::component::storage->new($conf);
    unless(defined($storage)) {
	print STDERR "Failed to initialize local data storage\n";
	return undef;
    }
    my $email = api::component::email->new($conf);
    unless(defined($email)) {
	print STDERR "Failed to initialize mailer.\n";
	return undef;
    }
    my $vcs = api::component::vcs->new($conf);
    unless(defined($vcs)) {
	print STDERR "Failed to initialize VCS repository. Perhaps datadir is missing.\n";
	return undef;
    }


    # Initialize base API object
    my $self = bless({
	%{$logger},
	%{$storage},
	%{$email},
	%{$vcs},
	'conf' => $conf,
	'devs' => $devs
    }, $class);

    openlog($self->get_progname(), "nodelay,pid", $conf->{'syslog_facility'});

    # Initialize repository
    $self->init_repository()
	or return undef;

    return $self;
}
#
# Safely load a Perl module
#
#  This method loads a Perl module safely, without
#  killing the process in case of failure.
#
#  Input:	1. self object reference
#		2. module class name
#		3+ (optional) module tags
#
#  Output:	TRUE, if succeeded,
#		FALSE, if failed
#
sub load_module($$;@)
{
    my $self = shift;
    my $classname = shift;

    # Load module
    eval 'require '.$classname.';';

    if(defined($@) && $@ ne '') {
	my $errmsg = "Failed to load module ".$classname;
	if(ref($self) ne '') {
	    $self->logging('LOG_ERR', $errmsg);
	} else {
	    print STDERR "[ERR] ".$errmsg."\n";
	}
	return 0;
    }

    if(scalar(@_) > 0) {
	$classname->export_to_level(1, undef, @_);
    }

    return 1;
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

    # Format path relative to the base repository dir
    return (defined($vrf) && $vrf ne "") ?
		    "vrfs/".$vrf."/".$device_name."/config":
		    "devices/".$device_name."/config";
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
# Send change report via email
#
#   Input:	1. self object reference
#		2. report content
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub report_config_changes($$)
{
    my ($self, $report) = @_;

    # Don't waste time on empty reports
    return 0 unless(defined($report) && $report ne '');

    # Get the list of notification recipients
    my $recipients = $self->{'conf'}->{'report_recipients'};

    # At least one recipient must be defined
    return 0 unless(defined($recipients) &&
		    defined($recipients->[0]) && 
		    $recipients->[0] ne '');

    # Include the list of changed files in report ?
    if($self->{'conf'}->{'report_files'}) {
	# Get the list of changed files
	my @files = $self->changed_files();
	if(@files) {
	    # Add the list to the report
	    $report .= "\nFiles that have changed:\n\n".join("\n", @files)."\n";
	}
    }

    # Include change details in report ?
    if($self->{'conf'}->{'report_diffs'}) {
	# Get changes since previous commit
	my @diff = $self->diff('.', 'HEAD^1', 'HEAD');
	if(@diff) {
	    # Add diffs to the report
	    $report .= "\nChanges that have occured:\n\n".join("\n", @diff)."\n";
	}
    }

    # Send report email
    return $self->send_email($self->{'conf'}->{'my_name'},
			     $self->{'conf'}->{'my_email'},
			     $recipients,
			     '[ConfSnap] Device configuration changes',
			     $report);
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
