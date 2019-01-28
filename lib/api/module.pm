#
# api::module.pm
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

package api::module;

##########################################################################################

use strict;
use warnings;

##########################################################################################

#
# Module constructor
#
#  This function is called to create a new instance
#  of the module by bless()ing module's configuration
#  hash into object of module's class. Since this is
#  a 'pure virtual' class, this method is ment to be
#  used only to instantiate module classes that inherit
#  from this one.
#
#   Input:	1. class name (passed implicitly)
#		2. hash reference to module's configuration
#
#   Output:	1. module object reference
#
sub instantiate($$)
{
    my ($mod_class, $mod_confref) = @_;

    my $self = bless($mod_confref, $mod_class);

    return $self;
}
#
# Get API reference
#
#  This method retrieves and returns API base object reference
#
#   Input:	1. module instance objref (passed implicitly)
#
#   Output:	1. API base object reference
#
sub api($)
{
    my $API;

    eval {
	# Disable strict references locally
	no strict qw(refs);
	# Call the function inside the main context
	# to retrieve API base object reference
	$API = &{'main::__get_api'}();
    };

    return $API;
}
#
# Get device connect/login protocol
#
#  This is the base class implementation of the method
#  used to query module instances about their configured
#  connect protocol.
#
#  It SHOULD be implemented by all modules, but it MUST
#  be implemented by modules that support remote/indirect
#  connections via other devices.
#
#   Input:	1. module instance objref (passed implicitly)
#
#   Output:	1. protocol name string
#
sub protocol($)
{
    return '';
}
#
# Get device login username
#
#  This is the base class implementation of the method
#  used to query module instances about their configured
#  login username.
#
#  It SHOULD be implemented by all modules, but it MUST
#  be implemented by modules that support remote/indirect
#  connections via other devices.
#
#   Input:	1. module instance objref (passed implicitly)
#
#   Output:	1. username string
#                  undef, if username is not defined
#
sub username($)
{
    return undef;
}
#
# Get device login password
#
#  This is the base class implementation of the method
#  used to query module instances about their configured
#  login password.
#
#  It SHOULD be implemented by all modules, but it MUST
#  be implemented by modules that support remote/indirect
#  connections via other devices.
#
#   Input:	1. module instance objref (passed implicitly)
#
#   Output:	1. password string
#                  undef, if password is not defined
#
sub password($)
{
    return undef;
}
#
# Get device admin password
#
#  This is the base class implementation of the method
#  used to query module instances about their configured
#  admin password.
#
#  It SHOULD be implemented by all modules, but it MUST
#  be implemented by modules that support remote/indirect
#  connections via other devices.
#
#   Input:	1. module instance objref (passed implicitly)
#
#   Output:	1. admin password string
#                  undef, if admin password is not defined
#
sub admin_password($)
{
    return undef;
}
#
# Connect to a network device
#
#  This is the base class implementation of the method
#  used to connect to configured network devices.
#
#  It MUST be implemented by all device modules.
#
#   Input:	1. module instance objref (passed implicitly)
#		2. network device's hostname or IP address
#
#   Output:	1. connection handle (can be anything that
#		   is meaningful to the rest of module's
#		   code - actual data/object type depends
#		   on the implementation).
#                  undef, if connection failed
#
sub connect($$)
{
    my $self = shift;

    $self->api->logging('LOG_ERR', "Module %s doesn't implement mandatory method connect()", ref($self));

    return undef;
}
#
# Collect configuration from a network device
#
#  This is the base class implementation of the method
#  used to collect configuration from network devices.
#
#  It MUST be implemented by all device modules.
#
#   Input:	1. module instance objref (passed implicitly)
#		2. connection handle
#
#   Output:	1. device configuration as string
#                  undef, if failed
#
sub collect($$)
{
    my $self = shift;

    $self->api->logging('LOG_ERR', "Module %s doesn't implement mandatory method collect()", ref($self));

    return undef;
}
#
# Disconnect from a network device
#
#  This is the base class implementation of the method
#  used to disconnect from connected network devices.
#
#  It MUST be implemented by all device modules.
#
#   Input:	1. module instance objref (passed implicitly)
#		2. connection handle
#
#   Output:	none
#
sub disconnect($$)
{
    my $self = shift;

    $self->api->logging('LOG_ERR', "Module %s doesn't implement mandatory method disconnect()", ref($self));
}
#
# Default module destructor
#
sub DESTROY
{
}

1;
