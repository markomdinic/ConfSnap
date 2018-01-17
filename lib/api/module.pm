#
# api::module.pm
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
# Default module destructor
#
sub DESTROY
{
}

1;
