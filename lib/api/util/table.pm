#
# api::util::table.pm
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

package api::util::table;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use api::util::list;

##########################################################################################

use constant {
    OBJECT	=>	2
};

##########################################################################################

#
# Module constructor
#
#  This function is called in order to create and
#  initialize table object.
#
#   Input:	1. class name (passed implicitly)
#
#   Output:	1. array object reference
#
sub new($$)
{
    my $class = shift;

    my $self = {
	'table'	  => api::util::list->new(),
	'index'  => {}
    };
    return bless($self, $class);
}
#
# Add an object to a table.
#
#  This function puts any sort of data (an 'object') into
#  a table. The 'object' must be unique, that is, it can
#  be put into a table only once.
#
#   Input:	1. self object reference (passed implicitly)
#		2. an 'object' to be inserted (any data type)
#
#   Output:	1. TRUE, if object was inserted
#		   FALSE, if inserting failed
#
sub add($$)
{
    my ($self, $obj) = @_;

    return 0 if(!defined($obj) ||
		defined($self->{'index'}{$obj}));

    # Create a new node
    my $node = api::util::list->node();

    # Store object into the list node
    $node->[OBJECT] = $obj;

    # Insert the node into the list
    $self->{'table'}->add_tail($node);

    # Maintain object-to-node mapping
    # for quick access to the node
    $self->{'index'}{$obj} = $node;

    return 1;
}
#
# Remove object from a table.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the table
#
#   Output:	1. TRUE, if object was removed
#		   FALSE, if object was not found
#
sub remove($$)
{
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return 0 unless defined($node);

    # Remove node from its current position
    $self->{'table'}->rem_node($node);

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return 1;
}
#
# Insert an object into a table before another object.
#
#  This function puts any sort of data (an 'object') into
#  a table before another (reference) object. The 'object'
#  must be  unique, that is, it can be put into a table
#  only once.
#
#   Input:	1. self object reference (passed implicitly)
#		2. an 'object' to be inserted (any data type)
#		2. reference 'object' (NOT object reference)
#
#   Output:	1. TRUE, if object was inserted
#		   FALSE, if inserting failed
#
sub insert_before($$)
{
    my ($self, $obj, $ref) = @_;

    return 0 if(!defined($obj) || !defined($ref) ||
		defined($self->{'index'}{$obj}));

    # Create a new node
    my $node = api::util::list->node();

    # Store object into the list node
    $node->[OBJECT] = $obj;

    # Find the referent object's node
    my $after = $self->{'index'}{$ref};
    # Insert the node into the list
    $self->{'table'}->insert_before($node, $after);

    # Maintain object-to-node mapping
    # for quick access to the node
    $self->{'index'}{$obj} = $node;

    return 1;
}
#
# Insert an object into a table after another object.
#
#  This function puts any sort of data (an 'object') into
#  a table after another (reference) object. The 'object'
#  must be  unique, that is, it can be put into a table
#  only once.
#
#   Input:	1. self object reference (passed implicitly)
#		2. an 'object' to be inserted (any data type)
#		2. reference 'object' (NOT object reference)
#
#   Output:	1. TRUE, if object was inserted
#		   FALSE, if inserting failed
#
sub insert_after($$)
{
    my ($self, $ref, $obj) = @_;

    return 0 if(!defined($obj) || !defined($ref) ||
		defined($self->{'index'}{$obj}));

    # Create a new node
    my $node = api::util::list->node();

    # Store object into the list node
    $node->[OBJECT] = $obj;

    # Find the referent object's node
    my $before = $self->{'index'}{$ref};
    # Insert the node into the list
    $self->{'table'}->insert_after($before, $node);

    # Maintain object-to-node mapping
    # for quick access to the node
    $self->{'index'}{$obj} = $node;

    return 1;
}
#
# Get the first object in the table.
#
#  This function gets the first node in the list
#  and returns the object it contains.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. a reference to the first 'object'
#		   undef, if table was empty
#
sub first($)
{
    my $self = shift;

    my $node = $self->{'table'}->head;
    return undef unless defined($node);

    return $node->[OBJECT];
}
#
# Get the last object in the table.
#
#  This function gets the last node in the list
#  and returns the object it contains.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. a reference to the last 'object'
#		   undef, if table was empty
#
sub last($)
{
    my $self = shift;

    my $node = $self->{'table'}->tail;
    return undef unless defined($node);

    return $node->[OBJECT];
}
#
# Get the next object in table
#
#  This function returns the next object in the table
#  relative to the given object.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the table
#
#   Output:	1. a reference to the next 'object'
#		   undef, if 'object' was the last one
#
sub next($$)
{
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return undef unless defined($node);

    # Get node's successor
    my $next = $node->next;
    return undef unless defined($next);

    return $next->[OBJECT];
}
#
# Get the previous object in table
#
#  This function returns the previous object in the table
#  relative to the given object.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the table
#
#   Output:	1. a reference to the previous 'object'
#		   undef, if 'object' was the first one
#
sub prev($$)
{
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return undef unless defined($node);

    # Get node's predecessor
    my $prev = $node->prev;
    return undef unless defined($prev);

    return $prev->[OBJECT];
}
#
# Test if object exists in the table.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object'
#
#   Output:	1. TRUE, if object was found
#		   FALSE, if object was not found
#
sub exists($$)
{
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return defined($node) ? 1:0;
}
#
# Get the number of objects in the table.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. number of objects in the table
#
sub count($)
{
    my $self = shift;

    return scalar(keys %{$self->{'index'}});
}
#
# Remove all objects from the table
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	nothing
#
sub flush($)
{
    my $self = shift;

    foreach my $obj (keys %{$self->{'index'}}) {
	$self->remove($obj);
    }
}

1;
