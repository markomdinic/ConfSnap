#
# api::util::fifo.pm
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

package api::util::fifo;

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
#  initialize FIFO queue object.
#
#   Input:	1. class name (passed implicitly)
#
#   Output:	1. FIFO queue object reference
#
sub new($$)
{
    my $class = shift;

    my $self = {
	'fifo'	=> api::util::list->new(),
	'index'	=> {}
    };
    return bless($self, $class);
}
#
# Put object into the FIFO queue.
#
#  This function puts any sort of data into the queue.
#  The 'object' in the queue must be unique, that is,
#  it can be put into the queue only once.
#
#   Input:	1. self object reference (passed implicitly)
#		2. an 'object' to be inserted (any data type)
#
#   Output:	1. TRUE, if object was inserted
#		   FALSE, if inserting failed
#
sub put($$)
{
    my ($self, $obj) = @_;

    return 0 if(!defined($obj) ||
		defined($self->{'index'}{$obj}));

    # Create a new node
    my $node = api::util::list->node();

    # Store object into the list node
    $node->[OBJECT] = $obj;

    # Insert the node into the list
    $self->{'fifo'}->add_tail($node);

    # Maintain object-to-node mapping
    # for quick access to the node
    $self->{'index'}{$obj} = $node;

    return 1;
}
#
# Get the first object in the FIFO queue.
#
#  This function removes the node from the head of the list
#  and returns the object it contains.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. a reference to the first 'object'
#		   undef, if queue was empty
#
sub get($)
{
    my $self = shift;

    my $node = $self->{'fifo'}->get_head;
    return undef unless defined($node);

    my $obj = $node->[OBJECT];

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return $obj;
}
#
# Remove object from the FIFO queue.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the queue
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
    $self->{'fifo'}->rem_node($node);

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return 1;
}
#
# Test if object exists in the FIFO queue.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the queue
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
# Get the number of objects in the FIFO queue.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. number of objects in the queue
#
sub count($)
{
    my $self = shift;

    return scalar(keys %{$self->{'index'}});
}
#
# Remove all objects from the FIFO queue
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
