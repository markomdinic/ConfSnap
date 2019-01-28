#
# api::util::list.pm
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

package api::util::list;

##########################################################################################

use strict;
use warnings;

##########################################################################################

# List constants
use constant {
    HEAD				=> 0,
    TAIL				=> 1,

    PREV				=> 0,
    NEXT				=> 1
};

##########################################################################################

#
# List constructor
#
#  This function creates a new list (which is, by its
#  nature - a simple 2-element array) and returns
#  a blessed reference to it.
#
#   Input:	1. class name (passed implicitly)
#
#   Output:	1. list object reference
#
sub new($)
{
    my $class = shift;

    my $list = [undef, undef];
    return bless($list, $class);
}
#
# Return the node at the head of the list
#
#   Input:	1. list object reference (passed implicitly)
#
#   Output:	1. head node object reference
#
#
sub head($)
{
    my $list = shift;
    return $list->[HEAD];
}
#
# Return the node at the tail of the list
#
#   Input:	1. list object reference (passed implicitly)
#
#   Output:	1. tail node object reference
#
#
sub tail($)
{
    my $list = shift;
    return $list->[TAIL];
}
#
# Add a node to the head of the list.
#
#   Input:	1. list object reference (passed implicitly)
#		2. a reference to the node to be added
#
#   Output:	none
#
sub add_head($$)
{
    my ($list, $node) =@_;

    return unless defined($list) &&
		  defined($node);

    $node->[PREV] = undef;
    $node->[NEXT] = $list->[HEAD];

    if(defined($list->[HEAD])) {
	$list->[HEAD][PREV] = $node;
    }

    unless(defined($list->[TAIL])) {
	$list->[TAIL] = $node;
    }

    $list->[HEAD] = $node;
}
#
# Add a node to the tail of the list.
#
#   Input:	1. list object reference (passed implicitly)
#		2. a reference to the node to be added
#
#   Output:	none
#
sub add_tail($$)
{
    my ($list, $node) =@_;

    return unless defined($list) &&
		  defined($node);

    $node->[NEXT] = undef;
    $node->[PREV] = $list->[TAIL];

    if(defined($list->[TAIL])) {
	$list->[TAIL][NEXT] = $node;
    }

    unless(defined($list->[HEAD])) {
	$list->[HEAD] = $node;
    }

    $list->[TAIL] = $node;
}
#
# Remove and return a node from the head of the list.
#
#   Input:	1. list object reference (passed implicitly)
#
#   Output:	1. node object reference
#
sub get_head($)
{
    my $list = shift;

    return undef unless defined($list);

    my $node = $list->[HEAD];
    return undef unless defined($node);

    $list->[HEAD] = $node->[NEXT];

    if(defined($list->[HEAD])) {
	$list->[HEAD][PREV] = undef;
    } else {
	$list->[TAIL] = undef;
    }

    # Remove refcnt to other objects to allow
    # garbage collector to release memory
    $node->[NEXT] = undef;
    $node->[PREV] = undef;

    return $node;
}
#
# Remove and return a node from the tail of the list.
#
#   Input:	1. list object reference (passed implicitly)
#
#   Output:	1. node object reference
#
sub get_tail($)
{
    my $list = shift;

    return undef unless defined($list);

    my $node = $list->[TAIL];
    return undef unless defined($node);

    $list->[TAIL] = $node->[PREV];

    if(defined($list->[TAIL])) {
	$list->[TAIL][NEXT] = undef;
    } else {
	$list->[HEAD] = undef;
    }

    # Remove refcnt to other objects to allow
    # garbage collector to release memory
    $node->[NEXT] = undef;
    $node->[PREV] = undef;

    return $node;
}
#
# Remove a node from anywhere in the list.
#
#   Input:	1. list object reference (passed implicitly)
#		2. node object reference
#
#   Output:	none
#
sub rem_node($$)
{
    my ($list, $node) = @_;

    return unless defined($list) &&
		  defined($node);

    # Node has another node before it ?
    if(defined($node->[PREV])) {
	$node->[PREV][NEXT] = $node->[NEXT];
    # Otherwise, its at the head of the list.
    # We must check if list head actually
    # points to the node.
    } elsif(defined($list->[HEAD]) &&
	    $list->[HEAD] == $node) {
	$list->[HEAD] = $node->[NEXT];
    }

    # Node has another node after it ?
    if(defined($node->[NEXT])) {
	$node->[NEXT][PREV] = $node->[PREV];
    # Otherwise, its at the tail of the list.
    # We must check if list tail actually
    # points to the node.
    } elsif(defined($list->[TAIL]) &&
	    $list->[TAIL] == $node) {
	$list->[TAIL] = $node->[PREV];
    }

    # Remove refcnt to other objects to allow
    # garbage collector to release memory
    $node->[NEXT] = undef;
    $node->[PREV] = undef;
}
#
# Insert a node immediately before the specified anchor node
#
#   Input:	1. list object reference (passed implicitly)
#		2. a reference to the node to be
#		   added before the anchor node
#		3. a reference to the anchor node
#
#   Output:	none
#
sub insert_before($$$)
{
    my ($list, $node, $after) = @_;

    return unless defined($list) &&
		  defined($node);

    $node->[NEXT] = $after;

    if(defined($after)) {

	$node->[PREV] = $after->[PREV];

	if(defined($after->[PREV])) {
	    $after->[PREV][NEXT] = $node;
	} else {
	    $list->[HEAD] = $node;
	}
	$after->[PREV] = $node;

    } else {

	$node->[PREV] = $list->[TAIL];

	if(defined($list->[TAIL])) {
	    $list->[TAIL][NEXT] = $node;
	} else {
            $list->[HEAD] = $node;
	}

	$list->[TAIL] = $node;

    }
}
#
# Insert a node immediately after the specified anchor node
#
#   Input:	1. list object reference (passed implicitly)
#		2. a reference to the anchor node
#		3. a reference to the node to be
#		   added after the anchor node
#
#   Output:	none
#
sub insert_after($$$)
{
    my ($list, $before, $node) = @_;

    return unless defined($list) &&
		  defined($node);

    $node->[PREV] = $before;

    if(defined($before)) {

	$node->[NEXT] = $before->[NEXT];

	if(defined($before->[NEXT])) {
	    $before->[NEXT][PREV] = $node;
	} else {
	    $list->[TAIL] = $node;
	}
	$before->[NEXT] = $node;

    } else {

	$node->[NEXT] = $list->[HEAD];

	if(defined($list->[HEAD])) {
	    $list->[HEAD][PREV] = $node;
	} else {
            $list->[TAIL] = $node;
	}

	$list->[HEAD] = $node;

    }
}
#
# Node constructor
#
#  This function creates a new node (which is, by its
#  nature - a simple 2-element array) and returns 
#  a blessed reference to it.
#
#   Input:	1. class name (passed implicitly)
#
#   Output:	1. node object reference
#
sub node($)
{
    my $class = shift;

    my $node = [undef, undef];
    return bless($node, $class);
}
#
# Return next node in the list
#
#   Input:	1. node object reference (passed implicitly)
#
#   Output:	1. next node object reference
#
#
sub next($)
{
    my $node = shift;
    return $node->[NEXT];
}
#
# Return previous node in the list
#
#   Input:	1. node object reference (passed implicitly)
#
#   Output:	1. previous node object reference
#
#
sub prev($)
{
    my $node = shift;
    return $node->[PREV];
}
#
# Remove all objects from the list
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	nothing
#
sub flush($)
{
    my $self = shift;

    while($self->get_head) {};
}

1;
