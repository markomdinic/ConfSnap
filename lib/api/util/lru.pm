#
# api::util::lru.pm
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

package api::util::lru;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use Time::HiRes qw(time);

##########################################################################################

use api::util::list;

##########################################################################################

use constant {
    LIFE	=>	2,
    EXPIRE	=>	3,
    OBJECT	=>	4
};

##########################################################################################

#
# Module constructor
#
#  This function is called in order to create and
#  initialize LRU cache object.
#
#   Input:	1. class name (passed implicitly)
#		2. default life time of objects (in secs)
#
#   Output:	1. LRU object reference
#
sub new($$) {
    my ($class, $life) = @_;

    return undef unless defined($life) &&
			$life > 0;

    my $self = {
	'lru'	=> api::util::list->new(),
	'index'	=> {},
	'life'	=> $life
    };
    return bless($self, $class);
}
#
# Put object into the cache.
#
#  This function puts any sort of data into the LRU cache.
#  The 'object' in the cache must be unique, that is, 
#  it can be put into the cache only once. If lifetime
#  wasn't specified, default LRU lifetime will be used.
#
#   Input:	1. self object reference (passed implicitly)
#		2. an 'object' to be inserted (any data type)
#		3. (optional) life time of object
#
#   Output:	1. TRUE, if object was inserted
#		   FALSE, if inserting failed
#
sub insert($$;$) {
    my ($self, $obj, $life) = @_;

    return 0 if(!defined($obj) ||
		defined($self->{'index'}{$obj}));

    # Create a new node
    my $node = api::util::list->node();

    # Life time of the object
    my $lifetime = defined($life) ?
			$life:$self->{'life'};
    # Unix timestamp when this object expires
    my $expire = time() + $lifetime;

    # We have to find a spot in the list
    # where to insert our object.
    my $anchor;
    # Search backwards for a spot
    for($anchor = $self->{'lru'}->tail;
	defined($anchor) && $anchor->[EXPIRE] > $expire;
	$anchor = $anchor->prev) {}

    # Insert the node into the list
    $self->{'lru'}->insert_after($anchor, $node);

    # Set node's expiration timestamp
    $node->[LIFE] = $lifetime;
    # Set node's expiration timestamp
    $node->[EXPIRE] = $expire;
    # Store object into the list node
    $node->[OBJECT] = $obj;

    # Maintain object-to-node mapping
    # for quick access to the node
    $self->{'index'}{$obj} = $node;

    return 1;
}
#
# Remove object from the cache.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the cache
#
#   Output:	1. TRUE, if object was removed
#		   FALSE, if object was not found
#
sub remove($$) {
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return 0 unless defined($node);

    # Remove node from its current position
    $self->{'lru'}->rem_node($node);

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return 1;
}
#
# Refresh object in the cache.
#
#  This function resets the expiration timer of the object
#  by assigning it new expiration timestamp and requeueing it.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the cache
#
#   Output:	1. TRUE, if object was refreshed
#		   FALSE, if object was not found
#
sub refresh($$) {
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the node
    my $node = $self->{'index'}{$obj};
    return 0 unless defined($node);

    # Get object's life time
    my $life = $node->[LIFE];

    # Remove the object from the cache
    $self->remove($obj);

    # Requeue the object
    $self->insert($obj, $life);

    return 1;
}
#
# Get object's remaining time.
#
#  This function returns the remaining time for the object.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object' in the cache
#
#   Output:	1. seconds until the 'object' expires
#		   undef, if object doesn't exist
#
sub due($$) {
    my ($self, $obj) = @_;

    return undef unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return defined($node) ?
		($node->[EXPIRE] - time()):undef;
}
#
# Get the oldest object's remaining time.
#
#  This function returns the remaining time for the object
#  at the head of the list (the oldest object in the cache).
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. seconds until the oldest 'object' expires
#		   undef, if cache is empty
#
sub firstdue($) {
    my $self = shift;

    my $node = $self->{'lru'}->head;
    return defined($node) ?
		($node->[EXPIRE] - time()):undef;
}
#
# Get the oldest object in the cache.
#
#  This function removes the node from the head of the list
#  regardless of its status and returns the object it contains.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. a reference to the oldest 'object'
#		   undef, if no (more) expired objects
#
sub first($) {
    my $self = shift;

    my $node = $self->{'lru'}->get_head;
    return undef unless defined($node);

    my $obj = $node->[OBJECT];

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return $obj;
}
#
# Get the oldest expired object in the cache.
#
#  This function removes the node from the head of the list
#  if it has expired and returns the object it contains.
#  By calling this method in a loop, one can flush all
#  objects that have expired from the cache.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. a reference to the expired 'object'
#		   undef, if no (more) expired objects
#
sub expired($) {
    my $self = shift;

    my $node = $self->{'lru'}->head;
    return undef unless defined($node) &&
			$node->[EXPIRE] <= time();

    my $obj = $node->[OBJECT];

    # Remove the node from the head of the list
    $self->{'lru'}->rem_node($node);

    # Remove object-to-node mapping
    delete $self->{'index'}{$obj};

    # Destroy node structure
    undef @{$node};

    return $obj;
}
#
# Test if object exists in the cache.
#
#   Input:	1. self object reference (passed implicitly)
#		2. a reference to an 'object'
#
#   Output:	1. TRUE, if object was found
#		   FALSE, if object was not found
#
sub exists($$) {
    my ($self, $obj) = @_;

    return 0 unless defined($obj);

    # Find the object's node
    my $node = $self->{'index'}{$obj};
    return defined($node) ? 1:0;
}
#
# Get the number of objects in the cache.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. number of objects in the cache
#
sub count($) {
    my $self = shift;

    return scalar(keys %{$self->{'index'}});
}
#
# Remove all objects from the cache
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. number of objects removed
#
sub flush($) {
    my $self = shift;
    my $count = 0;

    # Remove all objects from the cache
    while($self->first()) {
	$count++;
    }

    return $count;
}

1;
