#
# api::util::event.pm
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

package api::util::event;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use Fcntl;
use IO::Poll;
use Time::HiRes qw(time alarm sleep);
use POSIX qw(:signal_h :sys_wait_h);

##########################################################################################

use api::util::table;
use api::util::lru;

##########################################################################################

use constant {
    EV_STOPPED		=> 0,
    EV_SHUTDOWN		=> 1,
    EV_RUNNING		=> 2
};

##########################################################################################

#
# Module constructor
#
#  This function is called in order to create
#  API module instance.
#
#   Input:	1. class name (passed implicitly)
#
#   Output:	1. api::util::event object reference
#
sub new($)
{
    my $class = shift;

    my $read_poll = IO::Poll->new();
    return undef unless defined($read_poll);

    my $write_poll = IO::Poll->new();
    return undef unless defined($write_poll);

    my $timer = api::util::lru->new(1);
    return undef unless defined($timer);

    my $expire = api::util::lru->new(1);
    return undef unless defined($expire);

    my $delay = api::util::lru->new(1);
    return undef unless defined($delay);

    my $recur = api::util::table->new();
    return undef unless defined($recur);

    # Poll must be monitoring at least one handle,
    # so we register something that shouldn't happen
    # unless we do it ourselves - STDOUT close
    $read_poll->mask(STDOUT => POLLHUP);

    my $self = {
	'epoch' => time(),
	'rpoll' => $read_poll,
	'wpoll' => $write_poll,
	'timer' => $timer,
	'expire' => $expire,
	'delay' => $delay,
	'recur' => $recur,
	'read' => {},
	'write' => {},
	'state' => EV_RUNNING
    };

    return bless($self, $class);
}
#
# Set the state of event engine to running
#
#  This method changes the state of the event engine
#  to running. Event engine is created in this state,
#  so calling this method explicitly is not required.
#  However, event processing can be explicitly stopped,
#  so this method can be used to resume processing.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. previous state
#
sub start($)
{
    my $self = shift;

    # Get previous state
    my $state = $self->{'state'};
    # Set state to running
    $self->{'state'} = EV_RUNNING;
    # Return previous state
    return $state;
}
#
# Set the state of event engine to shutdown
#
#  This method changes the state of the event engine
#  to shutdown. While in this state, the event engine
#  is still processing events, with pending change to
#  stopped state. Shutdown state can be used to signal
#  program to cleanup and quit, while keeping the event
#  engine still running.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. previous state
#
sub shutdown($)
{
    my $self = shift;

    # Get previous state
    my $state = $self->{'state'};
    # Set state to shutdown
    $self->{'state'} = EV_SHUTDOWN;
    # Return previous state
    return $state;
}
#
# Set the state of event engine to stopped
#
#  This method changes the state of the event engine
#  to stopped. As a result, no event processing will
#  occur while in this state. Processing can resume
#  as soon as state is changed to running.
#
#  Also, this method can be used to terminate the code
#  utilizing the event engine if it relies on event
#  engine's state.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. previous state
#
sub stop($)
{
    my $self = shift;

    # Get previous state
    my $state = $self->{'state'};
    # Set state to stopped
    $self->{'state'} = EV_STOPPED;
    # Return previous state
    return $state;
}
#
# Determine if event engine is in running state
#
#  This method returns the state of event engine. Engine is
#  in running state only if status is set EV_RUNNING.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. TRUE, if event engine is running
#		   FALSE, if event engine is in other state
#
sub running($)
{
    my $self = shift;

    return ($self->{'state'} == EV_RUNNING) ? 1:0;
}
#
# Determine if event engine is in shutdown state
#
#  This method returns the state of event engine. Engine is
#  in terminating state only if status is set to EV_SHUTDOWN.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. TRUE, if event engine is shutting down
#		   FALSE, if event engine is in other state
#
sub shutting_down($)
{
    my $self = shift;

    return ($self->{'state'} == EV_SHUTDOWN) ? 1:0;
}
#
# Determine if event engine is in stopped state
#
#  This method returns the state of event engine. Engine is
#  in stopped state only if status is set to EV_STOPPED.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. TRUE, if event engine is stopped
#		   FALSE, if event engine is in other state
#
sub stopped($)
{
    my $self = shift;

    return ($self->{'state'} == EV_STOPPED) ? 1:0;
}
#
# Register termination event
#
#  This method registers event triggered by SIGTERM or SIGINT
#  signals. Upon receiving these signals, event handler will
#  be called, typically to perform cleanup,
#
#  Note that this is NOT the same as specifying a signal handler
#  because upon receiving a signal, event handler will not be
#  invoked immediately, but queued along with other handlers.
#  Thus, there is no way of knowing the exact moment it will
#  get to run.
#
#  Graceful termination allows pending events to be handled
#  prior to invoking our event handler. This is the default.
#
#  Otherwise, termination event handler will be inserted before
#  all others which will effectively prevent them from being
#  handled, if it changes event engine's status to stipped.
#
#  Event can be bound by any or all of the following:
#   - amount of time elapsed since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments (optional)
#		  'graceful' => boolean to indicate graceful termination (optional)
#		  'delay' => start up (first trigger) delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'on_expiry' => event expiration handler callback (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. termination event hashref, if successful
#		   undef, if failed
#
sub create_termination_event($%)
{
    my $self = shift;
    my $param = {@_};

    # Event handler callback must be coderef
    return undef unless(defined($param->{'handler'}) &&
			ref($param->{'handler'}) eq 'CODE');

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'termination',
	'handler'	=> $param->{'handler'},
	'graceful'	=> defined($param->{'graceful'}) ?
				$param->{'graceful'}:1,
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'delay'		=> (defined($param->{'delay'}) && $param->{'delay'} > 0) ?
				$param->{'delay'}:undef,
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > time()) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef
    );

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - event hashref
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'		=> $event->{'handler'},
				     'args'		=> [ $event, @{$event->{'args'}}, @_ ],
				     'attempts'		=> $event->{'attempts'},
				     'timeout'		=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Turn absolute expiration timestamp into
    # interval relative to this very moment.
    my $until_interval = defined($event->{'expire_at'}) ?
				($event->{'expire_at'} - time()):undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    my $expiry_interval = defined($event->{'expire_in'}) ?
			    ((defined($until_interval) && $until_interval < $event->{'expire_in'}) ?
				$until_interval:$event->{'expire_in'}):$until_interval;

    # If any time limit is imposed ...
    if(defined($expiry_interval)) {
	# ... schedule event expiration
	$self->{'expire'}->insert($event, $expiry_interval);
    }

    # This will be our SIGTERM/SIGINT handler
    my $sighandler = sub {
	# Schedule event to be invoked either with
	# delay, if defined, or immediately
	$self->{'delay'}->insert($event, defined($event->{'delay'}) ? $event->{'delay'}:0);
    };

    # Set our own safe SIGTERM handler
    $self->{'_sigterm'} = POSIX::SigAction->new();
    my $new_term = POSIX::SigAction->new($sighandler, POSIX::SigSet->new(SIGTERM));
    $new_term->safe(1);
    sigaction(SIGTERM, $new_term, $self->{'_sigterm'});
    # Set our own safe SIGINT handler
    $self->{'_sigint'} = POSIX::SigAction->new();
    my $new_int = POSIX::SigAction->new($sighandler, POSIX::SigSet->new(SIGINT));
    $new_int->safe(1);
    sigaction(SIGINT, $new_int, $self->{'_sigint'});

    return $event;
}
#
# Register reload event
#
#  This method registers event triggered by SIGHUP signal. Upon
#  receiving this signal, event handler will be called, typically
#  to perform configuration reload. 
#
#  Note that this is NOT the same as specifying a signal handler
#  because upon receiving a signal, event handler will not be
#  invoked immediately, but queued along with other handlers.
#  Thus, there is no way of knowing the exact moment it will
#  get to run.
#
#  Graceful reload allows pending events to be handled prior to
#  invoking our event handler and, ultimately, changing their 
#  configuration. This is the default.
#
#  Otherwise, reload event handler will be inserted before all
#  others which will effectively reconfigure pending events
#  right before they run.
#
#  Event can be bound by any or all of the following:
#   - maximum number of repetitions (iterations),
#   - amount of time elapsed since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'handler' => coderef to the event handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'graceful' => boolean to indicate graceful reload (optional)
#		  'delay' => start up (first trigger) delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'limit' => maximum number of iterations (optional)
#		  'on_limit' => repetition limit handler callback (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'on_expiry' => event expiration handler callback (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. reload event hashref, if successful
#		   undef, if failed
#
sub create_reload_event($%)
{
    my $self = shift;
    my $param = {@_};

    # Event handler callback must be coderef
    return undef unless(defined($param->{'handler'}) &&
			ref($param->{'handler'}) eq 'CODE');

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'reload',
	'handler'	=> $param->{'handler'},
	'graceful'	=> defined($param->{'graceful'}) ?
				$param->{'graceful'}:1,
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'delay'		=> (defined($param->{'delay'}) && $param->{'delay'} > 0) ?
				$param->{'delay'}:undef,
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'limit'		=> (defined($param->{'limit'}) && int($param->{'limit'}) > 0) ?
				int($param->{'limit'}):undef,
	'on_limit'	=> (ref($param->{'on_limit'}) eq 'CODE') ?
				$param->{'on_limit'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > time()) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef
    );

    # Event limit callback wrapper
    if(defined($event->{'on_limit'})) {
	$event->{'_on_limit'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_limit'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - event hashref
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'	=> $event->{'handler'},
				     'args'	=> [ $event, @{$event->{'args'}}, @_ ],
				     'attempts'	=> $event->{'attempts'},
				     'timeout'	=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Turn absolute expiration timestamp into
    # interval relative to this very moment.
    my $until_interval = defined($event->{'expire_at'}) ?
				($event->{'expire_at'} - time()):undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    my $expiry_interval = defined($event->{'expire_in'}) ?
			    ((defined($until_interval) && $until_interval < $event->{'expire_in'}) ?
				$until_interval:$event->{'expire_in'}):$until_interval;

    # If any time limit is imposed ...
    if(defined($expiry_interval)) {
	# ... schedule event expiration
	$self->{'expire'}->insert($event, $expiry_interval);
    }

    # This will be our SIGHUP handler
    my $sighandler = sub {
	# Schedule event to be invoked either with
	# delay, if defined, or immediately
	$self->{'delay'}->insert($event, defined($event->{'delay'}) ? $event->{'delay'}:0);
    };

    # Set our own safe SIGHUP handler
    $self->{'_sighup'} = POSIX::SigAction->new();
    my $new_hup = POSIX::SigAction->new($sighandler, POSIX::SigSet->new(SIGHUP));
    $new_hup->safe(1);
    sigaction(SIGHUP, $new_hup, $self->{'_sighup'});

    return $event;
}
#
# Register reaping event
#
#  This method registers event triggered by SIGCHLD signal. Upon
#  receiving this signal, event handler will be called, typically
#  to perform cleanup after child process exit.
#
#  Note that this is NOT the same as specifying a signal handler
#  because upon receiving a signal, event handler will not be
#  invoked immediately, but queued along with other handlers.
#  Thus, there is no way of knowing the exact moment it will
#  get to run.
#
#  Reaping event handler will be inserted before all other
#  pending events to be processed as soon as possible.
#
#  Event can be bound by any or all of the following:
#   - maximum number of repetitions (iterations),
#   - amount of time elapsed since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'handler' => coderef to the event handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'delay' => start up (first trigger) delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'limit' => maximum number of iterations (optional)
#		  'on_limit' => repetition limit handler callback (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'on_expiry' => event expiration handler callback (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. reaping event hashref, if successful
#		   undef, if failed
#
sub create_reaping_event($%)
{
    my $self = shift;
    my $param = {@_};

    # Event handler callback must be coderef
    return undef unless(defined($param->{'handler'}) &&
			ref($param->{'handler'}) eq 'CODE');

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'reaping',
	'handler'	=> $param->{'handler'},
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'delay'		=> (defined($param->{'delay'}) && $param->{'delay'} > 0) ?
				$param->{'delay'}:undef,
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'limit'		=> (defined($param->{'limit'}) && int($param->{'limit'}) > 0) ?
				int($param->{'limit'}):undef,
	'on_limit'	=> (ref($param->{'on_limit'}) eq 'CODE') ?
				$param->{'on_limit'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > time()) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef,
	'_pids'		=> []
    );

    # Event limit callback wrapper
    if(defined($event->{'on_limit'})) {
	$event->{'_on_limit'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_limit'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - child process PID
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - event hashref
	#  - child process PID
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'		=> $event->{'handler'},
				     'args'		=> [ $event, @_ ],
				     'attempts'		=> $event->{'attempts'},
				     'timeout'		=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Turn absolute expiration timestamp into
    # interval relative to this very moment.
    my $until_interval = defined($event->{'expire_at'}) ?
				($event->{'expire_at'} - time()):undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    my $expiry_interval = defined($event->{'expire_in'}) ?
			    ((defined($until_interval) && $until_interval < $event->{'expire_in'}) ?
				$until_interval:$event->{'expire_in'}):$until_interval;

    # If any time limit is imposed ...
    if(defined($expiry_interval)) {
	# ... schedule event expiration
	$self->{'expire'}->insert($event, $expiry_interval);
    }

    # This will be our SIGCHLD handler
    my $sighandler = sub {
	# Reap all dead child processes
	while((my $pid = waitpid(-1, &WNOHANG)) > 0) {
	    # Make a list of all child processes
	    # that we need to run handler for
	    push @{$event->{'_pids'}}, $pid;
	}
	if(scalar(@{$event->{'_pids'}}) > 0) {
	    # Schedule event to be invoked either with
	    # delay, if defined, or immediately
	    $self->{'delay'}->insert($event, defined($event->{'delay'}) ? $event->{'delay'}:0);
	}
    };

    # Set our own safe SIGCHLD handler
    $self->{'_sigchld'} = POSIX::SigAction->new();
    my $new_chld = POSIX::SigAction->new($sighandler, POSIX::SigSet->new(SIGCHLD));
    $new_chld->safe(1);
    sigaction(SIGCHLD, $new_chld, $self->{'_sigchld'});

    return $event;
}
#
# Register recurring (ever repeating) event
#
#  This method registers a recurring event that gets triggered
#  unconditionally on every event poll. poll() method always
#  returns event handlers for this type of event.
#
#  Event can be bound by any or all of the following:
#   - maximum number of repetitions (iterations),
#   - amount of time elapsed since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'handler' => coderef to the event handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'delay' => start up (first trigger) delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'limit' => maximum number of iterations (optional)
#		  'on_limit' => repetition limit handler callback (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'on_expiry' => event expiration handler callback (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. recurring event hashref, if successful
#		   undef, if failed
#
sub create_recurring_event($%)
{
    my $self = shift;
    my $param = {@_};

    # Event handler callback must be coderef
    return undef unless(defined($param->{'handler'}) &&
			ref($param->{'handler'}) eq 'CODE');

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'recurring',
	'handler'	=> $param->{'handler'},
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'limit'		=> (defined($param->{'limit'}) && int($param->{'limit'}) > 0) ?
				int($param->{'limit'}):undef,
	'on_limit'	=> (ref($param->{'on_limit'}) eq 'CODE') ?
				$param->{'on_limit'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > time()) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef
    );

    # Event limit callback wrapper
    if(defined($event->{'on_limit'})) {
	$event->{'_on_limit'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_limit'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - event hashref
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'		=> $event->{'handler'},
				     'args'		=> [ $event, @{$event->{'args'}}, @_ ],
				     'attempts'		=> $event->{'attempts'},
				     'timeout'		=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Turn absolute expiration timestamp into
    # interval relative to this very moment.
    my $until_interval = defined($event->{'expire_at'}) ?
				($event->{'expire_at'} - time()):undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    my $expiry_interval = defined($event->{'expire_in'}) ?
			    ((defined($until_interval) && $until_interval < $event->{'expire_in'}) ?
				$until_interval:$event->{'expire_in'}):$until_interval;

    # If any time limit is imposed ...
    if(defined($expiry_interval)) {
	# ... schedule event expiration
	$self->{'expire'}->insert($event, $expiry_interval);
    }

    # If startup delay was specified ...
    if(defined($param->{'delay'}) && $param->{'delay'} > 0) {
        # .. schedule event to begin in 'delay' seconds
	$self->{'delay'}->insert($event, $param->{'delay'});
    # Otherwise, set event to begin immediately
    } else {
	# ... by adding it to the table
	# of recurring events
	$self->{'recur'}->add($event);
    }

    return $event;
}
#
# Register timer event
#
#  This method registers a timer event. Each time interval expires
#  (timer ticks), event handler callback function is invoked.
#
#  Event can be bound by any or all of the following:
#   - maximum number of repetitions (ticks),
#   - amount of time elapsed since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'interval' => tick interval of the event (mandatory)
#		  'handler' => coderef to the event handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'delay' => start up (first tick) delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'limit' => maximum number of ticks (optional)
#		  'on_limit' => repetition limit handler callback (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. timer event hashref, if successful
#		   undef, if failed
#
sub create_timer_event($%)
{
    my $self = shift;
    my $param = {@_};

    my $time = time();

    # Callback must be coderef
    return undef unless(defined($param->{'handler'}) &&
			ref($param->{'handler'}) eq 'CODE' &&
			defined($param->{'interval'}) &&
			$param->{'interval'} > 0);

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'timer',
	'interval'	=> $param->{'interval'},
	'handler'	=> $param->{'handler'},
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'limit'		=> (defined($param->{'limit'}) && int($param->{'limit'}) > 0) ?
				int($param->{'limit'}):undef,
	'on_limit'	=> (ref($param->{'on_limit'}) eq 'CODE') ?
				$param->{'on_limit'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > $time) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef
    );

    # Turn relative expiration interval into
    # absolute expiration UNIX timestamp.
    my $expire_timestamp = defined($event->{'expire_in'}) ?
				($time + $event->{'expire_in'}):undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    $event->{'_expire_time'} = defined($event->{'expire_at'}) ?
					((defined($expire_timestamp) && $expire_timestamp < $event->{'expire_at'}) ?
					    $expire_timestamp:$event->{'expire_at'}):$expire_timestamp;

    # Event limit callback wrapper
    if(defined($event->{'on_limit'})) {
	$event->{'_on_limit'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_limit'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - event hashref
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - event hashref
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'		=> $event->{'handler'},
				     'args'		=> [ $event, @{$event->{'args'}}, @_ ],
				     'attempts'		=> $event->{'attempts'},
				     'timeout'		=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Schedule periodic event in LRU
    $self->{'timer'}->insert($event,
			     (defined($param->{'delay'}) &&
			      $param->{'delay'} > 0) ?
				$param->{'delay'}:0.00001);

    return $event;
}
#
# Register I/O event
#
#  This method registers an I/O event on a file/pipe/socket/etc.
#  On event, callback function is invoked.
#
#  Event can be bound by any or all of the following:
#   - maximum number of event triggers,
#   - life time since its creation
#   - specific expiry time given as UNIX timestamp.
#
#  If multiple limits are defined, event is terminated by whichever
#  limit is reached first.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'file' => file handle, socket, etc (mandatory)
#		  'op' => I/O operation ('r' - read, 'w' - write)
#		  'handler' => coderef to the event handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'delay' => trigger delay in seconds (optional)
#		  'attempts' => number of attempts to complete successfully (optional)
#		  'timeout' => single attempt timeout (optional)
#		  'on_timeout' => callback invoked when no more attempts remain (optional)
#		  'limit' => maximum number of occurrences (optional)
#		  'on_limit' => repetition limit handler callback (optional)
#		  'expire_in' => maximum lifetime of the event (optional)
#		  'expire_at' => specific termination time of the event (optional)
#		  'event' => ref to a hash to be used as the event struct (optional)
#
#   Output:	1. file handle, if successful
#		   undef, if failed
#
sub create_io_event($%)
{
    my $self = shift;
    my $param = {@_};

    # Get file descriptor for file
    my $fd = fileno($param->{'file'});

    # File descriptor must be valid,
    # operation must be 'r'ead or 'w'rite,
    # callback must be coderef
    return undef unless(defined($param->{'handler'}) && ref($param->{'handler'}) eq 'CODE' &&
			defined($param->{'op'}) && $param->{'op'} =~ /^r|w$/ &&
			defined($fd));

    # If caller passes a reference to an existing
    # hash, that hash will be used as the event
    # structure. Otherwise, new anonymous hash
    # will be used.
    my $event = defined($param->{'event'}) &&
		ref($param->{'event'}) eq 'HASH' ?
			$param->{'event'}:{};
    # This is our event including its handler.
    %{$event} = (
	'type'		=> 'io',
	'file'		=> $param->{'file'},
	'op'		=> $param->{'op'},
	'handler'	=> $param->{'handler'},
	'args'		=> (ref($param->{'args'}) eq 'ARRAY') ?
				$param->{'args'}:[],
	'delay'		=> (defined($param->{'delay'}) && $param->{'delay'} > 0) ?
				$param->{'delay'}:undef,
	'attempts'	=> (defined($param->{'attempts'}) && int($param->{'attempts'}) > 0) ?
				int($param->{'attempts'}):undef,
	'timeout'	=> (defined($param->{'timeout'}) && $param->{'timeout'} > 0) ?
				$param->{'timeout'}:undef,
	'on_timeout'	=> (ref($param->{'on_timeout'}) eq 'CODE') ?
				$param->{'on_timeout'}:undef,
	'limit'		=> (defined($param->{'limit'}) && int($param->{'limit'}) > 0) ?
				int($param->{'limit'}):undef,
	'on_limit'	=> (ref($param->{'on_limit'}) eq 'CODE') ?
				$param->{'on_limit'}:undef,
	'expire_in'	=> (defined($param->{'expire_in'}) && $param->{'expire_in'} > 0) ?
				$param->{'expire_in'}:undef,
	'expire_at'	=> (defined($param->{'expire_at'}) && $param->{'expire_at'} > time()) ?
				$param->{'expire_at'}:undef,
	'on_expiry'	=> (ref($param->{'on_expiry'}) eq 'CODE') ?
				$param->{'on_expiry'}:undef
    );

    # Event limit callback wrapper
    if(defined($event->{'on_limit'})) {
	$event->{'_on_limit'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - file descriptor
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_limit'},
					 'args'		=> [ $event->{'file'}, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event expiry callback wrapper
    if(defined($event->{'on_expiry'})) {
	$event->{'_on_expiry'} = sub {
	    # Arguments are passed to the handler in the following order:
	    #  - file descriptor
	    #  - arguments defined in 'args' parameter, in the same order
	    #  - whatever arguments are passed to the handler returned by
	    #    the poll() method, when it is actually invoked
	    return $self->invoke_handler('handler'	=> $event->{'on_expiry'},
					 'args'		=> [ $event->{'file'}, @{$event->{'args'}}, @_ ],
					 'attempts'	=> $event->{'attempts'},
					 'timeout'	=> $event->{'timeout'},
					 'on_timeout'	=> $event->{'on_timeout'});
	};
    }

    # Event handler wrapper
    $event->{'_handler'} = sub {
	# Arguments are passed to the handler in the following order:
	#  - file descriptor
	#  - arguments defined in 'args' parameter, in the same order
	#  - whatever arguments are passed to the handler returned by
	#    the poll() method, when it is actually invoked
	return $self->invoke_handler('handler'		=> $event->{'handler'},
				     'args'		=> [ $event->{'file'}, @{$event->{'args'}}, @_ ],
				     'attempts'		=> $event->{'attempts'},
				     'timeout'		=> $event->{'timeout'},
				     'on_timeout'	=> $event->{'on_timeout'});
    };

    # Turn absolute expiration timestamp into
    # interval relative to this very moment.
    my $until_interval = defined($event->{'expire_at'}) ?
				$event->{'expire_at'} - time():undef;

    # Expiry interval for this event will be either
    #  - a lifetime limit (in seconds), or
    #  - a specific time (given as UNIX timestamp),
    # whichever comes first
    my $expiry_interval = defined($event->{'expire_in'}) ?
			    ((defined($until_interval) && $until_interval < $event->{'expire_in'}) ?
				$until_interval:$event->{'expire_in'}):$until_interval;

    # If any time limit is imposed ...
    if(defined($expiry_interval)) {
	# ... schedule event expiration in LRU
	$self->{'expire'}->insert($event, $expiry_interval);
    }

    # Monitor file descriptor for read events ?
    if($event->{'op'} eq 'r') {
	# Setup poll() for read events
	$self->{'rpoll'}->mask($event->{'file'} => POLLIN);
	# Put newly created event into the list
	$self->{'read'}{$fd} = $event;
    # Monitor file descriptor for write events ?
    } elsif($event->{'op'} eq 'w') {
	# Setup poll() for write events
	$self->{'wpoll'}->mask($event->{'file'} => POLLOUT);
	# Put newly created event into the list
	$self->{'write'}{$fd} = $event;
    }

    return $event->{'file'};
}
#
# Unregister termination event
#
#  This method removes termination event from event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. termination event hashref
#
#   Output:	none
#
sub destroy_termination_event($$)
{
    my ($self, $event) = @_;

    return unless(defined($event) &&
		  ref($event) eq 'HASH');

    # Remove event from expiry timer
    $self->{'expire'}->remove($event);
    # Remove event from delay timer
    $self->{'delay'}->remove($event);
    # Restore previous signal handlers
    if($self->{'_sigterm'}) {
	sigaction(SIGTERM, $self->{'_sigterm'});
    }
    if($self->{'_sigint'}) {
	sigaction(SIGINT, $self->{'_sigint'});
    }
}
#
# Unregister reload event
#
#  This method removes reload event from event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. reload event hashref
#
#   Output:	none
#
sub destroy_reload_event($$)
{
    my ($self, $event) = @_;

    return unless(defined($event) &&
		  ref($event) eq 'HASH');

    # Remove event from expiry timer
    $self->{'expire'}->remove($event);
    # Remove event from delay timer
    $self->{'delay'}->remove($event);
    # Restore previous signal handlers
    if($self->{'_sighup'}) {
	sigaction(SIGHUP, $self->{'_sighup'});
    }
}
#
# Unregister reaping event
#
#  This method removes reaping event from event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. reaping event hashref
#
#   Output:	none
#
sub destroy_reaping_event($$)
{
    my ($self, $event) = @_;

    return unless(defined($event) &&
		  ref($event) eq 'HASH');

    # Remove event from expiry timer
    $self->{'expire'}->remove($event);
    # Remove event from delay timer
    $self->{'delay'}->remove($event);
    # Restore previous signal handlers
    if($self->{'_sigchld'}) {
	sigaction(SIGCHLD, $self->{'_sigchld'});
    }
}
#
# Unregister recurring event
#
#  This method removes recurring event from event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. recurring event hashref
#
#   Output:	none
#
sub destroy_recurring_event($$)
{
    my ($self, $event) = @_;

    return unless(defined($event) &&
		  ref($event) eq 'HASH');

    # Remove event from expiry timer
    $self->{'expire'}->remove($event);
    # Remove event from delay timer
    $self->{'delay'}->remove($event);
    # Remove event from the table
    $self->{'recur'}->remove($event);
}
#
# Unregister timer event
#
#  This method removes periodic or scheduled event from
#  event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. timer event hashref
#
#   Output:	none
#
sub destroy_timer_event($$)
{
    my ($self, $event) = @_;

    return unless(defined($event) &&
		  ref($event) eq 'HASH');

    # Remove timer event from timer
    $self->{'timer'}->remove($event);
}
#
# Remove I/O event
#
#  This method removes file read event from event monitor.
#
#   Input:	1. self object reference (passed implicitly)
#		2. file/pipe/socket/etc ...
#		3. I/O - operation ('r'-read, 'w'-write, 'rw'-read/write)
#
#   Output:	none
#
sub destroy_io_event($$;$)
{
    my $self = shift;
    my $file = shift;
    my $op = shift;

    my $fd = fileno($file);
    return unless defined($fd);

    # Destroy I/O event completely ?
    if(!defined($op) || $op eq 'rw') {
	# Remove file descriptor from poll()s
	$self->{'rpoll'}->remove($file);
	$self->{'wpoll'}->remove($file);
	# Remove all expiration timers
	$self->{'expire'}->remove($self->{'read'}{$fd});
	$self->{'expire'}->remove($self->{'write'}{$fd});
	# Remove all delay timers
	$self->{'delay'}->remove($self->{'read'}{$fd});
	$self->{'delay'}->remove($self->{'write'}{$fd});
	# Remove all I/O events
	delete $self->{'read'}{$fd};
	delete $self->{'write'}{$fd};
    # Stop monitoring file for read events ?
    } elsif($op eq 'r') {
	# Remove read expiration timer, if any
	$self->{'expire'}->remove($self->{'read'}{$fd});
	# Remove read delay timer, if any
	$self->{'delay'}->remove($self->{'read'}{$fd});
	# Remove read event
	delete $self->{'read'}{$fd};
	# Remove file from read poll
	$self->{'rpoll'}->remove($file);
    # Stop monitoring file for write events ?
    } elsif($op eq 'w') {
	# Remove write expiration timer, if any
	$self->{'expire'}->remove($self->{'write'}{$fd});
	# Remove write delay timer, if any
	$self->{'delay'}->remove($self->{'write'}{$fd});
	# Remove write event
	delete $self->{'write'}{$fd};
	# Remove file from write poll
	$self->{'wpoll'}->remove($file);
    }
}
#
# Modify termination event
#
#  This method modifies already registered termination event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. termination event hashref
#		3. parameter hash (all parameters are optional):
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'graceful' => boolean to indicate graceful termination
#		  'delay' => start up (first trigger) delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#		  'on_expiry' => event expiration handler callback
#
#   Output:	1. termination event hashref, if successful
#		   undef, if failed
#
sub modify_termination_event($$;%)
{
    my $self = shift;
    my $event = shift;
    my %param = (@_);

    # Input params must be sane
    return undef unless(defined($event) &&
			ref($event) eq 'HASH' &&
			keys %param > 0);

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};

    # If event has expiration time,
    # we need to reschedule it
    $event->{'expire_in'} = $self->{'expire'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remaining delay of event that was
    # already triggered (if any)
    my $due_in = $self->{'delay'}->due($event);

    # Remove termination event
    $self->destroy_termination_event($event);

    # Re-register termination event
    # with modified parameters
    return $self->create_termination_event(%{$event},
					   'delay' => $due_in,
					   'event' => $event);
}
#
# Modify reload event
#
#  This method modifies already registered reload event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. reload event hashref
#		3. parameter hash (all parameters are optional):
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'graceful' => boolean to indicate graceful reload
#		  'delay' => start up (first trigger) delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'limit' => maximum number of iterations
#		  'on_limit' => repetition limit handler callback
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#		  'on_expiry' => event expiration handler callback
#
#   Output:	1. reload event hashref, if successful
#		   undef, if failed
#
sub modify_reload_event($$;%)
{
    my $self = shift;
    my $event = shift;
    my %param = (@_);

    # Input params must be sane
    return undef unless(defined($event) &&
			ref($event) eq 'HASH' &&
			keys %param > 0);

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};

    # If event has expiration time,
    # we need to reschedule it
    $event->{'expire_in'} = $self->{'expire'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remaining delay of event that was
    # already triggered (if any)
    my $due_in = $self->{'delay'}->due($event);

    # Remove reload event
    $self->destroy_reload_event($event);

    # Re-register reload event
    # with modified parameters
    return $self->create_reload_event(%{$event},
				      'delay' => $due_in,
				      'event' => $event);
}
#
# Modify reaping event
#
#  This method modifies already registered reaping event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. reaping event hashref
#		3. parameter hash (all parameters are optional):
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'delay' => start up (first trigger) delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'limit' => maximum number of iterations
#		  'on_limit' => repetition limit handler callback
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#		  'on_expiry' => event expiration handler callback
#
#   Output:	1. reaping event hashref, if successful
#		   undef, if failed
#
sub modify_reaping_event($$;%)
{
    my $self = shift;
    my $event = shift;
    my %param = (@_);

    # Input params must be sane
    return undef unless(defined($event) &&
			ref($event) eq 'HASH' &&
			keys %param > 0);

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};

    # If event has expiration time,
    # we need to reschedule it
    $event->{'expire_in'} = $self->{'expire'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remaining delay of event that was
    # already triggered (if any)
    my $due_in = $self->{'delay'}->due($event);

    # Remove reaping event
    $self->destroy_reaping_event($event);

    # Re-register reaping event
    # with modified parameters
    return $self->create_reaping_event(%{$event},
				       'delay' => $due_in,
				       'event' => $event);
}
#
# Modify recurring event
#
#  This method modifies already registered recurring event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. recurring event hashref
#		3. parameter hash (all parameters are optional):
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'delay' => start up (first trigger) delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'limit' => maximum number of iterations
#		  'on_limit' => repetition limit handler callback
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#		  'on_expiry' => event expiration handler callback
#
#   Output:	1. recurring event hashref, if successful
#		   undef, if failed
#
sub modify_recurring_event($$;%)
{
    my $self = shift;
    my $event = shift;
    my %param = (@_);

    # Input params must be sane
    return undef unless(defined($event) &&
			ref($event) eq 'HASH' &&
			keys %param > 0);

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};

    # If event has expiration time,
    # we need to reschedule it
    $event->{'expire_in'} = $self->{'expire'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remaining delay of event that was
    # already triggered (if any)
    my $due_in = $self->{'delay'}->due($event);

    # Remove recurring event
    $self->destroy_recurring_event($event);

    # Re-register recurring event
    # with modified parameters
    return $self->create_recurring_event(%{$event},
					 'delay' => $due_in,
					 'event' => $event);
}
#
# Modify timer event
#
#  This method modifies already registered timer event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. timer event hashref
#		3. parameter hash (all parameters are optional):
#
#		  'interval' => tick interval of the event
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'delay' => start up (first tick) delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'limit' => maximum number of ticks
#		  'on_limit' => repetition limit handler callback
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#
#   Output:	1. timer event hashref, if successful
#		   undef, if failed
#
sub modify_timer_event($$;%)
{
    my $self = shift;
    my $event = shift;
    my %param = (@_);

    # Input params must be sane
    return undef unless(defined($event) &&
			ref($event) eq 'HASH' &&
			keys %param > 0);

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};

    # Remaining time until next timer tick
    my $due_in = $self->{'timer'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remove timer event
    $self->destroy_timer_event($event);

    # Re-register timer event
    # with modified parameters
    return $self->create_timer_event(%{$event},
				     'delay' => $due_in,
				     'event' => $event);
}
#
# Modify I/O event
#
#  This method modifies already registered I/O event.
#
#   Input:	1. self object reference (passed implicitly)
#		2. file handle
#		3. I/O operation ('r' - read, 'w' - write)
#		4. parameter hash (all parameters are optional):
#
#		  'handler' => coderef to the event handler callback
#		  'args' => arrayref to the callback arguments
#		  'delay' => trigger delay in seconds
#		  'attempts' => number of attempts to complete successfully
#		  'timeout' => single attempt timeout
#		  'on_timeout' => callback invoked when no more attempts remain
#		  'limit' => maximum number of occurrences
#		  'on_limit' => repetition limit handler callback
#		  'expire_in' => maximum lifetime of the event
#		  'expire_at' => specific termination time of the event
#
#   Output:	1. file handle, if successful
#		   undef, if failed
#
sub modify_io_event($$$;%)
{
    my $self = shift;
    my $file = shift;
    my $op = shift;
    my %param = (@_);

    my $fd = fileno($file);
    return unless defined($fd);

    # Input params must be sane
    return undef unless(defined($param{'op'}) &&
			$param{'op'} =~ /^r|w$/ &&
			defined($fd) && $fd > -1 &&
			keys %param > 0);

    # Get event associated with this file descriptor
    my %ops = ('r' => 'read', 'w' => 'write');
    my $event = $self->{$ops{$op}}{$fd};
    # Event must exist
    return undef unless(defined($event) &&
			ref($event) eq 'HASH');

    # Cannot modify event type
    # or event base parameters
    delete $param{'type'};
    delete $param{'file'};
    delete $param{'op'};

    # If event has expiration time,
    # we need to reschedule it
    $event->{'expire_in'} = $self->{'expire'}->due($event);

    # Merge new parameters with existing event
    foreach my $key (keys %param) {
	# Replace event params with new ones
	$event->{$key} = $param{$key};
    }

    # Remaining delay of event that was
    # already triggered (if any)
    my $due_in = $self->{'delay'}->due($event);

    # Remove I/O event
    $self->destroy_io_event($event->{'file'}, $event->{'op'});

    # Re-register I/Or event
    # with modified parameters
    $fd = $self->create_io_event(%{$event});

    # If already triggered event was delayed,
    # reschedule it with remaining delay time
    if(defined($due_in) && $due_in > 0) {
	$self->{'delay'}->insert($event, $due_in);
    }

    return $file;
}
#
# Get registered event
#
#  This method returns the event hashref of the registered event
#  passed parameter belongs to. Parameter can be an event hashref,
#  file descriptor or file handle. Event hashref will be returned
#  only if event is still registered with this instance of event
#  monitor. Even if event hashref is used as first input parameter,
#  result can be undef in case passed event parameter is no longer
#  registered.
#
#   Input:	1. self object reference (passed implicitly)
#		2. event hashref, file descriptor/handle
#		3. I/O - operation ('r'-read, 'w'-write), if previous
#		   parameter was file descriptor or file handle
#
#   Output:	1. event hashref, if registered event was found,
#		   undef, if nothing was found
#
sub get_event($$;$)
{
    my $self = shift;
    my $param = shift;
    my $op = shift;

    my $event;

    my %ops = ('r' => 'read', 'w' => 'write');

    # Event hashref as parameter ?
    if(ref($param) eq 'HASH') {
	if($param->{'type'} eq 'recurring') {
	    # Look for recurring event
	    $event = $self->{'recur'}->exists($param) ? $param:undef;
	} elsif($param->{'type'} eq 'timer') {
	    # Look for timer event
	    $event = $self->{'timer'}->exists($param) ? $param:undef;
	} elsif($param->{'type'} eq 'io') {
	    # Look for I/O event
	    $event = $self->{$ops{$param->{'op'}}}->exists($param) ? $param:undef;
	}
    # File handle as parameter ?
    } elsif((my $fd = fileno($param))) {
	# Get event associated with this file handle
	$event = defined($ops{$op}) ? $self->{$ops{$op}}{$fd}:undef;
    # File descriptor as parameter ?
    } else {
	# Get event associated with this file descriptor
	$event = defined($ops{$op}) ? $self->{$ops{$op}}{$param}:undef;
    }

    return $event;
}
#
# Delay next event handler trigger
#
#  This function delays next event trigger:
#
#  Recurring events are delayed immediatelly by the specified amount.
#  Timer events' next tick is rescheduled by the specified amount.
#  I/O events' next handler trigger is delayed by the specified amount.
#
#   Input:	1. self object reference (passed implicitly)
#		2. event object
#		3. delay in seconds
#
#   Output:	1. TRUE, if succeeded,
#		   FALSE, if failed
#
sub delay_event($$$)
{
    my ($self, $event, $delay) = @_;

    # Input params must be sane
    return 0 unless(defined($event) && ref($event) eq 'HASH' &&
		    defined($delay) && $delay > 0);

    # Recurring event delay ?
    if($event->{'type'} eq 'recurring') {
	# Remove recurring event
	$self->destroy_recurring_event($event);
	# Re-schedule recurring event with delay
	$self->create_recurring_event(%{$event}, 'delay' => $delay, 'event' => $event);
    # Timer event delay ?
    } elsif($event->{'type'} eq 'timer') {
	# Remove timer event
	$self->destroy_timer_event($event);
	# Re-schedule timer event with delay
	$self->create_timer_event(%{$event}, 'delay' => $delay, 'event' => $event);
    # Asynchronous event delay ?
    } elsif($event->{'type'} eq 'io' ||
	    $event->{'type'} eq 'termination' ||
	    $event->{'type'} eq 'reload' ||
	    $event->{'type'} eq 'reaping') {
	# To delay next event trigger,
	# simply add delay value
	$event->{'delay'} = $delay;
    }

    return 1;
}
#
# Poll for registered events
#
#  This function polls for I/O events on file handles
#  and registered timer and 'always on' (recurring)
#  events, building a list of event handlers to be called.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. array of coderefs to event handler functions
#		   for all registered events triggered between
#		   previous and current invocation of this
#		   method.
#
sub poll($)
{
    my $self = shift;
    my @term_handlers = ();
    my @reload_handlers = ();
    my @event_handlers = ();
    my @reaping_handlers = ();
    my @graceful_term_handlers = ();
    my @graceful_reload_handlers = ();

    # Event engine must be running
    return () unless $self->{'state'};

    #
    # Operation of this method is non-blocking.
    #
    # I/O events are always processed immediately as
    # they happen, while other events are processed
    # in time slices.
    #
    # Time is expressed in seconds, but in floating
    # point format, so it can define intervals that
    # are just a fraction of a second.
    #

    # Current time with microsecond precision
    my $time = time();

    # If recurring events are registered, our precision
    # has to be as high as possible. Otherwise, if timer
    # events are registered, timeslice will be equal to
    # the time until the next pending synchronous event.
    #
    # If neither recurring nor timer events are scheduled,
    # default time slice will be 1 second, as the remaining
    # asynchronous (I/O) events are not affected by time
    # slices.

    my @pending_intervals = (
	($self->{'recur'}->count > 0) ? 0.0001:undef,
	$self->{'expire'}->firstdue,
	$self->{'timer'}->firstdue,
	$self->{'delay'}->firstdue,
	1
    );

    my $timeslice;

    foreach my $next (@pending_intervals) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Skip empty time queues
	next unless defined $next && $next > 0;
	# Look for closest upcoming event
	unless(defined($timeslice) &&
	      $timeslice <= $next) {
	    $timeslice = $next;
	}
    }

    # Wait for I/O events using system poll()
    # as a generic hi-res wait/sleep mechanism.
    # We cannot use write poll here because,
    # unlike read operations, file handles are
    # ready for write operations most of the time,
    # so we wouldn't get any sleep which would
    # lead to high CPU utilization. Therefore, we
    # use read poll both to wait for read events
    # and to perform nano sleeps. We don't wait
    # for write events, just collect them from
    # write poll on each pass, if they are ready.
    $self->{'rpoll'}->poll($timeslice);

    # Remove all expired events, excluding timer events
    while((my $ev = $self->{'expire'}->expired())) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Termination event expiration
	if($ev->{'type'} eq 'termination') {
	    # Simply remove termination event
	    $self->destroy_termination_event($ev);
	# Reload event expiration
	} elsif($ev->{'type'} eq 'reload') {
	    # Simply remove reload event
	    $self->destroy_reload_event($ev);
	# Reaping event expiration
	} elsif($ev->{'type'} eq 'reaping') {
	    # Simply remove reaping event
	    $self->destroy_reaping_event($ev);
	# Recurring event expiration
	} elsif($ev->{'type'} eq 'recurring') {
	    # Simply remove recurring event
	    $self->destroy_recurring_event($ev);
	# I/O event expiration
	} elsif($ev->{'type'} eq 'io') {
	    # Simply remove I/O event
	    $self->destroy_io_event($ev->{'file'}, $ev->{'op'})
	}
	# Append event expiration callback coderef to
	# the list of handlers that should be called
	if(defined($ev->{'_on_expiry'})) {
	    push @event_handlers, $ev->{'_on_expiry'};
	}
    }

    # Get all read-ready file descriptors
    my @read_ready = $self->{'rpoll'}->handles(POLLIN);
    # Prepare all read-ready event handlers
    foreach my $file (@read_ready) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Get ready file descriptor
	my $fd = defined($file) ? fileno($file):undef;
	next unless defined($fd);
	# If ready file handle is registered for I/O event ...
	my $ev = $self->{'read'}{$fd};
	next unless defined($ev);
	# If trigger delay is defined ...
	if(defined($ev->{'delay'}) && $ev->{'delay'} > 0) {
	    # ... schedule event handler trigger
	    $self->{'delay'}->insert($ev, $ev->{'delay'});
	# Otherwise, trigger handler immediately
	} else {
	    # Append event handler coderef to the list
	    # of event handlers that should be called
	    push @event_handlers, $ev->{'_handler'};
	    # Is there a limit on number of event triggers ?
	    if(defined($ev->{'limit'})) {
		# Reduce trigger counter
		$ev->{'limit'}--;
		# If trigger counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @event_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_io_event($file, 'r');
		}
	    }
	}
    }

    # As previosly stated, we don't wait for
    # write events, just check for ready ones
    # and collect them from write poll.
    $self->{'wpoll'}->poll(0);
    # Collect all write-ready file descriptors
    my @write_ready = $self->{'wpoll'}->handles(POLLOUT);
    # Prepare all write-ready event handlers
    foreach my $file (@write_ready) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Get ready file descriptor
	my $fd = defined($file) ? fileno($file):undef;
	next unless defined($fd);
	# If ready file handle is registered for I/O event ...
	my $ev = $self->{'write'}{$fd};
	next unless defined($ev);
	# If trigger delay is defined ...
	if(defined($ev->{'delay'}) && $ev->{'delay'} > 0) {
	    # ... schedule event handler trigger
	    $self->{'delay'}->insert($ev, $ev->{'delay'});
	# Otherwise, trigger handler immediately
	} else {
	    # Append event handler coderef to the list
	    # of event handlers that should be called
	    push @event_handlers, $ev->{'_handler'};
	    # Is there a limit on number of event triggers ?
	    if(defined($ev->{'limit'})) {
		# Reduce trigger counter
		$ev->{'limit'}--;
		# If trigger counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @event_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_io_event($file, 'w');
		}
	    }
	}
    }

    # Recurring events are split into sub-second
    # time slices and processed by invoking their
    # handlers many times per second
    for(my $ev = $self->{'recur'}->first;
	defined($ev);
	$ev = $self->{'recur'}->next($ev)) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Append event handler coderef to the list
	# of event handlers that should be called
	push @event_handlers, $ev->{'_handler'};
	# Is there a limit on the number of iterations ?
	if(defined($ev->{'limit'})) {
	    # Reduce iteration counter
	    $ev->{'limit'}--;
	    # If iteration counter reached zero,
	    if($ev->{'limit'} < 1) {
		# Append trigger limit callback coderef to the list
		# of event handlers that should be called
		if(defined($ev->{'_on_limit'})) {
		    push @event_handlers, $ev->{'_on_limit'};
		}
		# Remove the event
		$self->destroy_recurring_event($ev);
	    }
	}
    }

    # Prepare/delay/expire timer events
    while((my $ev = $self->{'timer'}->expired())) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Append event handler coderef to the list
	# of event handlers that should be called
	push @event_handlers, $ev->{'_handler'};
	# Is there a limit on the number of timer ticks ?
	if(defined($ev->{'limit'})) {
	    # Reduce tick counter
	    $ev->{'limit'}--;
	    # If iteration counter reached zero,
	    if($ev->{'limit'} < 1) {
		# Append trigger limit callback coderef to the list
		# of event handlers that should be called
		if(defined($ev->{'_on_limit'})) {
		    push @event_handlers, $ev->{'_on_limit'};
		}
		# Remove the event
		$self->destroy_timer_event($ev);
		next;
	    }
	}
	# Did event expire ?
	if(defined($ev->{'_expire_time'}) &&
	   $ev->{'_expire_time'} >= $time) {
	    # Append event expiration callback coderef to
	    # the list of handlers that should be called
	    if(defined($ev->{'_on_expiry'})) {
		push @event_handlers, $ev->{'_on_expiry'};
	    }
	    # Remove the event
	    $self->destroy_timer_event($ev);
	    next;
	}
	# Reschedule event
	$self->{'timer'}->insert($ev, $ev->{'interval'});
    }

    # Prepare all delayed events, excluding timer events
    while((my $ev = $self->{'delay'}->expired())) {
	# Event engine must be running
	return () unless $self->{'state'};
	# Termination event delay
	if($ev->{'type'} eq 'termination') {
	    # Should termination be handled gracefully ?
	    if($ev->{'graceful'}) {
		# Append event handler coderef
		# to the list of event handlers
		# to allow all pending events to
		# be handled before termination.
		push @graceful_term_handlers, $ev->{'_handler'};
	    } else {
		# Prepend event handler coderef
		# to the list of event handlers
		# to be called before others,
		# preventing them from being
		# handled.
		push @term_handlers, $ev->{'_handler'};
	    }
	# Reload event delay
	} elsif($ev->{'type'} eq 'reload') {
	    # Should reload be handled gracefully ?
	    if($ev->{'graceful'}) {
		# Append event handler coderef
		# to the list of event handlers
		# to allow all pending events to
		# be handled before reload.
		push @graceful_reload_handlers, $ev->{'_handler'};
	    } else {
		# Prepend event handler coderef
		# to the list of event handlers
		# to be called before others,
		# reconfiguring them before they
		# are handled.
		push @reload_handlers, $ev->{'_handler'};
	    }
	    # Is there a limit on number of event triggers ?
	    if(defined($ev->{'limit'})) {
		# Reduce trigger counter
		$ev->{'limit'}--;
		# If trigger counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @graceful_reload_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_reload_event($ev);
		}
	    }
	# Reaping event delay
	} elsif($ev->{'type'} eq 'reaping') {
	    # Add multiple handlers, one per child process
	    while((my $pid = shift @{$ev->{'_pids'}})) {
		# Append event handler coderef
		# to the list of event handlers
		# to be called last
		push @reaping_handlers, sub {
		    # Arguments are passed to the handler in the following order:
		    #  - event hashref
		    #  - child process PID
		    #  - arguments defined in 'args' parameter, in the same order
		    #  - whatever arguments are passed to the handler returned by
		    #    the poll() method, when it is actually invoked
		    return $ev->{'_handler'}->($pid, @{$ev->{'args'}}, @_);
		};
	    }
	    # Is there a limit on number of event triggers ?
	    if(defined($ev->{'limit'})) {
		# Reduce trigger counter
		$ev->{'limit'}--;
		# If trigger counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @reaping_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_reaping_event($ev);
		}
	    }
	# Recurring event delay
	} elsif($ev->{'type'} eq 'recurring') {
	    # Append event handler coderef
	    # to the list of event handlers
	    # that should be called
	    push @event_handlers, $ev->{'_handler'};
	    # Is there a limit on the number of iterations ?
	    if(defined($ev->{'limit'})) {
		# Reduce iteration counter
		$ev->{'limit'}--;
		# If iteration counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @event_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_recurring_event($ev);
		    next;
		}
	    }
	    # Move event back to it's natural habitat
	    $self->{'recur'}->add($ev);
	# I/O event delay
	} elsif($ev->{'type'} eq 'io') {
	    # Append event handler coderef
	    # to the list of event handlers
	    # that should be called
	    push @event_handlers, $ev->{'_handler'};
	    # Is there a limit on number of event triggers ?
	    if(defined($ev->{'limit'})) {
		# Reduce trigger counter
		$ev->{'limit'}--;
		# If trigger counter reached zero,
		if($ev->{'limit'} < 1) {
		    # Append trigger limit callback coderef to the list
		    # of event handlers that should be called
		    if(defined($ev->{'_on_limit'})) {
			push @event_handlers, $ev->{'_on_limit'};
		    }
		    # Remove the event
		    $self->destroy_io_event($ev->{'file'}, $ev->{'op'});
		}
	    }
	}
    }

    return (@term_handlers,
	    @reload_handlers,
	    @event_handlers,
	    @reaping_handlers,
	    @graceful_term_handlers,
	    @graceful_reload_handlers);
}
#
# Invoke event handler (with timeout).
#
#  This function calls a callback function, wrapping it inside a block
#  which times its execution and interrupts it when the timer expires.
#  Handler is considered failed if it didn't complete in time.
#
#  Optionally, you can define the number of attempts to sucessfully
#  complete the execution of the handler and run a callback function
#  if all attempts have failed.
#
#  Timeout callback receives the same set of arguments as the handler.
#  This callback can complete the aborted operation and produce proper
#  return values, or simply report an error.
#
#   Input:	1. self object reference (passed implicitly)
#		2. parameter hash:
#
#		  'handler' => coderef to the handler callback (mandatory)
#		  'args' => arrayref to the callback arguments (optional)
#		  'attempts' => number of handler's attempts to complete (optional)
#		  'timeout' => single handler attempt timeout (optional)
#		  'on_timeout' => callback invoked when no attempts remain (optional)
#
#   Output:	whatever the called function returns,
#		or explicitly undef, if timed out
#
sub invoke_handler($%)
{
    my $self = shift;
    my %param = (@_);

    # This variable receives the return value
    # from the handler in scalar context
    my $scalar;
    # This variable receives the return value
    # from the handler in list context
    my @array;

    # Timeout flag:
    #  TRUE - timeout happened,
    #  FALSE - no timeout
    my $to;

    # Did the caller request list context ?
    my $wantarray = wantarray;

    # Handler must be coderef
    return $wantarray ? ():undef unless(defined($param{'handler'}) &&
					ref($param{'handler'}) eq 'CODE');

    # Handler we wish to execute
    my $handler = $param{'handler'};

    # Unless specified, timeout is 0 (no timeout)
    my $timeout = defined($param{'timeout'}) ?
				$param{'timeout'}:0;

    # Unless specified, number of attempts is 1
    my $attempts = (defined($param{'attempts'}) &&
		    $param{'attempts'} > 1) ?
				int($param{'attempts'}):1;

    # Custom timeout handler has the chance to complete
    # the operation or simply cleanup and/or report error
    my @on_timeout = (defined($param{'on_timeout'}) &&
		      ref($param{'on_timeout'}) eq 'CODE') ?
			($param{'on_timeout'}):();

    # Define temporary SIGALRM signal handler
    my $old_sigalrm = POSIX::SigAction->new();
    my $sigalrm = POSIX::SigAction->new(sub { $to = 1; die "Handler function call timed out"; },
					POSIX::SigSet->new(SIGALRM));
    sigaction(SIGALRM, $sigalrm, $old_sigalrm);

    # Remove the last saved error message
    undef $self->{'_errormsg'};

    # Retry as long as there is a function to call
    while(defined($handler)) {
	# Execute handler inside the sandbox
	eval {
	    eval {
		# Reset the timeout flag
		$to = 0;
		# Start timing the execution
		alarm($timeout);
		# Call handler function
		if($wantarray) {
		    @array = ($handler->(@{$param{'args'}}));
		} else {
		    $scalar = $handler->(@{$param{'args'}});
		}
	    };
	    # Disable the alarm clock
	    alarm(0);
	    # Unless timeout occured, we are done, even
	    # if handler failed for some other reason
	    undef $handler;
	    # Did nested eval block end abruptly ?
	    if($@) {
		# Store the last error message we caught
		$self->{'_errormsg'} = $@;
		# If eval termination was caused by handler timeout ...
		if($to) {
		    # We just spent one attempt
		    # to complete the execution
		    $attempts--;
		    # Once all attempts have failed, replace
		    # the handler coderef with optional timeout
		    # handler. If timeout handler is not defined,
		    # or it has itself timed out, array will be
		    # empty, which will cause retry loop to end.
		    $handler = ($attempts > 0) ?
				    $param{'handler'}:
				    shift @on_timeout;
		}
	    }
	};
    }

    # Restore the original signal handler
    sigaction(SIGALRM, $old_sigalrm);

    return $wantarray ? @array:$scalar;
}
#
# Retrieve the last error message
#
#  Retrieve the error message produced by previous handler
#  function call performed via invoke_handler() method.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. error message, if previous handler
#		   invocation via invoke_handler() failed,
#		   undef, if handler invocation succeeded
#
sub error_message($)
{
    my $self = shift;

    return $self->{'_errormsg'};
}
#
# Remove all events from the event monitor
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	nothing
#
sub flush($)
{
    my $self = shift;

    if(defined($self->{'timer'}) &&
       ref($self->{'timer'} eq "api::util::lru")) {
	$self->{'timer'}->flush();
    }
    if(defined($self->{'expire'}) &&
       ref($self->{'expire'} eq "api::util::lru")) {
	$self->{'expire'}->flush();
    }
    if(defined($self->{'delay'}) &&
       ref($self->{'delay'} eq "api::util::lru")) {
	$self->{'delay'}->flush();
    }
    if(defined($self->{'recur'}) &&
       ref($self->{'recur'} eq "api::util::table")) {
	$self->{'recur'}->flush();
    }
    foreach my $fd (keys %{$self->{'read'}}) {
	if(defined($fd)) {
	    $self->{'rpoll'}->remove($fd);
	}
    }
    $self->{'read'} = {};
    foreach my $fd (keys %{$self->{'write'}}) {
	if(defined($fd)) {
	    $self->{'wpoll'}->remove($fd);
	}
    }
    $self->{'write'} = {};
}

1;
