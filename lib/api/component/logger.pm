#
# api::component::logger.pm
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

package api::component::logger;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use Sys::Syslog;

##########################################################################################

#
# Module constructor
#
#  This function is called in order to create
#  API base module instance.
#
#   Input:	1. class name (passed implicitly)
#		2. hashref to the global configuration
#
#   Output:	1. api::logger object reference
#
sub new($$)
{
    my ($class, $conf) = @_;

    return undef unless(defined($conf) && ref($conf) eq "HASH");

    return bless({}, $class);
}
#
# Issue conditional syslog call
#
#  This function check if given log level is less or equal
#  to configured max. log level and only then issues syslog()
#  call.
#
#   Input:	1.self object reference
#		2.log level
# 		3.format string
#		+ variable number of arguments
#
#   Output:	nothing
#
sub logging($$;@)
{
    my $self = shift;
    my $loglevel = shift;
    my @SYSLOG_LEVELS = ('LOG_EMERG','LOG_ALERT','LOG_CRIT','LOG_ERR','LOG_WARNING','LOG_NOTICE','LOG_INFO','LOG_DEBUG');
    my $i;
    for($i = 0; $i < $#SYSLOG_LEVELS+1; $i++) {
	last if($SYSLOG_LEVELS[$i] eq $loglevel);
    }

    for(; $i < $#SYSLOG_LEVELS+1; $i++) {
	if($SYSLOG_LEVELS[$i] eq $self->{'conf'}->{'syslog_level'}) {

	    # Should we log message to syslog ?
	    if($self->{'conf'}->{'log_to_syslog'}) {
		syslog($loglevel, @_);
	    }

	    # Local format of the message
	    my $fmt = shift(@_);
	    my ($level) = ($loglevel =~ /^LOG\_(.+)$/);
	    my $message = sprintf("[".$level."] ".$fmt."\n", @_);

	    # Should we log to console ?
	    if($self->{'conf'}->{'log_to_console'}) {
		print STDERR $message;
	    }

	    # Should we append message to local buffer ?
	    if(defined($self->{'local_log_buffer'})) {
		$self->{'local_log_buffer'} .= $message;
	    }

	    last;

	}
    }
}
#
# Retrieve or set the local log buffer
#
#  If initially set to any value, even an empty string, a local,
#  in-memory log storage will be created that will buffer all
#  messages logged with logging() method. It can be retrieved
#  or reset at any time.
#
#  It is not created automatically or by default. Therefore,
#  logged messages are sent only to syslog and, optionally
#  to the STDERR, by default.
#
#  Log buffer's contents are retrieved by invoking this method.
#
#  Log buffer's contents are explicitly set/reset by using this
#  method as LVALUE.
#
#  It should be used with caution, especially in loops, as it
#  can consume considerable amounts of RAM, especially when
#  logging excessivly.
#
#   Input:	1. self object reference
#
#   Output:	1. log buffer to be read, in case method
#		   is on the right side of an expression,
#		   or set, in case method is on the left
#		   side of an expression.
#
sub local_log($) : lvalue
{
    my $self = shift;

    $self->{'local_log_buffer'};
}

1;
