#
# api::util::sendmail.pm
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

package api::util::sendmail;

##########################################################################################

use strict;
use warnings;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. (optional) full path to sendmail executable
#
#   Output:	1. api::util::mailer object reference
#
sub new($;$)
{
    my ($class, $sendmail) = @_;

    # If path to sendmail executable wasn't given as input,
    # we need to look for it in most likely places
    unless(defined($sendmail) && $sendmail ne '' && -x $sendmail) {

	my @likely_sendmail_locations = qw(/usr/sbin /usr/lib /usr/bin /usr/local/sbin /usr/local/lib /usr/local/bin /opt/sbin /opt/lib /opt/bin);

	# Look for sendmail executable in likely locations first
        foreach my $path (@likely_sendmail_locations) {
	    $path .= '/sendmail';
	    if(-x $path) {
		$sendmail = $path;
		last;
	    }
	}

	# If sendmail wasn't found in any of likely places,
	# ask system for assistance ...
	unless(defined($sendmail) && $sendmail ne "" && -x $sendmail) {
	    # Use whereis command to find sendmail
	    my @whereis = `whereis sendmail`;
	    w: foreach my $line (@whereis) {
		# Strip trailing newline
		chop($line);
		# Split each line of whereis output
		# into a list of paths
		my @paths = split(/\s+/, $line);
		foreach my $path (@paths) {
		    if(-x $path) {
			$sendmail = $path;
			last w;
		    }
		}
	    }
	}

	# Give up if sendmail executable still hasn't been found
	return undef unless(defined($sendmail) &&
		     $sendmail ne "" &&
		     -x $sendmail);

    }

    my $self = {
	'sendmail' => $sendmail
    };

    return bless($self, $class);
}
#
# Send email using local mailer
#
#   Input:	1. self object reference
#		2. sender name
#		3. sender email
#		4. recipient(s)
#		5. subject
#		6. content
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub send($$$$$$)
{
    my ($self, $sender_name, $sender_email, $recipients, $subject, $content) = @_;

    return 0 unless(defined($self->{'sendmail'}) && -x $self->{'sendmail'});

    my $from = (defined($sender_name) && $sender_name ne '') ?
		$sender_name.' <'.$sender_email.'>':$sender_email;

    my $to = (ref($recipients) eq 'ARRAY') ? $recipients:[$recipients];

    local *SM;

    # Run mailer
    if(open(SM, '|'.$self->{'sendmail'}.' -f '.$sender_email.' -t')) {
	# Pipe email to mailer
	print SM "From: ".$from."\n";
	print SM "To: ".join(',', @{$to})."\n";
	print SM "Subject: ".$subject."\n\n";
	print SM $content;
	# Close pipe
	close(SM);
	# Done
	return 1;
    }

    return 0;
}

1;
