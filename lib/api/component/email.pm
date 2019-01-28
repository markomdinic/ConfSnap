#
# api::component::email.pm
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

package api::component::email;

##########################################################################################

use strict;
use warnings;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. hashref to the global configuration
#
#   Output:	1. api::component::vcs object reference
#
sub new($$)
{
    my ($class, $conf) = @_;

    # Global configuration must be defined
    return undef unless(defined($conf) && ref($conf) eq "HASH");

    return bless({}, $class);
}
#
# Send email
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
sub send_email($$$$$$)
{
    my ($self, $sender_name, $sender_email, $recipients, $subject, $content) = @_;
    my $mailer;

    # Use SMTP for sending emails ?
    if($self->{'conf'}->{'use_smtp'}) {
	# Attempt to load SMTP mailer
	return 0 unless $self->load_module('api::util::smtp');
	# Instantiate SMTP mailer
	$mailer = api::util::smtp->new($self->{'conf'}->{'smtp_server'},
				       $self->{'conf'}->{'smtp_port'},
				       $self->{'conf'}->{'smtp_ssl'},
				       $self->{'conf'}->{'smtp_tls'},
				       $self->{'conf'}->{'smtp_auth'},
				       $self->{'conf'}->{'smtp_user'},
				       $self->{'conf'}->{'smtp_pass'});
    # Otherwise, use local mailer
    } else {
	# Attempt to load local mailer
	return 0 unless $self->load_module('api::util::sendmail');
	# Instantiate local mailer
	$mailer = api::util::sendmail->new();
    }

    # Cannot proceed without mailer
    return 0 unless defined($mailer);

    # Send email
    return $mailer->send($sender_name, $sender_email, $recipients, $subject, $content);
}

1;
