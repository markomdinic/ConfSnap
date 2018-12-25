#
# api::util::smtp.pm
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

package api::util::smtp;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use Net::SMTP;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. SMTP server hostname/address
#		3. (optional) SMTP port
#		4. (optional) use SSL (boolean)
#		5. (optional) use TLS (boolean)
#		6. (optional) use SMTP auth (boolean)
#		7. (optional) SMTP username
#		8. (optional) SMTP password
#
#   Output:	1. api::component::vcs object reference
#
sub new($$;$$$$$$)
{
    my ($class, $server, $port, $ssl, $tls, $auth, $user, $pass) = @_;

    return undef unless(defined($server) && $server ne '');

    return bless({
	'server' => $server,
	'port'	 => defined($port) ? $port:25,
	'ssl'	 => defined($ssl) ? $ssl:0,
	'tls'	 => defined($tls) ? $tls:0,
	'auth'	 => defined($auth) ? $auth:0,
	'user'	 => $user,
	'pass'	 => $pass
    }, $class);
}
#
# Send email using SMTP
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

    # Connect to SMTP relay
    my $smtp = Net::SMTP->new($self->{'server'},
			      'Port' => $self->{'port'},
			      'SSL' => $self->{'ssl'} && !$self->{'tls'});

    # Use STARTTLS ?
    if($self->{'tls'} && !$self->{'ssl'}) {
	# Negotiate TLS
	$smtp->starttls;
    }

    # Use SMTP auth ?
    if($self->{'auth'}) {
	my $user = $self->{'user'};
	my $pass = $self->{'pass'};
	if(defined($user) && $user ne '' &&
	   defined($pass) && $pass ne '') {
	    # Use configured SMTP auth credentials
	    $smtp->auth($user, $pass);
	}
    }

    my $from = (defined($sender_name) && $sender_name ne '') ?
			$sender_name.' <'.$sender_email.'>':$sender_email;

    my $to = ref($recipients) eq 'ARRAY' ? $recipients:[$recipients];

    # Send email
    $smtp->mail($sender_email);
    $smtp->to(@{$to});
    $smtp->data();
    $smtp->datasend("From: ".$from."\n");
    $smtp->datasend("To: ".join(',', @{$to})."\n");
    $smtp->datasend("Subject: ".$subject."\n");
    $smtp->datasend("\n");
    $smtp->datasend($content);
    $smtp->dataend();

    return 1;
}

1;
