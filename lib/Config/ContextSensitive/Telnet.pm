# Config::ContextSensitive::Telnet.pm
#
# Copyright (c) 2019 Marko Dinic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#


#############################################################################################
######################### P A C K A G E / E X P O R T   S T U F F ###########################

package Config::ContextSensitive::Telnet;

use Exporter;
use Config::ContextSensitive qw(:macros);

our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

$VERSION = '0.01';
@ISA = qw(Exporter Config::ContextSensitive);
*EXPORT = *Config::ContextSensitive::EXPORT;
*EXPORT_OK = *Config::ContextSensitive::EXPORT_OK;
*EXPORT_TAGS = *Config::ContextSensitive::EXPORT_TAGS;

#############################################################################################
################################### C O N S T A N T S #######################################

# ioctl() constants from asm/ioctls.h
use constant {
    TIOCGWINSZ			=> 0x5413,
    TIOCSWINSZ			=> 0x5414,
    FIONBIO			=> 0x5421
};

use constant {
    # Telnet command escape
    IAC				=> "\xff",
    # Telnet commands
    SE				=> "\xf0",
    BREAK			=> "\xf3",
    INTPROC			=> "\xf4",
    SB				=> "\xfa",
    WILL			=> "\xfb",
    WONT			=> "\xfc",
    DO				=> "\xfd",
    DONT			=> "\xfe",
    # Telnet options
    ECHO			=> "\x01",
    SUPPRESS_GO_AHEAD		=> "\x03",
    STATUS			=> "\x05",
    TIMINGMARK			=> "\x06",
    TERMTYPE			=> "\x18",
    NAWS			=> "\x1f",
    TERMINAL_SPEED		=> "\x20",
    FLOWCONTROL			=> "\x21",
    LINEMODE			=> "\x22",
    ENVVARS			=> "\x24",
    # Telnet suboptions
    IS				=> "\x00",
    SEND			=> "\x01"
};

# Our defaults
use constant DEFAULT_MAXTTYS	=> 16;

#############################################################################################
###################################### G L O B A L S ########################################

use Socket;
use IO::Pty;
use Fcntl qw(F_GETFL F_SETFL O_NONBLOCK);
use IO::Poll qw(POLLIN);


use strict;
use warnings;

my %DBG_TELCMDS = (
    &DO			=> 'DO',
    &DONT		=> 'DONT',
    &WILL		=> 'WILL',
    &WONT		=> 'WONT'
);

my %DBG_TELOPTS = (
    &ECHO		=> 'ECHO',
    &SUPPRESS_GO_AHEAD	=> 'SUPPRESS-GO-AHEAD',
    &STATUS		=> 'STATUS',
    &TIMINGMARK		=> 'TIMINGMARK',
    &TERMTYPE		=> 'TERMINAL-TYPE',
    &TERMINAL_SPEED	=> 'TERMINAL-SPEED',
    &NAWS		=> 'NAWS',
    &FLOWCONTROL	=> 'FLOWCONTROL',
    &LINEMODE		=> 'LINEMODE',
    &ENVVARS		=> 'ENVVARS'
);

#############################################################################################
############################ O B J E C T   I N T E R F A C E ################################

sub new {
    my $class = shift;
    my $opts = {@_};

    return undef unless defined($opts->{'template'});

    # Create poll() object
    my $poll = defined($opts->{'poll'}) ?
		$opts->{'poll'}:
		IO::Poll->new();

    return undef unless defined($poll);

    # Create new config object
    # that we will inherit from
    my $self = Config::ContextSensitive->new($opts->{'template'});
    return undef unless defined($self);

    # This will class speficic data
    # for our object
    $self->{'parent'}		= $$;
    $self->{'poll'}		= $poll;
    $self->{'run'}		= 1;
    $self->{'pty'}		= [];
    $self->{'tty'}		= {};
    $self->{'sess'}		= {};
    $self->{'localaddr'}	= $opts->{'localaddr'} ?
    				  $opts->{'localaddr'}:"0.0.0.0";
    $self->{'localport'}	= $opts->{'localport'} ?
				  $opts->{'localport'}:23;
    $self->{'prompt'}		= defined($opts->{'prompt'}) ?
				  $opts->{'prompt'}:undef;
    $self->{'banner'}		= defined($opts->{'banner'}) ?
				  $opts->{'banner'}:undef;
    $self->{'proctitle'}	= defined($opts->{'proctitle'}) ?
				  $opts->{'proctitle'}:undef;
    $self->{'debug'}		= $opts->{'debug'} ? 1:0;

    # Bless this hash into object
    # ... oh Perl, almighty :)
    bless $self, $class;

    # Set maximum number of TTYs
    my $maxttys = defined($opts->{'maxttys'}) ?
	$opts->{'maxttys'}:DEFAULT_MAXTTYS;

    # Create pseudo terminal objects
    for(1..$maxttys) {

	# Create new pty
	my $pty = IO::Pty->new();
	return undef unless defined($pty);
	# Put pty into raw mode
	$pty->set_raw();
	# Set non blocking mode for PTY
	$self->setnonblocking($pty);

	# Get it's subordinate tty
	my $tty = $pty->slave;
	return undef unless defined($tty);
	# Put tty into raw mode
	$tty->set_raw();
	# Set non blocking mode for TTY
	$self->setnonblocking($tty);
	# Add tty to the poll() monitor
	$poll->mask($tty => POLLIN);

	# Create new CLI slave for every tty
	my $cli = $self->cli('tty' => $tty,
			     'prompt' => $self->{'prompt'},
			     'banner' => $self->{'banner'},
			     'dontblock' => 1);
	return undef unless defined($cli);

	# Store new CLI object
	$self->{'tty'}{$tty} = $cli;
	# Store newly open pty into pty pool
	push @{$self->{'pty'}}, $pty;

    }

    # Spawn telnet daemon
    my $pid = $self->daemon;

    return undef unless defined $pid;
    $self->{'child'} = $pid;

    return $self;
}

sub process_once($) {
    my $self = shift;
    # Get list of handles that have read event pending
    my @ttys = $self->{'poll'}->handles(POLLIN);
    # Loop through all read ready sockets
    foreach my $tty (@ttys) {
	# Attach CLI to tty if neccessary
	unless(defined($self->{'tty'}{$tty})) {
	    # Create new CLI slave for every tty
	    my $cli = $self->cli('tty' => $tty,
				 'prompt' => $self->{'prompt'},
				 'banner' => $self->{'banner'},
				 'dontblock' => 1);
	    return undef unless defined($cli);
	    # Store new CLI object
	    $self->{'tty'}{$tty} = $cli;
	}
	# Process event through CLI
	my $res = $self->{'tty'}{$tty}->process;
	# If CLI process() returned undef,
	# user initiated logout event
	unless(defined($res)) {
	    # Remove CLI object
	    undef %{$self->{'tty'}{$tty}};
	    delete $self->{'tty'}{$tty};
	}
    }
}

sub process_loop($) {
    my $self = shift;

    # We are on the slave side,
    # so close master ptys
    foreach my $pty (@{$self->{'pty'}}) {
	# Close the master
	close($pty);
    }

    # This is main slave processing loop
    while($self->{'run'}) {
	# Wait on events
	if($self->{'poll'}->poll(0.01) > 0) {
	    # Process all ready ttys
	    $self->process_once;
	}
    }
#    print "processloop ended\n";
}

sub end_loop($) {
    my $self = shift;
    # Pull the run flag down
    $self->{'run'} = 0;
    # Send TERM to the telnet daemon child
    kill 'TERM', $self->{'child'};
}

sub daemon($) {
    my $self = shift;

    # Launch daemon child process
    my $pid = fork();
    return undef unless defined($pid);
    return $pid if $pid;

    ## Child process starts here ##

    # Set proc title if requested
    if(defined($self->{'proctitle'}) &&
       $self->{'proctitle'} ne "") {
	$0 = $self->{'proctitle'};
    }

    # Prepare signal handlers
    $SIG{TERM} = $SIG{CHLD} = $SIG{INT} = sub { $self->{'run'} = 0; };

    # Close the TTY slaves on our side
    foreach my $pty (@{$self->{'pty'}}) {
	$pty->close_slave;
    }
    delete $self->{'tty'};
    delete $self->{'poll'};

    # Create poll() object for our side
    $self->{'poll'} = IO::Poll->new();

    # Initiate listener socket
    my $listen = $self->listen();
    return undef unless defined $listen;

    while($self->{'run'}) {
	# Wait on events
	if($self->{'poll'}->poll(0.01) > 0) {
	    # Get list of handles that have read event pending
	    my @ready = $self->{'poll'}->handles(POLLIN);
	    # Loop through all read ready sockets
	    foreach my $rdy (@ready) {
		# If socket is our listener
		if($rdy == $listen) {
		    # Accept the incoming connection
		    my $nvt = $self->accept($listen);
		    # Fetch first free pty from the pool
		    my $pty = shift @{$self->{'pty'}};
		    if(defined($pty)) {
			# Monitor PTY for read events
			$self->{'poll'}->mask($pty => POLLIN);
			# Create session entry: associate
			# pty with this client connection
			# and vice versa
			$self->{'sess'}{$pty}{'nvt'} = $nvt;
			$self->{'sess'}{$nvt}{'pty'} = $pty;
		    } else {
			# reject the client if no ptys remain
			$self->close($nvt);
		    }
		# Read event on client connection
		} elsif(my $pty = $self->{'sess'}{$rdy}{'pty'}) {
		    # Read from telnet client
		    my $c = $self->readnvt($rdy);
		    if(ord($c) && $c ne "") {
			# Relay to the pseudo terminal
			syswrite($pty, $c);
		    # If result was undef, client disconnected
		    } elsif(!defined($c)) {
			# Close telnet session
			$self->close($rdy, $pty);
		    }
		# Read event on pty
		} elsif(my $nvt = $self->{'sess'}{$rdy}{'nvt'}) {
		    my $buff;
		    # Read from pseudo terminal
		    my $n = sysread($rdy, $buff, 8192);
		    # If we haven't received EOF or EOT,
		    if($n && $buff !~ /\x04$/) {
			# Relay to the telnet client
			syswrite($nvt, $buff, $n);
		    # If we received EOF or EOT
		    } else {
			# Close telnet session
			$self->close($nvt, $rdy);
		    }
#		} else {
#		    print "Unknown descriptor ready\n";
		}
	    }
	}
    }

    # Remove listener from the poll() monitor and close
    $self->{'poll'}->remove($listen);
    close($listen);

    # Close PTYs and matching NVTs (if any)
    foreach my $pty (@{$self->{'pty'}}) {
	$self->close($self->{'sess'}{$pty}{'nvt'}, $pty);
    }

    # Destroy poll() object
    undef $self->{'poll'};

    exit(0);
}

sub listen($) {
    my $self = shift;
    my $listen;

    my $host = $self->{'localaddr'};
    my $port = $self->{'localport'};

    my $address = ($host =~ /^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$/) ?
		    inet_aton($host):(($host =~ /[a-zA-Z\-\.]+/) ? 
		    gethostbyname($host):($host ? $host:0));
    return undef unless defined($address);
    # Create listener socket
    socket($listen, PF_INET, SOCK_STREAM, getprotobyname("tcp"))
	or return undef;
    # Allow reusing source address
    setsockopt($listen, SOL_SOCKET, SO_REUSEADDR, 1)
	or return undef;
    # Prepare sockaddr
    my $inet = sockaddr_in($port, $address);
    # Bind to given address:port
    bind($listen, $inet)
	or return undef;
    # Put socket into listener mode
    listen($listen, SOMAXCONN)
	or return undef;
    # Set non-blocking listener
    $self->setnonblocking($listen)
	or return undef;
    # Add listener to the poll() monitor
    $self->{'poll'}->mask($listen => POLLIN);
    return $listen;
}

sub accept($$) {
    my $self = shift;
    my $listen = shift;
    my $nvt;

    # accept telnet connection
    accept($nvt, $listen) or return undef;
    # Set non-blocking connection
    $self->setnonblocking($nvt) or return undef;
    # Monitor client for read events
    $self->{'poll'}->mask($nvt => POLLIN);

    if($self->{'debug'}) {
	print "Connection accepted\n";
    }

    # Request to do the echoing
    $self->request($nvt, WILL, ECHO);
    # Request to suppress go-ahead
    $self->request($nvt, WILL, SUPPRESS_GO_AHEAD);
    # Tell client not to negotiate line mode
    $self->request($nvt, DONT, LINEMODE);
    # Request that client negotiates window size
    $self->request($nvt, DO, NAWS);
    # Request that terminal speed info can be exchanged
    $self->request($nvt, DO, TERMINAL_SPEED);

    return $nvt;
}
#
#   Input:	1. object reference, passed implicitly
#		2. telnet connection socket
#
#   Output:	1: character, if data received
#		   0, if telnet escape sequence received
#		   undef, if error
sub readnvt($$) {
    my $self = shift;
    my $nvt = shift;
    my $c;

    # Read char
    sysread($nvt, $c, 1) or return undef;
#    print "getc got ".ord($c)."\n";

    # Interpret As Command escape ?
    if($c eq IAC) {

	my ($cmd, $opt);

	sysread($nvt, $cmd, 1) or return undef;
	sysread($nvt, $opt, 1) or return undef;
#        print "getc got opt ".ord($opt)."\n";

	# Show debug output
	if(defined($DBG_TELCMDS{$cmd}) && 
	    defined($DBG_TELOPTS{$opt})) {
	    $self->debug("Received %s %s",
			 $DBG_TELCMDS{$cmd},

			 $DBG_TELOPTS{$opt});
	}

	# Parse IAC commands
	if($cmd eq WILL) {

	    # Client shouldn't echo
	    if($opt eq ECHO) {
		$self->respond($nvt, DONT, $opt);
	    # Client should SUPPRESS-GO-AHEAD
	    } elsif($opt eq SUPPRESS_GO_AHEAD) {
		$self->respond($nvt, DO, $opt);
	    # We don't want to negotiate LINEMODE
	    } elsif($opt eq LINEMODE) {
		$self->respond($nvt, DONT, $opt);
	    # We want to negotiate window size
	    } elsif($opt eq NAWS) {
		$self->respond($nvt, DO, $opt);
	    # We want to negotiate TERMINAL-SPEED
	    } elsif($opt eq TERMINAL_SPEED) {
		$self->respond($nvt, DO, $opt);
		# Request terminal speed
		$self->negotiate($nvt, TERMINAL_SPEED, SEND);
	    # Dont negotiate the rest
	    } else {
		$self->respond($nvt, DONT, $opt);
	    }

	} elsif($cmd eq WONT) {

	    # Client MUST suppress go-ahead
	    if($opt eq SUPPRESS_GO_AHEAD) {
		return undef;
	    # Agree with client not to negotiate the rest
	    } else {
		$self->respond($nvt, DONT, $opt);
	    }

	} elsif($cmd eq DO) {

	    # We will echo
	    if($opt eq ECHO) {
		$self->respond($nvt, WILL, $opt);
	    # We will suppress go-ahead
	    } elsif($opt eq SUPPRESS_GO_AHEAD) {
		$self->respond($nvt, WILL, $opt);
	    # Reject everything else
	    } else {
		$self->respond($nvt, WONT, $opt);
	    }

	} elsif($cmd eq DONT) {

	    # We will always suppress go-ahead
	    if($opt eq SUPPRESS_GO_AHEAD) {
		return undef;
	    # Agree with client not to do anything else
	    } else {
		$self->respond($nvt, WONT, $opt);
	    }

	} elsif($cmd eq SB) {

	    $self->debug("Received suboption ".$DBG_TELOPTS{$opt});

	    if($opt eq NAWS) {

		sysread($nvt, $c, 6) or return undef;
		my ($width, $height, $iac, $se) = unpack("n2C2", $c);
		if(chr($iac) eq IAC && chr($se) eq SE) {
		    # Set new terminal size for current pty
		    my $winsize = pack("S4", $height, $width, 0, 0);
		    ioctl($self->{'sess'}{$nvt}{'pty'}, TIOCSWINSZ, $winsize);
		    # Signal window change to the parent process
		    kill 'WINCH', $self->{'parent'};
		}

	    } elsif($opt eq TERMINAL_SPEED) {

		sysread($nvt, $c, 32) or return undef;
		my $se = index($c, IAC.SE);
		if($se > 0) {
		    # Extract terminal speeds
		    my ($ospeed, $ispeed) = split(',', substr($c, 1, $se-1));
		    # Set terminal speed on our pty
		    my $termios = POSIX::Termios->new;
		    my $pty = fileno($self->{'sess'}{$nvt}{'pty'});
		    if(defined($termios) && $termios->getattr($pty)) {
			$termios->setospeed($ospeed);
			$termios->setispeed($ispeed);
			$termios->setattr($pty, &POSIX::TCSANOW);
		    }
		}
	    }

	} else {
	    # Error occured
	    return undef;
	}
	# Terminal escape sequence processed
	return "\0";
    }
    # Return character read from NVT
    return $c;
}

sub close($$$) {
    my ($self, $nvt, $pty) = @_;
    if(defined($pty)) {
	# Remove pty from poll
	$self->{'poll'}->remove($pty);
	# If daemon is still supposed to run ...
	if($self->{'run'}) {
	    # ... return pty to the pool
	    push @{$self->{'pty'}}, $pty;
	}
	# Clean up
        delete $self->{'sess'}{$pty};
    }
    if(defined($nvt)) {
	# Remove nvt from poll
	$self->{'poll'}->remove($nvt);
	# Shutdown and close client connection
	shutdown($nvt, SHUT_RDWR);
	close($nvt);
	# Clean up
	delete $self->{'sess'}{$nvt};
	if($self->{'debug'}) {
	    print "client disconnected\n";
	}
    }
}

sub setnonblocking($$) {
    my ($self, $fd) = @_;

    my $flags = fcntl($fd, F_GETFL, 0);
    return fcntl($fd, F_SETFL, $flags | O_NONBLOCK);
}

sub request($$$$) {
    my $self = shift;
    my ($nvt, $cmd, $opt) = @_;
    my $res;

    # Write request to the network virtual terminal
    $res = syswrite($nvt, IAC.$cmd.$opt, 3);
    # Set negotiation status flag
    $self->{'sess'}{$nvt}{'req'}{$cmd}{$opt} = 1;
    # Show debug output
    $self->debug("Sent %s %s", $DBG_TELCMDS{$cmd}, $DBG_TELOPTS{$opt});

    return $res;
}

sub negotiate($$$$) {
    my $self = shift;
    my ($nvt, $opt, $data) = @_;
    my $res;

    # Write request to the network virtual terminal
    syswrite($nvt, IAC.SB.$opt.$data.IAC.SE);
    # Show debug output
    $self->debug("Sent suboption %s", $DBG_TELOPTS{$opt});
}

sub respond($$$$) {
    my $self = shift;
    my ($nvt, $cmd, $opt) = @_;
    my $res;

    my %neg = (
	&DO	=> DONT,
	&DONT	=> DO,
	&WILL	=> WONT,
	&WONT	=> WILL
    );

    # Answer only if we didn't initiate negotiation
    unless($self->{'sess'}{$nvt}{'req'}{$cmd}{$opt} ||
	    $self->{'sess'}{$nvt}{'req'}{$neg{$cmd}}{$opt}) {
	# Write response to the network virtual terminal
	$res = syswrite($nvt, IAC.$cmd.$opt, 3);
	# Show debug output
	$self->debug("Responded %s %s", $DBG_TELCMDS{$cmd}, $DBG_TELOPTS{$opt});
    }
    # Reset negotiation status flag
    $self->{'sess'}{$nvt}{'req'}{$cmd}{$opt} = 0;
    $self->{'sess'}{$nvt}{'req'}{$neg{$cmd}}{$opt} = 0;
    # Set or reset appropriate negotiated options
    if($cmd eq DO || $cmd eq WILL) {
	$self->setopt($nvt, $opt);
    } elsif($cmd eq DONT || $cmd eq WONT) {
	$self->resetopt($nvt, $opt);
    }
    return $res;
}

sub setopt($$$) {
    my $self = shift;
    my ($nvt, $opt) = @_;

    $self->{'sess'}{$nvt}{'opt'}{$opt} = 1;
}

sub resetopt($$$) {
    my $self = shift;
    my ($nvt, $opt) = @_;

    $self->{'sess'}{$nvt}{'opt'}{$opt} = 0;
}

sub isopt($$$$) {
    my $self = shift;
    my ($nvt, $opt) = @_;

    return (defined($self->{'sess'}{$nvt}{'opt'}{$opt}) &&
	    $self->{'sess'}{$nvt}{'opt'}{$opt}) ? 1:0;
}

sub debug($$$$$) {
    my $self = shift;

    return unless $self->{'debug'};

    printf(shift()."\n", @_);
}
1;
__END__
