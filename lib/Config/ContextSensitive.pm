# Config::ContextSensitive.pm
#
# Copyright (c) 2019 Marko Dinic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#


#############################################################################################
######################### P A C K A G E / E X P O R T   S T U F F ###########################

package Config::ContextSensitive;

require Exporter;

our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

$VERSION = '0.04';
@ISA = qw(Exporter);
@EXPORT = qw(	CF_NONE
		CF_SECTION
		CF_SECTION_NAME
		CF_MAPPED
		CF_ARRAY
		CF_CONST
		CF_EXPR
		CF_LINE
		CF_STRING
		CF_PATH
		CF_BOOLEAN
		CF_INTEGER
		CF_REAL
		CF_INET
		CF_INET6
		CF_ADDR
		CF_PREFIX
		CF_PORT
		CF_FQDN
		CF_ATOI
		CF_NONZERO
		CF_POSITIVE
		CF_NEGATIVE
		TT_SECTION
		TT_DIRECTIVE
		TT_VALUE
		TT_OPER
		S_OP_NOOP
		S_OP_REQUIRE
		S_OP_ALLOW
		S_OP_OPTIONAL
		D_OP_NOOP
		D_OP_HIDDEN
		D_OP_GLOBAL
		D_OP_NONEXTARG
		V_OP_NOOP
		V_OP_NONEXTARG
		O_OP_NOOP
		O_OP_DELETE
		O_OP_MODIFY
		O_OP_STORE
		O_OP_STOREONCE
		O_OP_OVERWRITE
		O_OP_RELATIVE
);
@EXPORT_OK = qw(SECTION_NAME
		SECTION
		REQUIRE
		ALLOW
		OPTIONAL
		DEPS
		DIRECTIVE
		HIDDEN
		GLOBAL
		CATCHALL
		IF
		DELETE
		ARG
		SKIP
		MAP
		OPER
		STORE
		STOREONCE
		OVERWRITE
		FROM
		TO
		KEY
		MODIFY
		POSTPARSER
		DEFAULT
		HELP
);
%EXPORT_TAGS = ('macros' => [ @EXPORT, @EXPORT_OK ]);


#############################################################################################
################################### C O N S T A N T S #######################################

# Allowed expression operators for safe evals
use constant EXPR_OP_ALLOWED		=> qw(
    null scalar const lineseq leaveeval

    rv2sv rv2cv rv2av rv2hv padsv padav padhv padany
    aelem helem

    int hex oct abs pow ord chr

    multiply i_multiply divide i_divide modulo i_modulo add i_add subtract i_subtract
    left_shift right_shift bit_and bit_xor bit_or negate i_negate

    flip flop and or xor not complement
    lt i_lt gt i_gt le i_le ge i_ge eq i_eq ne i_ne ncmp i_ncmp slt sgt sle sge seq sne scmp

    uc lc match concat split subst qr
);
# IOCTL constants from asm/ioctls.h
use constant {
    TIOCGWINSZ				=> 0x5413,		# ioctl() get terminal window size
    TIOCSWINSZ				=> 0x5414		# ioctl() set terminal window size
};
# Known configuration value types
use constant {
    CF_NONE				=> 0x00000000,
    CF_SECTION				=> 0x00000001,
    CF_SECTION_NAME			=> 0x00000002,
    CF_MAPPED				=> 0x00000004,
    CF_ARRAY				=> 0x00000008,
    CF_CONST				=> 0x00000010,
    CF_EXPR				=> 0x00000020,
    CF_LINE				=> 0x00000040,
    CF_STRING				=> 0x00000080,
    CF_PATH				=> 0x00000100,
    CF_BOOLEAN				=> 0x00000200,
    CF_REAL				=> 0x00000400,
    CF_INTEGER				=> 0x00000800,
    CF_INET				=> 0x00001000,
    CF_INET6				=> 0x00002000,
    CF_ADDR				=> 0x00004000,
    CF_PREFIX				=> 0x00008000,
    CF_PORT				=> 0x00010000,
    CF_FQDN				=> 0x00020000,

    CF_ATOI				=> 0x10000000,
    CF_NONZERO				=> 0x20000000,
    CF_POSITIVE				=> 0x40000000,
    CF_NEGATIVE				=> 0x80000000
};
# Known template types
use constant {
    TT_SECTION				=> 0x00000001,		# This template is a section template
    TT_DIRECTIVE			=> 0x00000002,		# This template is a directive template
    TT_VALUE				=> 0x00000004,		# This template is a value template
    TT_OPER				=> 0x00000008		# This template is a operation template
};
# Known section operators
use constant {
    S_OP_NOOP				=> 0x00000000,		# do nothing
    S_OP_REQUIRE			=> 0x00000001,		# directives in this section are mandatory
    S_OP_ALLOW				=> 0x00000002,		# directives in this section are not mandatory
    S_OP_OPTIONAL			=> 0x00000004		# directives in this section are optional
};
# Known directive operators
use constant {
    D_OP_NOOP				=> 0x00000000,		# do nothing
    D_OP_HIDDEN				=> 0x00000001,		# this directive must never appear in the configuration,
								# but it's defaults can be parsed and used
    D_OP_GLOBAL				=> 0x00000002,		# this is a global directive, so the section context
								# should be restored once this directive has been parsed
    D_OP_NONEXTARG			=> 0x40000000		# don't increase arg index after parsing directive name
};
# Known value operators
use constant {
    V_OP_NOOP				=> 0x00000000,		# do nothing
    V_OP_NONEXTARG			=> 0x40000000,		# don't increase arg index after parsing argument
};
# Known operations operators
use constant {
    O_OP_NOOP				=> 0x00000000,		# do nothing
    O_OP_DELETE				=> 0x00000001,		# delete target hash entry or entire hash
    O_OP_MODIFY				=> 0x00000002,		# modify values themselves
    O_OP_STORE				=> 0x00000004,		# store if not defined already, complain otherwise
    O_OP_STOREONCE			=> 0x00000008,		# store if not defined already, skip otherwise
    O_OP_OVERWRITE			=> 0x00000010,		# store in any case
    O_OP_RELATIVE			=> 0x80000000		# destination entry is relative to the current section
};
# Cfline 'struct' fields
use constant {
    CF_CONF_HEAD			=> 0,			# list head (first cfline ref)
    CF_CONF_TAIL			=> 1,			# list tail (last cfline ref)
    CF_CONF_INDEX			=> 2,			# list index (hash ref)

    CF_LINE_PREV			=> 0,			# next cfline list node ref
    CF_LINE_NEXT			=> 1,			# prev cfline list node ref
    CF_LINE_FILE			=> 2,			# conf file name ref
    CF_LINE_NUM				=> 3,			# conf file line num
    CF_LINE_ARGV			=> 4,			# conf line argument array ref
};
# Include recursion path tree constants
use constant {
    T_NAME				=> 0,			# tree node name
    T_PREV				=> 1,			# tree node parent ref
    T_DEPTH				=> 2			# current tree depth
};
# Section hierarchy context constants
use constant {
    CX_TEMPLATE				=> 0,			# hash reference to section template
    CX_NAME				=> 1			# section name
};
# Known section names
use constant {
    CF_SECTION_TOP			=> "TOP"
};
# Known context sensitive action sources
use constant {
    CSA_EOL				=> 0x00000001,
    CSA_DIRECTIVE			=> 0x00000002,
    CSA_ARGUMENT			=> 0x00000004,
    CSA_CONDITIONAL			=> 0x00000008
};
# ASCII special chars
use constant {
    NUL					=> "\x00",
    SOH					=> "\x01",
    STX					=> "\x02",
    ETX					=> "\x03",
    EOT					=> "\x04",
    ENQ					=> "\x05",
    ACK					=> "\x06",
    BEL					=> "\x07",
    BS					=> "0x08",
    TAB					=> "\x09",
    LF					=> "\x0a",
    VT					=> "\x0b",
    FF					=> "\x0c",
    CR					=> "\x0d",
    SO					=> "\x0e",
    SI					=> "\x0f",
    DLE					=> "\x10",
    DC1					=> "\x11",
    DC2					=> "\x12",
    DC3					=> "\x13",
    DC4					=> "\x14",
    NAK					=> "\x15",
    SYN					=> "\x16",
    ETB					=> "\x17",
    CAN					=> "\x18",
    EM					=> "\x19",
    SUB					=> "\x1a",
    ESC					=> "\x1b",
    FS					=> "\x1c",
    GS					=> "\x1d",
    RS					=> "\x1e",
    US					=> "\x1f",
    DEL					=> "\x7f"
};
# Terminal input sequence mappings
use constant {
    TIS_CTRL_A				=> &SOH,
    TIS_CTRL_E				=> &ENQ,
    TIS_CTRL_C				=> &ETX,
    TIS_CTRL_D				=> &EOT,
    TIS_CTRL_W				=> &ETB,
    TIS_CTRL_K				=> &VT,
    TIS_CTRL_U				=> &NAK,
    TIS_CTRL_L				=> &FF,
    TIS_CTRL_Z				=> &SUB,
    TIS_CTRL_R				=> &DC1,
    TIS_CR				=> &CR,
    TIS_TAB				=> &TAB,
    TIS_BACKSPACE			=> &DEL,
    TIS_DELETE				=> "\x1b[3~",
    TIS_CRSR_UP				=> "\x1b[A",
    TIS_CRSR_DOWN			=> "\x1b[B",
    TIS_CRSR_RIGHT			=> "\x1b[C",
    TIS_CRSR_LEFT			=> "\x1b[D"
};
# Terminal commands
use constant {
    TCMD_CR				=> "\r",
    TCMD_CRLF				=> "\n\r",
    TCMD_CLRRIGHT			=> "\x1b[0K",
    TCMD_CLRLEFT			=> "\x1b[1K",
    TCMD_CLRLINE			=> "\x1b[2K",
    TCMD_CLRSCR				=> "\x1b[2J",
    TCMD_TOP				=> "\x1b[H",
    TCMD_UP				=> "\x1b[1A",
    TCMD_DOWN				=> "\x1b[1B",
    TCMD_RIGHT				=> "\x1b[1C",
    TCMD_LEFT				=> "\b"
};
# Terminal input handlers
our %INPUT_EVENT = (
    &TIS_CRSR_LEFT			=> \&input_cursorleft,
    &TIS_CRSR_RIGHT			=> \&input_cursorright,
    &TIS_CRSR_UP			=> \&input_historyprev,
    &TIS_CRSR_DOWN			=> \&input_historynext,
    &TIS_DELETE				=> \&input_delete,
    &TIS_BACKSPACE			=> \&input_backspace,
    &TIS_CTRL_A				=> \&input_linestart,
    &TIS_CTRL_E				=> \&input_lineend,
    &TIS_CTRL_D				=> \&input_delete,
    &TIS_CTRL_C				=> \&input_interrupt,
    &TIS_CTRL_W				=> \&input_replaceword,
    &TIS_CTRL_K				=> \&input_clrtoeol,
    &TIS_CTRL_U				=> \&input_clrline,
    &TIS_CTRL_L				=> \&input_clrscreen,
    &TIS_CTRL_R				=> \&input_noop,
    &TIS_TAB				=> \&input_completion,
    &TIS_CR				=> \&input_linefeed,
    '?'					=> \&input_help,
    "'"					=> \&input_singlequotes,
    '"'					=> \&input_doublequotes,
    "\\"				=> \&input_backslash
);

#############################################################################################
###################################### G L O B A L S ########################################

use POSIX;
use Cwd 'abs_path';
use Safe qw(:default);
use IO::Poll qw(POLLIN);

use Term::ReadKey;

#use IO::Pager::Buffered;

# We use symbolic references,
# so, no strict refs here
use warnings;
use strict;
no strict qw(refs);

# These are all declared local in their respective subs
# to provide persistence through recursion levels and
# save us some parameter juggling.
our (%global_dest_hash, %map_hash);
our ($nested_section_name, $nested_section_template);
our ($argv, $argi, $argl);
our @evar;

#############################################################################################
############################ O B J E C T   I N T E R F A C E ################################

#
# Create new configuration object.
#
#  This is the initial function the programmer must call.
#  Once the object is created, the configuration file
#  can be loaded at programmers convenience. The same
#  template object can parse any configuration file
#  that conforms to the same template.
#
#   Input:	1. class name, passed implicitly
#		   when user code ivokes classname->new
#		2. hash reference to the top section
#		   template.
#
#   Output:	1. object reference, on success
#		2. undef, on error
#
sub new($$;$$) {
    my $class = shift;
    my $template = shift;

    my $self = {};

    # Bless this hash into object,
    # (oh, Perl, almighty ...)
    bless $self, $class;

    # Safe object for expression evaluation
    # using restricted set of operators
    my $eval =  new Safe;
    $eval->permit_only(EXPR_OP_ALLOWED);
    $self->{'EVAL'} = $eval;

    # Prepare hash for dependency tree.
    $self->{'DEPS'}			= {};
    # Prepare hash for tracking ttys in use.
    $self->{'TTYS_INUSE'}		= {};
    # Prepare hash for WINCH signaling.
    $self->{'WINCH'}			= {};

    # This is the default template
    my $default_directive_template = {
	'template' => TT_DIRECTIVE,
	'arg' => {
	    'template' => TT_VALUE,
	    'type' => CF_STRING|CF_SECTION_NAME,
	    'ops' => {
		'template' => TT_OPER,
		'op' => O_OP_STORE|O_OP_RELATIVE,
		'hash' => 'CONFIG',
		'key' => { '$DIRECTIVE' => '$NESTED_SECTION' }
	    }
	}
    };
    my $default_config_template = {
	'template' => TT_SECTION,
	'dirs' => {
	    '/.*/' => $default_directive_template
	}
    };
    $default_directive_template->{'opt'} = {
	'template' => TT_SECTION,
	'dirs' => {
	    '{' => { 'section' => $default_config_template },
	    '/.*/' => $default_directive_template
	}
    };

    $self->{'DEFAULT_TEMPLATE'}	= $default_config_template;

    # Do we have a configuration template ?
    if(defined($template)) {

	# This hash will hold private instance data for the object
	$self->{'CONFIG_TEMPLATE'}	= $template;
	$self->{'INTERNAL_DIRECTIVES'}	= {
					    'INCLUDE_FILE'	=> 'include_file',
					    'INCLUDE_DIRECTORY'	=> 'include_dir',
					    'ONCE'		=> 'once',
					    'END_SECTION'	=> 'exit'
					  };
	$self->{'DESTINATIONS'}		= {};

    # If no configuration template was given,
    # we will use the default one
    } else {

	# This hash will hold private instance data for the object
	$self->{'CONFIG_TEMPLATE'}	= $default_config_template;
	$self->{'INTERNAL_DIRECTIVES'}	= {
					    'INCLUDE_FILE'	=> 'include_file',
					    'INCLUDE_DIRECTORY' => 'include_dir',
					    'ONCE'		=> 'once',
					    'END_SECTION'	=> '}'
					  };
	$self->{'DESTINATIONS'}		= {
					    'CONFIG'		=> {}
					  };

    }

    # Set WINCH signal handler for CLI window size changes
    $SIG{WINCH} = sub {
	# Set 'semaphore' for SIGWINCH.
	# TTYs will adjust their winsize if
	# they find this to be non-zero. Once
	# they adjust the window size.
	my @ttys = keys %{$self->{'TTYS_INUSE'}};
	@{$self->{'WINCH'}}{@ttys} = @ttys;
    };

    return $self;
}
#
# Change default directive names for internal directives.
#
#   Input:	1. object reference, passed implicitly
#		2. directive names hash (key => value):
#		    'INCLUDE_FILE' => custom keyword for this directive
#		    'INCLUDE_DIRECTORY' => custom keyword for this directive
#		    'ONCE' => custom keyword for this directive
#		    'END_SECTION' => custom keyword for this directive
#
#   Output:	1. TRUE, on success,
#		   FALSE, on error
#
sub internal_directive_names($%) {
    my $self = shift;
    my $directives = {@_};

    while(my ($directive, $name) = each %{$directives}) {
	return 0 unless(defined($self->{'INTERNAL_DIRECTIVES'}{$directive}));
	$self->{'INTERNAL_DIRECTIVES'}{$directive} = $name;
    }
    return 1;
}
#
# Assign a hash (by reference) to a named destination.
#
#  If global hashes are used as destinations, the configuration
#  template directly references them by name. If we don't want
#  to use global hashes, we need to assign a hash to every named
#  destination. In that case, the destination hash names will 
#  indirectly reference assigned hashes.
#
#  This function will assign a hash reference to the named 
#  destination by creating a destination_name => hash_reference
#  relationship and storing it into the object instance. The name
#  of the destination obviosly doesn't have to match the actual
#  name of the hash, unlike when using global hashes. Passed hash
#  reference can be an anonymous hash.
#
#  If hash reference is not provided, a new anonyomus hash will
#  be created for every named destination. A hash reference is
#  retrieved when needed, using get method.
#
#   Input:	1. object reference, passed implicitly
#		2. a name by which the destination hash
#		   will be identified by the configuration
#		   template.
#		3. optional reference to the destination hash
#
#   Output:	none
#
sub assign_destination($$;$) {
    my ($self, $dest_name, $hash_ref) = @_;

    # Destination name is a mandatory parameter
    unless(defined($dest_name) && $dest_name ne "") {
	die "Config template error: assign method requires a destination name\n";
    }
    # If hash reference is not provided,
    # we will create an anonymous hash
    unless(defined($hash_ref)) {
	$hash_ref = {};
    } elsif(ref($hash_ref) ne "HASH") {
	die "Config template error: assign method expects second argument to be a hash reference\n";
    }

    # Store hash reference into our object instance
    $self->{'DESTINATIONS'}{$dest_name} = $hash_ref;
}
#
# Retrieve a destination hash reference.
#
#  If anonymous hashes are assigned to named destinations,
#  we can retrieve references to them using this method.
#
#   Input:	1. object reference, passed implicitly
#		2. a name by which the destination hash
#		   is identified by the configuration
#		   template.
#
#   Output:	1. a reference to the destination hash, or
#		   undef, if requested hash doesn't exist.
#
sub get_destination($$) {
    my ($self, $dest_name) = @_;

    unless(defined($dest_name) && $dest_name ne "") {
	die "Config template error: retrieve method requires a destination hash name\n";
    }

    return $self->{'DESTINATIONS'}{$dest_name};
}
#
# Load, parse and commit configuration.
#
#  This is the function the user code invokes whenever
#  it needs to fully load, parse and apply configuration
#  from a file.
#
#  It is simply a batch of loadonly and commit methods
#  that do the actual work. Former loads configuration
#  from a file and parses it, and latter commits it.
#
#  Being the object method, the only implicit parameter
#  it receives is the top configuration file name.
#
#   Input:	1. object reference, passed implicitly
#		2. file name of the top configuration file
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#
sub load($$) {
    my ($self, $config_file) = @_;

    # Remember our caller's namespace
    $self->{'CALLER'} = caller();

    # Prepare private storage for loading phase
    $self->{'CONFIG'} = [];
    $self->{'INCLUDED_FILES'} = {};

    # Load top configuration file. Files included
    # from this one are loaded recursively.
    $self->load_config_file($config_file, undef)
	or return 0;

    # Destroy private storage data to release memory
    undef %{$self->{'INCLUDED_FILES'}};
    delete $self->{'INCLUDED_FILES'};

    # Prepare private storage for parsing phase
    $self->{'WORK'} = {};
    $self->{'SECTION_HIERARCHY'} = [];

    # If we have a non-default configuration template,
    unless($self->{'CONFIG_TEMPLATE'} == $self->{'DEFAULT_TEMPLATE'}) {
	# create storage space for
	# configuration defaults
	$self->{'DEFAULTS'} = {};
    }

    # prepare configuration defaults.
    $self->section_defaults($self->{'CONFIG_TEMPLATE'});

    # Start parsing from the top level section
    my ($res, $argv) = $self->parse_section;

    # Destroy private storage data to release memory
    undef @{$self->{'SECTION_HIERARCHY'}};
    undef $self->{'SECTION_HIERARCHY'};
    undef @{$self->{'CONFIG'}};
    undef $self->{'CONFIG'};

    #
    # - If everything went well, $res will be TRUE
    # - If unknown directive was encountered, $res will be undef
    #   (returned cfline reference is ignored at this point).
    #
    return 0 unless $res;

    # Commit newly loaded configuration
    $self->commit;

    return 1;
}
#
# Load and parse configuration.
#
#  This is the function the user code invokes whenever it
#  needs just to load configuration without commiting it.
#  Configuration can be split into multiple configuration
#  files that include one another.
#
#  When parsed, configuration is stored into a temporary hash.
#  In order to be applied to its final destination, commit()
#  method must be called.
#
#  Being the object method, the only explicit parameter it
#  receives is the top configuration file name.
#
#   Input:	1. object reference, passed implicitly
#		2. file name of the top configuration file
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#
sub loadonly($$) {
    my ($self, $config_file) = @_;

    # Remember our caller's namespace
    $self->{'CALLER'} = caller();

    # Prepare private storage for loading phase
    $self->{'CONFIG'} = [];
    $self->{'INCLUDED_FILES'} = {};

    # Load top configuration file. Files included
    # from this one are loaded recursively.
    $self->load_config_file($config_file, undef)
	or return 0;

    # Destroy private storage data to release memory
    undef %{$self->{'INCLUDED_FILES'}};
    delete $self->{'INCLUDED_FILES'};

    # Prepare private storage for parsing phase
    $self->{'WORK'} = {};
    $self->{'SECTION_HIERARCHY'} = [];

    # If we have a non-default configuration template,
    unless($self->{'CONFIG_TEMPLATE'} == $self->{'DEFAULT_TEMPLATE'}) {
	# create storage space for
	# configuration defaults
	$self->{'DEFAULTS'} = {};
    }

    # Prepare configuration defaults.
    $self->section_defaults($self->{'CONFIG_TEMPLATE'});

    # Start parsing from the top level section
    my ($res, $argv) = $self->parse_section;

    # Destroy private storage data to release memory
    undef @{$self->{'SECTION_HIERARCHY'}};
    undef $self->{'SECTION_HIERARCHY'};
    undef @{$self->{'CONFIG'}};
    undef $self->{'CONFIG'};

    #
    # - If everything went well, $res will be TRUE
    # - If unknown directive was encountered, $res will be undef
    #   (returned cfline reference is ignored at this point).
    #
    return $res;
}
#
# Enter CLI
#
#  If mode is dontblock, this function prepares tty for
#  command line mode and displays the initial prompt.
#  If mode is blocking (the default), after preparing
#  the tty, it enters the command processing loop and
#  doesn't return unless our object's 'run' flag goes
#  down by whatever means.
#
#   Input:	1. object reference, passed implicitly
#		2. optional parameters:
#		   'tty'	=> TTY to attach CLI to
#		   'dontblock'	=> run CLI as nonblocking
#		   'prompt'	=> initial CLI prompt
#
#   Output:	1. reference to cli object, on success
#		2. undef, on error or logout
#
sub cli($;%) {
    my $self = shift;
    my $opts = {@_};

    # Our TTY file handle.
    # If not supplied, use STDIN.
    my $fh  = defined($opts->{'tty'}) ? 
		     (isdigit($opts->{'tty'}) ?
		     $opts->{'tty'}:(index($opts->{'tty'},"::") > -1 ?
		     $opts->{'tty'}:caller().'::'.$opts->{'tty'})):\*STDIN;

    # Verify that file handle is a TTY.
    return undef unless isatty($fh);

    # Reopen TTY in read/write mode.
    open(my $tty, ">&=", fileno($fh))
	or return undef;

    # We will create a new instance of
    # our class for each spawned CLI.
    my $cli = {

	%{$self},			# Internal data from our sibling

	'LINE_BUFFER'	=> '',		# current line
	'LINE_LEN'	=> 0,		# current line length
	'LINE_PTR'	=> 0,		# current line buffer pointer

	'CURSOR_X'	=> 0,		# current cursor position
	'INPUT_WIDTH'	=> 0,		# input area width

	'FLAG_CLI_UP'	=> 1,		# CLI is up and running
	'FLAG_CSA'	=> 0,		# context sensitive action pending
	'FLAG_SQ'	=> 0,		# single quotes flag
	'FLAG_DQ'	=> 0,		# double quotes flag
	'FLAG_ESC'	=> 0,		# backslash escape flag
	'FLAG_HELP'	=> 0,		# context sensitive help flag
	'FLAG_COMPLETE'	=> 0,		# command completion flag
	'FLAG_TSPACES'	=> 0,		# trailing spaces flag

	'HISTORY'	=> [],		# history buffer
	'HIST_INDEX'	=> 0,		# current history index

	'TTY'		=> $tty, 

	'DONTBLOCK'	=> defined($opts->{'dontblock'}) ?
			    $opts->{'dontblock'}:0,

	'PROMPT'	=> defined($opts->{'prompt'}) ?
			    $opts->{'prompt'}.' ':'> ',

	'PROMPT_LEN'	=> defined($opts->{'prompt'}) ?
			    (length($opts->{'prompt'}) + 1):2
    };

    # Maintain list of used TTYs
    $self->{'TTYS_INUSE'}{$cli->{'TTY'}} = 1;

    # Bless this anon hash into object
    bless $cli, ref($self);
    $self = $cli;

    # Prepare private storage for parsing phase
    $self->{'WORK'} = {};
    $self->{'SECTION_HIERARCHY'} = [];

    # Set ultra-raw terminal mode
#    my $ioctl = ioctl($tty, 0, 0);
#    $ioctl = 0 unless defined($ioctl);
#    $self->{'TERMINAL_MODE_SAVE'} = $ioctl;
#    ioctl($tty, 1, (($ioctl&0xff)|0x20));
    ReadMode 5, $tty;

    # Get initial terminal window size
    $self->term_resetsize;
    # Display banner if defined
    if(defined($opts->{'banner'}) &&
       $opts->{'banner'} ne '') {
	$self->display($opts->{'banner'}."\n");
    } else {
	# Display prompt
	$self->term_prompt;
    }

    # If we are in nonblocking mode,
    # don't enter processing loop
    if($self->{'DONTBLOCK'}) {
	# Return reference to the new CLI object
	return $self;
    }

    # Create poll() object
    my $poll = IO::Poll->new();

    # Monitor input channel for the read readiness
    $poll->mask($self->{'TTY'} => POLLIN);

    while($self->is_cli) {
	# Wait on read events
	if($poll->poll(0.01) > 0) {
	    # process char by char
	    $self->process;
	}
    }

    # Remove input channel from poll()
    $poll->remove($self->{'TTY'});
    # Destroy poll() object
    undef $poll;

    # Destroy the CLI object
    undef %{$self};

    return undef;
}
#
# Enter new section context.
#
#   Input:	1. object reference, passed implicitly
#		2. a hash reference to the section template
#		3. optional section name
#
#   Output:	none
#
sub section_context_enter($$;$) {
    my $self = shift;
    my ($section_template, $section_name) = @_;

    # Add the section template and name to the recursion path.
    # Section recursion path preserves information about current
    # and upper level contexts which define section hierarchy.
    #
    # Each context 'structure' is a 2-element array:
    #  1. element: a hash reference to the section template
    #  2. optional section name or undef.
    push @{$self->{'SECTION_HIERARCHY'}}, [$section_template, $section_name];
}
#
# Exit current section and go one context 'up'.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. TRUE, on success
#		   FALSE, if we were in the topmost section
#
sub section_context_up($) {
    my $self = shift;

    # If we are inside a section, remove section
    # template and name from the section hierarchy
    if($self->{'SECTION_HIERARCHY'}[$#{$self->{'SECTION_HIERARCHY'}}]) {
	pop @{$self->{'SECTION_HIERARCHY'}};
	return 1;
    }
    return 0;
}
#
# Exit current section and go to the topmost context.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. TRUE, on success
#		   FALSE, if we already were at the top
#
sub section_context_top($) {
    my $self = shift;

    # If we are inside a section, clear hierarchy
    # which effectively moves us to the top
    if($self->{'SECTION_HIERARCHY'}[$#{$self->{'SECTION_HIERARCHY'}}]) {
	@{$self->{'SECTION_HIERARCHY'}} = ();
	return 1;
    }
    return 0;
}
#
# Get current section context.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. an array reference to the section context 'structure'
#
sub section_context_get($) {
    my $self = shift;

    # Return current section context
    return ($self->{'SECTION_HIERARCHY'}[$#{$self->{'SECTION_HIERARCHY'}}]) ?
		$self->{'SECTION_HIERARCHY'}[$#{$self->{'SECTION_HIERARCHY'}}]:
		[$self->{'CONFIG_TEMPLATE'}, CF_SECTION_TOP];
}
#
# Get parent section context.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. an array reference to the section context 'structure'
#
sub section_context_parent($) {
    my $self = shift;

    # Return parent section context
    return ($#{$self->{'SECTION_HIERARCHY'}}) ?
		$self->{'SECTION_HIERARCHY'}[$#{$self->{'SECTION_HIERARCHY'}}-1]:
		[$self->{'CONFIG_TEMPLATE'}, CF_SECTION_TOP];
}
#
# Save current section hierarchy.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	none
#
sub section_context_save($) {
    my $self = shift;
    # Save current section hierarchy
    $self->{'SECTION_HIERARCHY_SAVED'} = [@{$self->{'SECTION_HIERARCHY'}}];
}
#
# Restore saved section hierarchy.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. TRUE, if hierarchy was restored
#		   FALSE, if nothing happened
#
sub section_context_restore($) {
    my $self = shift;

    if(defined($self->{'SECTION_HIERARCHY_SAVED'}) &&
	ref($self->{'SECTION_HIERARCHY_SAVED'}) eq "ARRAY") {
	$self->{'SECTION_HIERARCHY'} = $self->{'SECTION_HIERARCHY_SAVED'};
	delete $self->{'SECTION_HIERARCHY_SAVED'};
	return 1;
    }
    return 0;
}
#
# Discard saved section hierarchy.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. TRUE, if saved hierarchy was discarded
#		   FALSE, if nothing happened
#
sub section_context_discard_saved($) {
    my $self = shift;

    if(defined($self->{'SECTION_HIERARCHY_SAVED'})) {
	delete $self->{'SECTION_HIERARCHY_SAVED'};
	return 1;
    }
    return 0;
}
#
# Exit CLI session.
#
#  This function flags the end of the CLI command loop
#  and CLI command processing in general and restores
#  original tty parameters.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	none
#
sub end($) {
    my $self = shift;

    # Flag the end of the processing loop
    $self->{'FLAG_CLI_UP'} = 0;
    # Position cursor before logout
    $self->term_cr;
    $self->term_clrtoeol;
    # Send EOT
    $self->term_logout;
    # Restore original terminal mode
#    ioctl($self->{'TTY'}, 1, $self->{'TERMINAL_MODE_SAVE'});
    ReadMode 0, $self->{'TTY'};
    # Remove tty from list of ttys in use
    delete $self->{'TTYS_INUSE'}{$self->{'TTY'}};
    delete $self->{'WINCH'}{$self->{'TTY'}};
}
#
# Process CLI input.
#
#  This function reads and processes input from tty character
#  by character. A-character-at-a-time operation is neccessary
#  for context sensitive CLI features as well as to enable
#  applications to multiplex CLI input with other I/O operations.
#  Special characters and escape sequences are read and interpreted
#  in a single pass, as terminal commands, while the rest is read
#  and accumulated in the line buffer until the line feed, which 
#  marks the end of the line, is received. Complete lines are then
#  processed as if they were read from the configuration file.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		   undef, on CLI logout
#
sub process($) {
    my $self = shift;

    return undef unless $self->is_cli;

    # Should we adjust the terminal window size ?
    if($self->{'WINCH'}{$self->{'TTY'}}) {
	# Reset current tty's window size
	$self->term_resetsize;
	# Remove us from SIGWINCH queue
	delete $self->{'WINCH'}{$self->{'TTY'}};
    }

    # Process input char by char and when
    # current line is complete, parse it.
    my ($res, $resmsg) = $self->process_cli_input;

    # If end() was called from within CLI,
    # session is over, so we will return undef.
    return undef unless $self->is_cli;

    return $res;
}
#
# Format and write string to the terminal
#
#  This function is essentially the same as Perl's printf function.
#  The only difference is that it replaces LF with CRLF sequence
#  for output to the terminal and uses raw write.
#
#   Input:	1. object reference, passed implicitly
#		2. format string
#		3..N format parameters
#
#   Output:	none
#
sub printf($$;@) {
    my $self = shift;
    my $fmt = shift;
    # Replace lf with crlf
    $fmt =~ s/\r?\n\r?/\n\r/g;
    # Format string
    my $line = sprintf($fmt, @_);
    # Write to tty
    syswrite($self->{'TTY'}, $line, length($line));
}
#
# Display given text on the terminal
#
#  This function displays multi-line string on the terminal screen
#  and then puts us back to the blank command line.
#
#   Input:	1. object reference, passed implicitly
#		2. text to display
#		3..N format parameters
#
#   Output:	none
#
sub cat($$;@) {
    my $self = shift;
    my $text = shift;
    # Display text
    $self->printf($text, @_);
    # Redraw prompt
    $self->term_prompt;
}
#
# Display message on the terminal
#
#  This function displays multi-line message on the terminal screen
#  as normal or context sensitive text output, depending on the mode
#  we are currently in.
#
#  In normal (parsing) mode, this function is equivalent to cat() and
#  will assume that we have already moved to the new line as a result
#  of user input and that it should return to an empty line once 
#  the message is displayed. 
#
#  If context sensitive action is in progress, this function will
#  first move output to the new line, display the message and then
#  resume user input in the current line.
#
#   Input:	1. object reference, passed implicitly
#		2. text to display
#		3..N format parameters
#
#   Output:	none
#
sub display($$;@) {
    my $self = shift;
    # If context sensitive action is in progress,
    # we are probably editing the current line.
    if($self->{'FLAG_CSA'}) {
	# First, put us into new line
	$self->term_newline;
    }

    # Display message
    $self->cat(@_);

    # If context sensitive action is in progress,
    # we should get back to editing current line.
    if($self->{'FLAG_CSA'}) {
	# Repaint the current line
	# and restore cursor position
	$self->input_putline;
    }
}
#
# Set CLI Prompt
#
#   Input:	1. object reference, passed implicitly
#		2. new CLI prompt string
#
#   Output:	none
#
sub prompt($$) {
    my ($self, $prompt) = @_;

    $self->{'PROMPT'} = $prompt.' ';
    $self->{'PROMPT_LEN'} = length($prompt) + 1;
}
#
# Commit previosly loaded and parsed configuration.
#
#  This is the function the user code invokes whenever it
#  needs to commit previosly prepared configuration. Pending
#  configuration can be loaded by loadonly() method or input
#  from CLI, or can be taken from some other source and then
#  parsed by methods exposed by this package.
#
#  When parsed, configuration is stored into a temporary hash.
#  This function copies the contents of the temporary hash
#  into the specified destination hash.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	none
#
sub commit($) {
    my $self = shift;

    # Newly parsed configuration was stored in the temporary
    # hash in order to prevent live configuration from being
    # corrupt by changing it partially or incorrectly in case of
    # an error. Temporary hash stores all parsed data even though
    # config template explicitly specifies possibly many destination
    # hashes for various directives. Temporary hash uses final
    # destination hash names as first level keys and its entries
    # are used as work hashes for each destination hash.
    foreach my $dest_hash_name (keys %{$self->{'WORK'}}) {
	# See if we have a destination hash reference
	# explicitly assigned. This way, it can be any
	# hash, global or otherwise, as long as we have
	# a hash reference to it.
	my $dest_hash = $self->{'DESTINATIONS'}{$dest_hash_name};
	# If the destination hash reference was not defined,
	# we will use a global hash.
	unless(defined($dest_hash)) {
	    # Hash name can be fully qualified, with namespace prefix,
	    # or relative to the caller, in which case we have to
	    # prepend caller's namespace to the hashname
	    local *global_dest_hash = *{$self->typeglob_qualify($dest_hash_name)};
	    # If the destination hash doesn't exist
	    # in caller's namespace, we will assume
	    # it is a global hash within main::
	    local *global_dest_hash = *{'::'.$dest_hash_name} unless %global_dest_hash;
	    # By copying parsed configuration from temporary hash
	    # into its local hash alias, we actually apply config
	    # to its final global destination hash.
	    #
	    # Empty the destination hash
	    %global_dest_hash = ();
	    # Assign a global hash alias reference
	    # to a common variable
	    $dest_hash = \%global_dest_hash;
	}
	# If configuration defaults are defined,
	if(defined($self->{'DEFAULTS'})) {
	    # apply default values. Each destination hash has
	    # its own entry in the defaults hash
	    &deepcopy_hash($self->{'DEFAULTS'}{$dest_hash_name}, $dest_hash);
	}
	# Then, copy parsed values over. Each destination hash
	# has its own entry in the temporary hash. Each temporary
	# hash entry will now be copied to the actual destination
	# hash.
	&deepcopy_hash($self->{'WORK'}{$dest_hash_name}, $dest_hash);
	# Destroy temporary storage to release memory
	undef %{$self->{'WORK'}{$dest_hash_name}};
	delete $self->{'WORK'}{$dest_hash_name};
    }

}

#
# Dump some or all of the configuration hashes.
#
#  This function returns a string packed (not pack()ed) with 
#  contents of requested destination hashes. It will loop through
#  all the entries and nested hashes and arrays recursively.
#  Indentation in the output should reflect the hierarchy of
#  the hashes.
#
#   Input:	1. object reference, passed implicitly
#		2+ optional, the destination hashes' names
#
#   Output:	1. single, multiline string,
#		   undef, if no output
#
sub show_config($;@) {
    my $self = shift;
    my $dump = "";

    # The list of destination hashes will either be whatever
    # was passed to us as input parameters, or our object's
    # list of destinations, if we were called without params.
    my $destinations = defined($_[0]) ? \@_:[keys %{$self->{'DESTINATIONS'}}];

    # Loop through the list of destinations we need to dump
    foreach my $dest (@{$destinations}) {
	$dump .= "----------------------------------------------------------------\n";
	$dump .= " Configuration hash \'$dest\'\n";
	$dump .= "----------------------------------------------------------------\n";
	# Dump contents of the destination hash
	my $conf = $self->dump_config($dest, $self->{'DESTINATIONS'});
	$dump .= $conf if(defined($conf) && $conf ne "");
    }

    if($dump ne "") {
	$dump .= "----------------------------------------------------------------\n";
    }

    return $dump ne "" ? $dump:undef;
}
#
# Dump some or all of the pending configuration hashes.
#
#  This function returns a string packed (not pack()ed) with
#  contents of requested destination hashes. It will loop through
#  all the entries and nested hashes and arrays recursively.
#  Indentation in the output should reflect the hierarchy of
#  the hashes.
#
#   Input:	1. object reference, passed implicitly
#		2+ optional, the destination hashes' names
#
#   Output:	1. single, multiline string,
#		   undef, if no output
#
sub show_pending($;@) {
    my $self = shift;
    my $dump = "";

    # The list of destination hashes will either be whatever
    # was passed to us as input parameters, or our object's
    # list of destinations, if we were called without params.
    my $destinations = defined($_[0]) ? \@_:[keys %{$self->{'WORK'}}];

    # Loop through the list of destinations we need to dump
    foreach my $dest (@{$destinations}) {
	$dump .= "----------------------------------------------------------------\n";
	$dump .= " Pending configuration hash \'$dest\'\n";
	$dump .= "----------------------------------------------------------------\n";
	# Dump contents of the destination hash
	$dump .= $self->dump_config($dest, $self->{'WORK'});
    }

    if($dump ne "") {
	$dump .= "----------------------------------------------------------------\n";
    }

    return $dump ne "" ? $dump:undef;
}

#############################################################################################
############################# M A I N   P A R S E R   C O D E ###############################

#
# Prepare section defaults.
#
#  This method prepares default values for arguments within
#  a single section. Defaults are applied only when the context
#  to which they are bound exists. Default values for arguments
#  belonging to a section are prepared only when parser creates
#  that section's context. This is because multiple sections
#  can be instantiated from a single section template. The number
#  and names of these instances cannot be predicted - they become
#  known only while parsing the actual configuration and only
#  then can these defaults be parsed and prepared.
#
#  Similarly, subdirectives' sections and their default argument
#  values are parsed and used only after their contexts have been
#  created. That happens only during actual configuration line
#  parsing, when some particular subdirective is referenced, which
#  results in the creation of that subdirective's context.
#
#  All this implies that you cannot simply set default values to
#  all arguments defined in the configuration template and expect
#  to see them applied in the resulting configuration. Because
#  configurations can be defined as context sensitive on various
#  levels and possibly use conditional directives and/or sections,
#  some contexts will never be created. We cannot and (in case some
#  contexts are ment to be mutualy exclusive) should not prepare
#  and apply default argument values within all potential contexts,
#  only those that will actually 'see the light of day'.
#
#  Clearly, default argument values are optional and they can be
#  defined for any value template that requires them. We define them
#  as strings in the 'default' entry of a value template. They have
#  the same format as they would in the configuration file.
#
#   Input:	1. object reference, passed implicitly
#		2. hash reference to the section template
#		3. current section name, if any
#		4. nested_section_name, if any
#
#   Output:	none
#
sub section_defaults($$;$$) {
    my $self = shift;
    my ($section_template, $current_section_name, $nested_section_name) = @_;
    my $errmsg = "Config template error: ";

    return unless defined($self->{'DEFAULTS'});

    # Save current work
    my $work = $self->{'WORK'};
    # We will be updating existing defaults
    $self->{'WORK'} = $self->{'DEFAULTS'};

    my %section_deptree = ();

    # In order to prepare the default configuration
    # we need to loop through all value templates
    # in this configuration template and pick their
    # 'default' settings.
    while(my ($directive_name, $directive_template) = each %{$section_template->{'dirs'}}) {

	my $vtarray;

        # Turn current directive into an array if it's not already.
	if(defined($directive_template->{'arg'})) {
	    if(ref($directive_template->{'arg'}) eq "HASH") {
		$vtarray = [$directive_template->{'arg'}];
	    } elsif(ref($directive_template->{'arg'}) eq "ARRAY") { 
		$vtarray = $directive_template->{'arg'};
	    } else {
		die $errmsg."invalid section template entry at directive \"".$directive_name."\"\n";
	    }
	}

	# Build directive dependency hash.
	if(defined($directive_template->{'deps'})) {
	    # Check if listed dependencies have their directive templates.
	    foreach my $dep (@{$directive_template->{'deps'}}) {
		unless(defined($section_template->{'dirs'}{$dep})) {
		    die $errmsg."directive \"".$dep."\" listed as dependency of \"".$directive_name."\" doesn't exist\n";
		}
	    }
	    # Add dependencies for the current directive to the deptree
	    $self->deptree_push(\%section_deptree,
				$directive_name,
				$directive_template->{'deps'});
	}

	# Loop through all expected arguments for the current directive
	foreach my $argument (@{$vtarray}) {

	    # If current argument is a value template,
	    next unless $argument->{'template'} == TT_VALUE;

	    # This is our default value for this argument
	    my $default = $argument->{'default'};

	    # If default value or a store operation aren't
	    # defined, we can move on to the next argument.
	    next unless(defined($default) &&
			$default ne "" &&
			defined($argument->{'ops'}));

	    # Split the default value for the current
	    # argument into the argument array.
	    local $argv = $self->split_config_line($default);
	    local $argi = 0;
	    local $argl;

	    my ($res, $resmsg, $value);

	    # Value templates of CF_NONE data type are
	    # silently ignored, while others are parsed
	    # the same way the actual configuration file
	    # line arguments are parsed.
	    unless($argument->{'type'} == CF_NONE) {
		# Parse the default value for the current
		# value template.
		($res, $value) = $self->parse_data_type($directive_name,
							$argument,
							$current_section_name,
							$nested_section_name);
		# Catch configuration template errors.
		die $errmsg.$value."\n" unless $res;
	    }

	    # Parsed default value(s) can now be stored
	    # according to specified operation templates
	    my $operation_templates;

	    # Prep array of store operations
	    my $oper_type = ref($argument->{'ops'});
	    if($oper_type eq "HASH") { 
		$operation_templates = [$argument->{'ops'}];
	    } elsif($oper_type eq "ARRAY") { 
		$operation_templates = $argument->{'ops'};
	    } elsif(defined($oper_type)) {
		die $errmsg."invalid value template operation definition at directive \"".$directive_name."\"\n";
	    }

	    # Execute operations in sequence
	    foreach my $operation_template (@{$operation_templates}) {

		# Skip if this operation template specifies
		# delete or modify operation
		next if($operation_template->{'op'} & (O_OP_DELETE|O_OP_MODIFY));

		# Store produced value(s) into work hash.
		$self->store_values($operation_template,
				    $value,
				    $directive_name,
				    $current_section_name,
				    $nested_section_name);

	    }

	}
    }

    # Restore our work
    $self->{'WORK'} = $work;
	
    # Check for cross-dependent directives
    unless($self->deptree_is_looped(\%section_deptree)) {
	die $errmsg."directives cannot be cross-dependent\n";
    }

}
#
# Load configuration file and parse it.
#
#  This function loads any configuration file in the hierarchy.
#  Once the configuration file is open, it fetches line by line,
#  removes comments, assembles multiline directives, splits line
#  into an argument array and stores it into config array for
#  later processing.
#
#  If include directive is encountered, this function calls itself
#  recursively to load nested configuration file or enumerate 
#  entire directory.
#
#  In order to prevent include looping, this function constructs
#  a simple tree of all file inclusions. Each branch represents
#  one recursion path. While each file can be present at multiple
#  tree nodes (can be included multiple times from various files),
#  it must always be unique to every branch (it can show up only
#  once in each recursion path).
#
#   Input:	1. object reference, passed implicitly
#		2. file name
#		3. reference to parent node or undef, if this
#		   is the top level configuration file
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#
sub load_config_file($$;$) {
    my $self = shift;
    my ($config_file, $parent) = @_;
    my $filename;
    my $line;
    my $ln = 0;

    local *CONF;

    # If maximum number of recursion levels has been reached
    # we won't go on. This limit must exist because there is
    # no clean way to test for include loop if hard links are
    # used instead of symlinks.
    if(defined($parent->[T_DEPTH]) && $parent->[T_DEPTH] >= 10) {
	print "Maximum include recursion levels reached.\n";
	return 0;
    }
    # A single config file can be included multiple times from
    # various locations, but it must never be included from the
    # recursion branch in which it already exists, because it
    # cannot descend from itself - it would result in an endless
    # recursion loop (theoretically:).
    #
    # Get absolute path to actual config file (also follows symlinks).
    $filename = abs_path($config_file);
    return 0 unless defined($filename);
    #
    # Examine this branch of include tree - if this filename
    # exists in the list of parents, it has already been included
    # one or more recursion levels above. That means include loop
    # exists, so we cannot go on ...
    for(my $node = $parent;
	defined($node->[T_NAME]) && $node->[T_NAME] ne "";
	$node = $node->[T_PREV]) {
	if($node->[T_NAME] eq $filename) {
	    my $errmsg = "include recursion loop detected: config file \"$filename\" ";
	    if(defined($node->[T_PREV]) && 
	       $node->[T_PREV][T_NAME] ne "") {
		return (0, $errmsg."previosly included from \"$node->[T_PREV][T_NAME]\"");
	    } else {
		return (0, $errmsg."already is the top level config file");
	    }
	}
    }
    #
    # Assuming there is no recursion loop, each time a file is
    # included from its parent file, its full path is added as
    # a leaf node to the include tree.
    my $path_node = [
		      $filename,
		      $parent,
		      defined($parent->[T_DEPTH]) ?
				$parent->[T_DEPTH]+1:1
		    ];
    # Also, filename is added to the list of unique file names.
    # This 'list' is used for quick 'include once' lookup.
    $self->{'INCLUDED_FILES'}{$filename} = 1;

    # Open configuration file
    open(CONF, $filename)
	or return 0;

    while(<CONF>) {

	$ln++;

	# Get rid of newlines, leading and trailing whitespaces and comments
	$_ =~ s/^(\s+)|(\s+)$|(\s*(?<!\\)#.*)$|(\n)//g;
	# Skip empty lines
	next if($_ =~ /^$/);
	# Join multiple log lines ending with '\'
	$line .= $_;
	if($line =~ s/(\\)$//g) {
	    $ln--;
	    next;
	}

	# Split configuration line into argument array
	my $argv = $self->split_config_line($line);

	my $errmsg = "Config file \"$filename\" error on line $ln: ";

	# Is this the internal include_file directive
	# (load designated nested config file) ?
	if($argv->[0] eq $self->{'INTERNAL_DIRECTIVES'}{'INCLUDE_FILE'}) {

	    my $include_file = abs_path($argv->[1]);
	    unless(-f $include_file) {
		print $errmsg."include_file argument must be a file name\n";
		close(CONF);
		return 0;
	    }
	    # If include file directive has an optional argument,
	    # check if it is the correct one.
	    if(defined($argv->[2]) && 
	       $argv->[2] ne "" && 
	       $argv->[2] ne $self->{'INTERNAL_DIRECTIVES'}{'ONCE'}) {
		print $errmsg."unknown argument \"$argv->[2]\"\n";
		close(CONF);
		return 0;
	    }
	    # If include once was specified, file will be included
	    # only if it hasn't been included before.
	    unless(defined($self->{'INTERNAL_DIRECTIVES'}{'ONCE'}) &&
		   $self->{'INTERNAL_DIRECTIVES'}{'ONCE'} ne "" && 
		   defined($argv->[2]) &&
		   $argv->[2] eq $self->{'INTERNAL_DIRECTIVES'}{'ONCE'} &&
		   $self->{'INCLUDED_FILES'}{$include_file}) {
		# Load nested configuration file and parse it
		my ($res, $resmsg) = $self->load_config_file($include_file,
							     $path_node);
		unless($res) {
		    print $errmsg.$resmsg."\n" if(defined($resmsg));
		    close(CONF);
		    return 0;
		}
	    }

	# Is this the internal include_dir directive
	# (load all config files from designated dir) ?
	} elsif($argv->[0] eq $self->{'INTERNAL_DIRECTIVES'}{'INCLUDE_DIRECTORY'}) {

	    my $include_path = abs_path($argv->[1]);
	    unless(-d $include_path) {
		print $errmsg."include_dir argument must be a directory name\n";
		close(CONF);
		return 0;
	    }
	    # If include dir directive has an optional argument,
	    # check if it is the correct one.
	    if(defined($argv->[2]) && defined($self->{'INTERNAL_DIRECTIVES'}{'ONCE'}) &&
	      $argv->[2] ne "" && $argv->[2] ne $self->{'INTERNAL_DIRECTIVES'}{'ONCE'}) {
		print $errmsg."unknown argument \"$argv->[2]\"\n";
		close(CONF);
		return 0;
	    }
	    opendir(CDIR, $include_path);
	    # Fetch filenames and sort them alphabetically
	    my @include_files = sort(grep(!/^\.{1,2}$/,(readdir(CDIR))));
	    closedir(CDIR);
	    # Load all listed files
	    foreach my $include_file (@include_files) {
		$include_file = abs_path($include_path."/".$include_file);
		# Skip subdirs
		next unless(-f $include_file);
		# If include once was specified, file will be included
		# only if it hasn't been included before.
		unless(defined($argv->[2]) &&
		       defined($self->{'INTERNAL_DIRECTIVES'}{'ONCE'}) &&
		       defined($self->{'INCLUDED_FILES'}{$include_file}) &&
		       $self->{'INTERNAL_DIRECTIVES'}{'ONCE'} ne "" && 
		       $argv->[2] eq $self->{'INTERNAL_DIRECTIVES'}{'ONCE'} &&
		       $self->{'INCLUDED_FILES'}{$include_file}) {
		    # Load nested configuration file and parse it
		    my ($res, $resmsg) = $self->load_config_file($include_file,
								 $path_node);
		    unless($res) {
			print $errmsg.$resmsg."\n" if(defined($resmsg));
			close(CONF);
			return 0;
		    }
		}
	    }

	} else {

	    # Store:
	    #
	    #  1. the scalar reference to the config file name,
	    #  2. current config file line number,
	    #  3. the array reference to the argument array
	    #
	    # into a fixed array that we use as an equivalent
	    # to a C struct representing single configuration 
	    # line, and then store reference to it into config
	    # linked list.
	    #
	    # Config array is, in layman's terms, a list
	    # of configuration lines, with some additional
	    # info tagging along.
	    #
	    $self->cfline_push($self->cfline_new(\$filename, $ln, $argv));
	
	}

	# Reset for next line
	$line = "";

    }

    close(CONF);

    return 1;
}
#
# Process CLI input and parse the configuration line
# in current section section context.
#
#  Config file is hierarchically organised. Each hierarchy level
#  is one section. A section is delimited by a starter directive
#  and either an explicit end directive, or an unknown directive.
#
#  If an unknown configuration directive is found during section
#  parsing, it will mark the end of the section and the unparsed
#  line will be returned to the parent section for processing.
#  Each time a section cannot parse the directive, it will keep
#  'moving' upwards until it reaches the top section. If the top
#  section itself cannot parse the directive, it is considered
#  unknown and the error is returned along with the failed line
#  itself.
#
#   Input:	1. object reference, passed implicitly
#		2. array reference to the current context 'struct'
#
#   Output:	1. TRUE, on success
#                  FALSE, on error or noop
#		   undef, if returning an unparsed line
#		2. undef, on success, error or noop,
#		   cfline, if returning an unparsed line
#
sub process_cli_input($) {
    my $self = shift;

    # Get CLI line if ready
    my $cfline = $self->read_cli_input;
    # If line is not yet complete,
    # just return 0
    return 0 unless($cfline);

    # Exit section if explicit section end
    # was requested from the command line
    if(defined($self->{'INTERNAL_DIRECTIVES'}{'END_SECTION'}) &&
       defined($cfline->[CF_LINE_ARGV][0]) &&
       $cfline->[CF_LINE_ARGV][0] eq $self->{'INTERNAL_DIRECTIVES'}{'END_SECTION'} &&
	!defined($cfline->[CF_LINE_ARGV][1])) {
	# Exit current section unless we are
	# already in the topmost section
	if($self->section_context_up) {
	    # Redraw prompt
	    $self->term_prompt;
	# If we were in the top section,
	# we will end cli section
	} else {
	    $self->end;
	}
	return 1;
    }

    # Save current section hierarchy
    $self->section_context_save;

retry:

    # Get current section context
    my $context = $self->section_context_get;
    # Parse current cfline
    my ($res, $unparsed) = $self->parse_line($cfline, @{$context});

    # On successful line parse,
    if($res) {

	# Redraw prompt
	$self->term_prompt;

    # If line parser returns undef result, it failed
    # to parse the line in this context, so we should
    # move one context level up and retry.
    } elsif(!defined($res)) {

	# Keep moving up the section hierarchy until
	# either the line is successfully parsed in
	# some context along the path, or top section
	# is reached, which means the line couldn't
	# be parsed.
	goto retry if($self->section_context_up);
	
	# In case current directive cannot be parsed
	# in any context, an error message should be
	# displayed, but the section context shouldn't
	# change, because it would be annoying to have
	# to navigate back to the proper section every
	# time we make a typpo. Thats why when unparsed
	# directive is encountered, we will search for
	# a match in upper contexts all the way to the
	# top, and if there is no match, we will simply
	# restore saved section hierarchy.
	$self->section_context_restore;

	# Display message returned by line parser.
	$self->display("Invalid command \"%s\"\n",
			join(' ',@{$cfline->[CF_LINE_ARGV]}));
	$res = 0;

    # If an error occured, result will be defined,
    # but FALSE, so, if error message is defined,
    # show it and return FALSE. Otherwise, just
    # return FALSE, to signal error condition.
    } else {
	
	
	# If error or help message was returned,
	# just display it and return FALSE
	if(defined($unparsed)) {
	    # Display message returned by line parser.
	    $self->display($unparsed."\n");
	}

    }

    # If context sensitive action was in progress
    if($self->is_context_action) {
	# Restore original section context
	$self->section_context_restore;
    # If normal parsing occured,
    } else {
	# Discard saved section hierarchy, if any.
	# If parsed line had a global leading directive,
	# section hierarchy has already been restored
	# and this call will have no effect.
	$self->section_context_discard_saved;
    }

    # Reset context sensitive action flags
    $self->context_action_reset;

    return $res;
}
#
# Parse the configuration section
#
#  Config file is hierarchically organised. Each hierarchy level
#  is one section. A section is delimited by a starter directive
#  and either an explicit end directive, or an unknown directive.
#
#  If an unknown configuration directive is found during section
#  parsing, it will mark the end of the section and the unparsed
#  line will be returned to the parent section for processing.
#  Each time a section cannot parse the directive, it will keep
#  'moving' upwards until it reaches the top section. If the top
#  section itself cannot parse the directive, it is considered
#  unknown and the error is returned along with the failed line
#  itself.
#
#   Input:	1. object reference, passed implicitly
#		2. hash reference to the current section template
#		3. current section name, if any
#
#   Output:	1. TRUE, on success
#                  FALSE, on error
#		   undef, if returning an unparsed line
#		2. undef, on success,
#		   array reference to cfline this section
#		   failed to parse
#
sub parse_section($) {
    my $self = shift;
    my $errmsg = "Config file \"%s\" error on line %u: ";

    # Parse line by line within current section
    while(my $cfline = $self->cfline_shift()) {

	# Exit section if explicit section end was
	# encountered in the configuration file.
	if($cfline->[CF_LINE_ARGV][0] eq $self->{'INTERNAL_DIRECTIVES'}{'END_SECTION'} &&
	    !defined($cfline->[CF_LINE_ARGV][1])) {
	    # Exit current section unless we are
	    # already in the topmost section
	    $self->section_context_up;
	    # Go to the next line
	    next;
	}

retry:

	# Get current context
	my $context = $self->section_context_get;
	# Parse current cfline
	my ($res, $unparsed) = $self->parse_line($cfline, @{$context});

	# If line parser returns undef result, it failed
	# to parse the line in this context, so we should
	# move one context level up and retry.
	unless(defined($res)) {

	    # Keep moving up the section hierarchy until
	    # either the line is successfully parsed in
	    # some context along the path, or top section
	    # is reached, which means the line couldn't
	    # be parsed.
	    goto retry if($self->section_context_up);

	    # Display message returned by line parser.
	    printf($errmsg."invalid directive \"%s\"\n",
		   ${$cfline->[CF_LINE_FILE]},
		   $cfline->[CF_LINE_NUM],
		   join(' ',@{$cfline->[CF_LINE_ARGV]}));
	    return 0;

	# If an error occured, result will be defined,
	# but FALSE, so, if error message is defined,
	# show it and return FALSE. Otherwise, just
	# return FALSE, to signal error condition.
	} elsif(!$res) {

	    # If error or help message was returned,
	    # just display it and return FALSE
	    if(defined($unparsed)) {
		# Display message returned by line parser.
		printf($errmsg.$unparsed."\n",
		       ${$cfline->[CF_LINE_FILE]},
		       $cfline->[CF_LINE_NUM])
	    }
	    return 0;

	}

    }

    return 1;
}
#
# Parse the configuration line
#
#  This function gets called from parse_section() and process_cli_input()
#  to parse each config line. It must be made aware from which section
#  it is being called by passing the referrence to the current section 
#  template and, optionally, the current section name, if it's defined.
#  If configuration directive enters a nested section, it will recursively
#  call parse_section() or process_section() and the whole process will
#  be repeated.
#
#   Input:	1. object reference, passed implicitly
#		2. cfline array ref
#		3. hash reference to the current section template
#		4. current section name, if any
#
#   Output:	1. TRUE, on success
#                  FALSE, on error
#		   undef, if returning an unparsed line
#         	2. undef, on success
#		   error message, on error,
#		   cfline, if returning an unparsed line
#
sub parse_line($$$;$) {
    my $self  = shift;
    my ($cfline, $section_template, $current_section_name) = @_;
    my $resmsg;
    my $res;

    unless(defined($section_template)) {
	die "Config template error: missing section template\n";
    }

    # Argument array, its current index and last argument used are
    # local and persist throughout entire current line context, 
    # but do not exist outside it.
    local $argv = $cfline->[CF_LINE_ARGV];
    local $argi = 0;
    local $argl;

    # These will hold section name and section template reference for
    # nested section. Nested sections must not be parsed before full
    # line parsing is done, regardless of directive template they have
    # been defined in. These variables are declared as locals, because
    # they need to persist throughout entire current line context.
    local ($nested_section_name, $nested_section_template);

    # Save what is supposed to be directive name
    my $directive_name = defined($argv->[$argi]) ? $argv->[$argi]:'';

    # Recursively parse the directive and its subdirectives
    ($res, $resmsg) = $self->parse_directive($cfline,
					     $section_template,
					     $current_section_name);

    # If directive parser returned FALSE and an error message, or undef
    # and, optionally, unparsed cfline from subordinate section, we will
    # propagate them to upper recursion levels.
    return ($res, $resmsg) unless($res);

    # User entered more than (s)he is supposed to ?
    if(($res & 1) && defined($argv->[$argi])) {
	# Return not-parsed and complain
	return (0, $self->is_cli ?
		    "Unknown argument \"".$argv->[$argi]."\"":
		    "directive \"$directive_name\" given unknown argument \"".$argv->[$argi]."\"");
    }

    # Context sensitive action in progress ?
    if($self->is_context_action) {
	# Context sensitive help requested ?
	if($self->{'FLAG_HELP'}) {
	    # If this is help for lead directive,
	    # we will prepend global directives
	    # help to it.
	    if($res & CSA_DIRECTIVE) {
		# Get context sensitive help
		# for global directives
		my $gcshelp = $self->global_cshelp;
		$resmsg = (defined($gcshelp) ? $gcshelp:'').(defined($resmsg) ? $resmsg:'');
	    # If this is help for a conditional
	    # or a last argument, we will append
	    # '... press enter ...' message.
	    } elsif($res & (1|CSA_CONDITIONAL) && $self->{'FLAG_TSPACES'}) {
		$resmsg .= "\n" if(defined($resmsg) && $resmsg ne '');
		$resmsg .=  "  <[enter]>           Execute this command";
	    }
	    # Return context sensitive help
	    return (defined($resmsg) && $resmsg ne '') ?
		    (0, "Possible completions:\n".$resmsg):0;
	}
	# Default context sensitive action result
	# that will be reported to section parser
	# is not-parsed/without-error-message
	return 0;
    }

    # Otherwise, the end of the line was reached without errors.

    # If any directive template for the current line defines entry
    # 'section' which holds a reference to a lower hierarchy level
    # section template, we will recursively parse it by invoking
    # section parser. When section parser encounters a directive
    # it cannot parse, it will return it to us, so that we can try
    # to parse it ourselves. If we cannot parse it, we will return
    # it to the upper recursion level. It will keep going upwards
    # until top section is reached. If top section fails to parse
    # it, it is an unknown directive.
    if(defined($nested_section_template)) {
	#
	# Add the section template and name to the recursion path.
	# Section recursion path preserves information about current
	# and upper level contexts which define section hierarchy.
	#
	# Each context 'structure' is a 2-element array:
	#  1. element: a hash reference to the section template
	#  2. optional section name or undef.
	$self->section_context_enter($nested_section_template,
				     $nested_section_name);
	# If a section requires a section name and because section
	# name wasn't known before the config line was actually
	# parsed, default values for arguments within the section
	# that store values in hash key paths that depend on the
	# section name, could not have been parsed before because
	# we couldn't store them, so we will parse them now, adding
	# to our configuration defaults template.
	#
	# Prepare defaults for this particular section
	$self->section_defaults($nested_section_template,
				$nested_section_name);
    }

    return 1;
}
#
# Parse current directive and its subdirectives.
#
#  This function gets called from the line parser to begin parsing 
#  the current line starting with its main directive, or recursively
#  from itself, to parse the main directive's subdirectives. Also, if
#  current main directive has dependencies, they are recursively
#  processed before the current directive. This requires some config
#  linked list juggling: If dependency line is found, config list has
#  to be split into 2 segments. Lower segment starts with dependency
#  line which is then fed to the section parser (we don't use the line
#  parser directly, because the section parser is the block that does
#  both processing and error handling for its subordinate blocks, so
#  we don't have to do error handling ourselves). Once dependency is
#  processed, config segments are merged back together. This is done
#  for each dependency of the current main directive. Splitting and
#  merging config lists is a matter of moving a few references, so 
#  it is not particularly expensive.
#
#   Input:	1. object reference, passed implicitly
#		2. cfline array ref
#		3. reference to a hash with directive templates
#		   entries:
#		      directive_name => directive_template
#		   It could be a section template or
#		   a hash of nested subdirectives.
#		4. current section name if any
#
#   Output:	1. CSA_xxxxxx, on successful context sensitive action,
#		   TRUE, on other successful operations,
#		   FALSE, on error
#		   undef, if directive is unknown
#		2. undef, on success
#		   error message, on error
#		   unparsed cfline, if directive is unknown
#		   and propagated from subordinate contexts
#
sub parse_directive($$) {
    my $self = shift;
    my ($cfline, $section_template, $current_section_name) = @_;
    my ($res, $resmsg, $helpmsg);

retry:

    # First argument is the directive name
    my $directive_name = defined($argv->[$argi]) ? $argv->[$argi]:'';

    # Get directive's directive template.
    my $directive_template = $section_template->{'dirs'}{$directive_name};

    # If we were called from a CLI session ...
    if($self->is_cli) {
	# If exact match doesn't exist, we will
	# attempt a partial match in order to
	# achieve command auto-completion ...
	unless(defined($directive_template)) {
	    # ... but only if context sensitive action
	    # is not in progress or the current argument
	    # is not the last argument on the line.
	    #
	    # If current line has trailing spaces,
	    # we will treat them as a null argument
	    # which will not be parsed, but will
	    # increase the argument array size by 1
	    # element.
	    #
	    # This is to prevent autocompleting
	    # directives that are subject to context
	    # sensitive actions which are triggered
	    # manually, by user.
	    if(!$self->is_context_action ||
		$argi < ($#{$argv} + ($self->{'FLAG_TSPACES'} ? 1:0))) {
		# Look for possible completions
		my @compl = $self->match_keys($directive_name,
					      $section_template->{'dirs'});
		# Any completion candidates found ?
		if(my $num_compl = scalar(@compl)) {
		    # We can complete current command only if
		    # there is a single completion candidate.
		    if($num_compl == 1) {
			# Complete the partial directive name
			$directive_name = shift @compl;
			# Get the directive template
			$directive_template = $section_template->{'dirs'}{$directive_name};
		    # Otherwise, when there is more than one
		    # possible completion, complain ...
		    } else {
			return(0, "Ambiguous command \"$directive_name\"");
		    }
		}
	    }
	}
    }

    # If neither exact nor partial match exists,
    # we will attempt a regex match ...
    unless(defined($directive_template)) {
	# Search through all the keys in current section
	foreach my $regex (keys %{$section_template->{'dirs'}}) {
	    # Regular expressions must be / / enclosed.
	    # Try matching current directive name against regex
	    if($regex =~ /^\/.*\/$/ && $self->eval_expression("'$directive_name' =~ $regex")) {
		# On successful match, get matching directive template
		$directive_template = $section_template->{'dirs'}{$regex};
		last;
	    }
	}
    }

    # If we are called from a CLI session ...
    if($self->is_cli) {
	# If there is no directive match whatsoever ...
	unless(defined($directive_template)) {
	    # ... and the CLI context sensitive action flag is set,
	    # we wont really parse the directive. Instead, we will
	    # allow context sensitive functions to examine and
	    # possibly modify the current directive name.
	    if($self->is_context_action) {
		# Perform context sensitive action
		($res, $resmsg) = $self->perform_csa($directive_name,
						     $section_template->{'dirs'});
		# If context sensitive action succeeded,
		# we are done, so we should just exit
		# presenting ourselves as leading directive
		# within current context.
		return (CSA_DIRECTIVE, $resmsg) if($res);
	    }
	}
    }

    # If current section template doesn't have any entry matching
    # this directive name, not even the wildcard directive template,
    # return undef to signal line parser to return unparsed cfline
    # to the upper template hierarchy level, to give it a chance to
    # parse it itself. Also, if this entry is supposed to be hidden
    # from the parser, treat it as unknown directive and return undef,
    # triggering the same process.
    if(!defined($directive_template) ||
       defined($directive_template->{'op'}) &&
       $directive_template->{'op'} & D_OP_HIDDEN) {
	return defined($resmsg) ? (0, $resmsg):undef;
    }

    # If this is the main directive of the current config line,
    if(!$argi && !$self->is_cli) {
	# Check if this directive template has any dependencies defined
	if(defined($directive_template->{'deps'})) {
	    # Loop through all dependencies
	    foreach my $dep (@{$directive_template->{'deps'}}) {
		# Find configuration lines for the current dependency
		# (some directives can be defined more than once)
		foreach my $dep_cfline ($self->cfline_find($dep)) {
		    # Split config linked list into 2 parts:
		    #   1. from the current directive upto (but excluding)
		    #      the dependency
		    #   2. from the dependency to the end of the list
		    # and replace config reference with lower half's ref
		    my ($config_upper, $config_lower) = &config_split($self->{'CONFIG'},
								      $dep_cfline);
		    $self->{'CONFIG'} = $config_lower;
		    # Parse dependency line recursively (wrapped inside
		    # section parser, in case errors should be handled)
		    ($res, $resmsg) = $self->parse_section;

		    unless($res) {
			# If section parser returned undef and
			# an unparsed line, it means that it
			# doesn't belong to that context, so
			# we will just put it back into config
			# lines list and let it be processed
			# normally, later.
			if(!defined($res) && defined($resmsg)) {
			    $self->cfline_unshift($resmsg);
			# Otherwise, we will just propagate
			# the error and, optionally, error
			# message, upwards.
			} else {
		    	    return ($res, $resmsg);
			}
		    }
		    # Restore original config linked list - merge
		    # config list segments into one list.
		    $self->{'CONFIG'} = &config_concat($config_upper,
						       $config_lower);
		}
	    }
	}
    }

    # Move argument index to the next argument,
    # unless explicitly told not to
    $argi++ unless(defined($directive_template->{'op'}) &&
		    $directive_template->{'op'} & D_OP_NONEXTARG);

    # Get reference to the nested section template if any.
    # Section that will begin once full line has been processed,
    # depends on the current context. The last (sub)directive
    # of the current argument line that has 'section' entry defined
    # will determine the section that will be entered, once full
    # line processing is done.
    if(defined($directive_template->{'section'})) {
	$nested_section_template = $directive_template->{'section'};
    }
    # If current directive has arguments, try to parse them
    if(defined($directive_template->{'arg'})) {

	# Check what type of directive this is:
	#  - It can be a directive with a single argument or using everyting
	#    up to the end of the line, in which case 'arg' entry will
	#    be a hash reference to a single value template.
	#  - It can be a directive with multiple arguments, in which case
	#    'arg' entry will be a reference to the array of hash
	#    references to value templates (huh:)
	my $args_type = ref($directive_template->{'arg'});

	# If this 'arg' entry specifies a single value or section template,
	# turn it into a single member array and assign array's reference 
	# to the common variable $arguments.
	#
	# If this 'arg' entry specifies multiple value or section templates 
	# (current directive has multiple arguments and/or subdirectives),
	# this directive template entry should already be a reference to
	# the array of value template hash references, so we can just assign
	# it as is to the common variable $arguments.
	my $arguments;

	if($args_type eq "HASH") { 
	    $arguments = [$directive_template->{'arg'}];
	} elsif($args_type eq "ARRAY") { 
	    $arguments = $directive_template->{'arg'};
	} elsif(defined($args_type)) {
	    die "Config template error: invalid directive template entry at directive \"$directive_name\"\n";
	}

	# A directive with a single argument appears as a single element
	# array, and a directive with multiple arguments already is
	# an array, so we can both parse them with a single loop.
	for(my $i = 0, my $numargs = $#{$arguments}+1; $i < $numargs; $i++) {

	    # Get current argument
	    my $argument = $arguments->[$i];

	    # If this is conditional argument,
	    # evaluate the condition expression
	    if(defined($argument->{'cond'})) {
		# Expression must evaluate to TRUE,
		# if this argument is to be used
		next unless $self->eval_expression($argument->{'cond'},
						   undef,
						   $directive_name,
						   $current_section_name,
						   $nested_section_name);
	    }

	    # If current argument is a value template,
	    if($argument->{'template'} == TT_VALUE) {

		# Parse current directive's paramaters
		($res, $resmsg) = $self->parse_argument($directive_name,
							$argument,
							$current_section_name);
		# Propagate error to our caller
		return ($res, $resmsg) unless($res);

		# Result from context sensitive action ?
		if($res > 1 && $self->is_context_action) {
		    # Context sensitive help returned ?
		    if($self->{'FLAG_HELP'}) {
			# Help message present ?
			if(defined($resmsg)) {
			    # Append newline
			    $helpmsg .= "\n" if(defined($helpmsg) && $helpmsg ne "");
			    # Append help message returned
			    # by argument parser function
			    $helpmsg .= $resmsg;
			}
			# If argument was contidional, continue looping
			# in order to construct help message explaining
			# all possible arguments.
			next if(defined($argument->{'cond'}));
		    }
		    # Return the context sensitive action results
		    return (CSA_ARGUMENT, $helpmsg);
		}

	    # If current argument is a section template ...
	    } elsif($argument->{'template'} == TT_SECTION) {

		# ... and there's no more line arguments,
		# a subdirective is missing.
		unless(defined($argv->[$argi])) {
		    # If context sensitive action wasn't requested ...
		    unless($self->is_context_action) {
			# ... if subdirective is not present
			# and not required, just skip it
			next unless($argument->{'op'} & S_OP_REQUIRE);
			# Otherwise, complain that
			# subdirective is missing
			return (0, "directive \"$directive_name\" is missing a required sub-directive");
		    # If command completion was requested,
		    } elsif($self->{'FLAG_COMPLETE'}) {
			# Just return, because we can't
			# complete an empty string.
			return CSA_ARGUMENT;
		    }
		}

		# Because this particular mandatory/non-mandatory
		# subdirectives' context exists only within current
		# directive and only if parsing of the actual config
		# line has led to it's creation, this is the only
		# place where we can prepare default values for
		# arguments belonging to this context.

		# Prepare defaults for this subdirectives' context
		$self->section_defaults($argument,
					$current_section_name,
					$nested_section_name);

		# Call ourselves recursively
		($res, $resmsg) = $self->parse_directive($cfline,
							 $argument,
							 $current_section_name);

		unless(defined($res)) {
		    # If subdirective is unknown,
		    # but not required, just skip it
		    next unless($argument->{'op'} & S_OP_REQUIRE);

		    # Otherwise, return TRUE, to make line parser
		    # that called us, complain about unknown argument.
		    # It's a dirty hack, I know ...
		    return 1;
		}

		# Propagate error to our caller
		return ($res, $resmsg) unless($res);

		# Result from context sensitive action ?
		if($res > 1 && $self->is_context_action) {
		    # Context sensitive help returned ?
		    if($self->{'FLAG_HELP'}) {
			# Help message present ?
			if(defined($resmsg)) {
			    # Append newline if current help
			    # message doesn't end with newline.
			    $helpmsg .= "\n" if(defined($helpmsg) && $helpmsg ne "");
			    # Append help message returned
			    # by directive parser function
			    $helpmsg .= $resmsg;
			}
			# If directive was a contidional leading subdirective,
			# continue looping in order to construct help message
			# explaining all possible arguments.
			next if(($res & CSA_DIRECTIVE) &&
				 (defined($argument->{'cond'}) ||
				 ($argument->{'op'} & S_OP_ALLOW)));
		    }
		    # Propagate context sensitive action status to the caller
		    return ($res & CSA_CONDITIONAL ? $res:CSA_ARGUMENT, $helpmsg);
		}

	    } else {
		die "Config template error: invalid argument at directive \"$directive_name\"\n";
	    }
	}
    }
    # If this directive template has optional subdirectives,
    # we will parse them in a loop, until we encounter subdirective
    # that we cannot parse or we reach the end of the line.
    # If unknown directive is found, we will keep returning undef
    # upwards, through recursion levels, until we reach the level
    # that can parse it, or until we get back all the way to the
    # top section, in which case the subdirective cannot be parsed
    # so error is returned.
    if(defined($directive_template->{'opt'})) {

	# Because this particular optional subdirectives' context
	# exists only within current directive and only if parsing
	# of the actual config line has led to it's creation, this
	# is the only place where we can prepare default values for
	# arguments belonging to this context.

	# Prepare defaults for optional subdirectives' context
	$self->section_defaults($directive_template->{'opt'},
				$current_section_name,
				$nested_section_name);

	# Go through arguments on the current line,
	# until the end of the line is reached
	while(defined($argv->[$argi])) {
	    # Call ourselves recursively
	    ($res, $resmsg) = $self->parse_directive($cfline,
						     $directive_template->{'opt'},
						     $current_section_name);
	    # If lower recursion level returned undef, subdirective
	    # is unknown to it, so we will prepare error message
	    # just in case, and then we will try to parse it on our
	    # level. If we cannot parse it ourselves, we will return
	    # undef to upper level along with the error message. If
	    # we reach section parser while going upwards, noone managed
	    # to parse the subdirective along the way, so the error will
	    # be returned and error message will be displayed.
	    goto retry unless defined($res);
	    # If lower recursion level returned FALSE, then it or some
	    # level below it returned an error, so propagate it upwards,
	    # towards the section parser.
	    return ($res, $resmsg) unless $res;
	}

    }

    # Are we in CLI mode ?
    if($self->is_cli) {
	# Context sensitive action in progress ?
	if($self->is_context_action) {
	    # If we have context sensitive help message pending
	    # after the last argument has been processed, we should
	    # return it to the caller to be displayed.
	    if($self->{'FLAG_HELP'} && defined($helpmsg) && $helpmsg ne "") {
		return (CSA_CONDITIONAL, $helpmsg);
	    }
	# Otherwise, it is normal operation ...
	} else {
	    # If global operator is set, we will restore the saved
	    # section hierarchy, which effectively makes this directive
	    # a global one. If encountered within a section, parser will
	    # search for a matching template upwards until it reaches
	    # the top section. If global directive has been found and
	    # parsed, parser will return to the section that was its 
	    # starting point, effectively parsing the global directive
	    # that is defined outside the current section, while staying
	    # inside the current section context.
	    if(defined($directive_template->{'op'}) &&
	       ($directive_template->{'op'} & D_OP_GLOBAL)) {
		$self->section_context_restore;
	    }
	}
    }

    return 1;
}
#
# Parse current directive arguments.
#
#  This function takes arguments from the current argument line
#  and parses them, producing one or more values, depending on
#  the requested data type. Produced values are returned as 
#  - scalars, if parsed argument results in a single value
#  - array references, if parsed argument or arguments result
#    in an array of values.
#  - hash references, if parsed argument or arguments result
#    in a hash of values.
#  Sanity check is performed on most of produced values, but not
#  all - strings are not checked, for exmple. If this check fails
#  or values are out of bounds, FALSE is returned along with
#  appropriate error message.
#
#   Input:	1. object reference, passed implicitly
#		2. current directive's name
#		3. current value template
#		4. current section name, if any
#		5. line operator flags (optional
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		2. produced value, on success
#		   error message, on error
#
sub parse_argument($$$;$$) {
    my $self = shift;
    my ($directive_name, $value_template, $current_section_name) = @_;
    my ($res, $resmsg);

    # All directive arguments are mandatory by default. If argument
    # is missing and is not explicitly declared as optional, throw
    # an error with error message.
    unless(($value_template->{'type'} == CF_MAPPED
	     && defined($value_template->{'key'})) ||
	    $value_template->{'type'} == CF_NONE ||
	    defined($argv->[$argi])) {
	# Was CLI context sensitive action requested ?
	if($self->is_context_action) {
	    if($argi >= $#{$argv}) {
		#
		unless((defined($argv->[$argi]) && $argv->[$argi] ne '') ||
			$self->{'FLAG_TSPACES'} ||
			$argi == 0) {
		    $self->{'FLAG_COMPLETE'} = 0;
		    $self->{'FLAG_HELP'} = 1;
		    return (CSA_ARGUMENT, "  <[space]>           Move on to the next argument");
		}
		# Context sensitive help ?
		if($self->{'FLAG_HELP'}) {
		    # Format help string
		    my $help = sprintf("  %-20s%s",
				       "<argument>",
				       $value_template->{'help'});
		    # Return context sensitive help message
		    return (CSA_ARGUMENT, $help);
		}
	    }
	} else {
	    return (0, "directive \"$directive_name\" is missing an argument");
	}
    }

    # Parse different value types
    my $value;

    # Unless value type is CF_NONE, which means
    # no argument is expected for the current
    # directive, we will call the data type parser,
    # which will produce one or more argument values.
    unless($value_template->{'type'} == CF_NONE) {
	# Call the data type parser
	($res, $value) = $self->parse_data_type($directive_name,
						$value_template,
						$current_section_name,
						$nested_section_name);
	# Parsing failed - return failed
	# status and an error message upstream.
	return ($res, "argument ".$value) unless($res);
    }

    # If CLI context sensitive action
    # was requested, we will stop here.
    return 1 if($self->is_context_action);

    # Every value template can have a custom post parser
    # function that can do a sanity check on parsed value
    # or it can store parsed value in some way other than
    # default. If used for live command line parsing, it
    # can be used to execute cli commands.
    my $post_parser = $value_template->{'postparser'};
    if(defined($post_parser)) {
	#
	# Call the post-parser.
	#
	# Input:	1. object reference,
	#		2. configuration directive name,
	#		3. reference to hash or array of values
	#		   or value itself, depending on the
	#                  value type,
	#		4. hash reference to the destination hash store
	#		5. hash reference to the map hash (if any)
	#		6. current section name (if any)
	#		7. nested section name (if any)
	# Output:	1. TRUE, if post-parsing was successful
	#		   FALSE, if post-parsing failed
	#		2. optional custom-parsed value, on success
	#		   optional error message, on error
	#
	($res, $resmsg) = $post_parser->($self,
					 $directive_name,
					 $value,
					 $self->{'WORK'},
					 $value_template->{'map'},
					 $current_section_name,
					 $nested_section_name);
	return ($res, $resmsg) unless $res;
	# If post-parser returned success, and a value,
	# we will replace whatever we produced as value(s)
	# with value(s) the post-parser returned.
	if(defined($resmsg)) {
	    # If value type has CF_SECTION modifier,
	    # return value from post-parser will be
	    # interpreted as a section template hash
	    # reference. This enables cool features
	    # like making it easy for modular code
	    # to load its 'plugins' and dynamically
	    # register their configuration templates,
	    # all from within post-parser. Post-parser
	    # only has to return a reference to the
	    # section template, which will automatically
	    # be used as the nested section once current
	    # configuration line is fully processed.
	    if($value_template->{'type'} & CF_SECTION) {
		if(ref($resmsg) eq "HASH" &&
		    $resmsg->{'template'} == TT_SECTION) {
		    $nested_section_template = $resmsg;
		} else {
		    return (0, "postparser returned invalid section template");
		}
		# Otherwise, it will be treated
		# as ordinary re-parsed argument.
	    } else {
		$value = $resmsg;
	    }
	}
    }

    # If value type is CF_SECTION_NAME, current argument
    # is taken as the name of the section the parser
    # will enter once the current line has been fully
    # processed. Section name is saved in a dedicated
    # variable until all arguments have been processed.
    if($value_template->{'type'} & CF_SECTION_NAME &&
       !($value_template->{'type'} & CF_SECTION)) {
	$nested_section_name = $value;
    }
    # Now we usualy want to do something with produced values,
    # like store them, or modify them and then store them ...
    # Does this value template have any operations defined ?
    if(defined($value_template->{'ops'})) {

	my $operation_templates;

	# Prep array of store operations
	my $oper_type = ref($value_template->{'ops'});
	if($oper_type eq "HASH") { 
	    $operation_templates = [$value_template->{'ops'}];
	} elsif($oper_type eq "ARRAY") { 
	    $operation_templates = $value_template->{'ops'};
	} elsif(defined($oper_type)) {
	    die "Config template error: invalid value template operation definition at directive \"$directive_name\"\n";
	}

	# Execute operations in sequence
	foreach my $operation_template (@{$operation_templates}) {

	    # Operator O_OP_DELETE deletes particular hash entry
	    # defined by hash key or the entire destination hash.
	    if($operation_template->{'op'} & O_OP_DELETE) {

		# Let's begin deleting.
		if(defined($operation_template->{'hash'})) {
		    if(defined($operation_template->{'key'})) {
			# Delete entire destination hash
			delete $self->{'WORK'}{$operation_template->{'hash'}}{$operation_template->{'key'}};
		    } else {
			# Delete entire destination hash
			delete $self->{'WORK'}{$operation_template->{'hash'}};
		    }
		} else {
		    die "Config template error: value template with operator O_OP_DELETE is missing a target for deletion\n";
		}

	    # Operator O_OP_MODIFY modifies produced values
	    } elsif($operation_template->{'op'} & O_OP_MODIFY) {

		# Perhaps, modify produced values before trying
		# to store or assign them as a nested section name
		$value = $self->modify_value($operation_template->{'mod'},
					     $value,
					     $directive_name,
					     $current_section_name,
					     $nested_section_name);
		# If value is also ment to be a nested section name,
		# any modification to it has to be applied to 
		# the nested section name as well.
		if($value_template->{'type'} & CF_SECTION_NAME &&
		   !($value_template->{'type'} & CF_SECTION)) {
		    $nested_section_name = $value;
		}

	    # Remaining available operators are flavors of store operation
	    } else {

		# If the destination hash and the hash key
		# were defined, value(s) should be stored.
		if(defined($operation_template->{'hash'}) &&
		    defined($operation_template->{'key'})) {
		    # Store values produced by parsing current argument(s).
		    ($res, $resmsg) = $self->store_values($operation_template,
						          $value,
						          $directive_name,
						          $current_section_name,
						          $nested_section_name);
		    # Something went wrong - return failed
		    # status and an error message upstream
		    return ($res, $resmsg) unless($res);
		}

	    }
	
	}
    }

    return 1;
}
#
# Parse (from) current argument.
#
#  Parsing configuration directive's arguments can produce one or more
#  values. Either directive's value type implies that all or some
#  config line arguments up to the end of the line are members of
#  multi-element value (like CF_ARRAY, for example), or a single
#  argument can produce multiple values (like CF_SUBNET, for example).
#  Also, we can have combination of both, where there are multiple
#  arguments up to the end of the line, each producing multiple values.
#
#
#   Input:	1. object reference, passed implicitly
#		2. Current directive name
#		3. Current value template reference
#		4. Current section name
#		5. Nested section name
#		6. Current value type (optional)
#		7. Operator bitfield (optional)
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		2. produced value, on success
#		   error message, on error
#
sub parse_data_type($$$$$;$$) {
    my $self = shift;
    my ($directive_name, $value_template, $current_section_name, $nested_section_name, $value_type, $operator) = @_;
    my $value;

    # Parse different value types
    $value_type = $value_template->{'type'} unless defined($value_type);
    unless(defined($operator)) {
	$operator = defined($value_template->{'op'}) ?
			$value_template->{'op'}:V_OP_NOOP;
    }

    # Save current argument
    $argl = $argv->[$argi];

    if($value_type & CF_ARRAY) {
	# Check if array needs to have 
	# specific element data types
	my $element_type = $value_type ^ CF_ARRAY;
	# If element data type was defined,
	if($element_type) {
	    # Loop through all array elements
	    while($argi < $#{$argv}+1) {
		# Call ourselves recursively to parse each array element
		my ($res, $resmsg) = $self->parse_data_type($directive_name,
							    $value_template,
							    $current_section_name,
							    $nested_section_name,
							    $element_type,
							    $operator & ~V_OP_NONEXTARG);
		return ($res, "array element ".$resmsg) unless($res);
		# Put produced value into array
		push @{$value}, $resmsg;
	    }
	# Otherwise, just copy the entire array
	} else {
	    @{$value} = splice(@{$argv}, $argi);
	}
	# Advance argument index unless explicitly told not to
	$argi = $#{$argv}+1 unless $operator & V_OP_NONEXTARG;
    } elsif($value_type & CF_MAPPED) {
	# Maps for mapped entries are hashes whose keys
	# are current directives' arguments
	my $mapname = $value_template->{'map'};
	my ($mapkey, $map);
	if(defined($value_template->{'key'})) {
	    # Format map hash key
	    $mapkey = $self->eval_expression($value_template->{'key'},
					     undef,
					     $directive_name,
					     $current_section_name,
					     $nested_section_name);
	} else {
	    $mapkey = $argv->[$argi];
	    # Advance argument index unless explicitly told not to
	    $argi++ unless $operator & V_OP_NONEXTARG;
	}
	# If map is a hash name that is being configured,
	# we will look for it in the temporary work hash
	if(defined($self->{'WORK'}{$mapname})) {
	    $map = $self->{'WORK'}{$mapname};
	# Otherwise, we will assume it is some unrelated
	# global hash.
	} else {
	    # Hash name can be fully qualified, with namespace prefix,
	    # or relative to the caller, in which case we have to 
	    # prepend caller's namespace to the hashname
	    local *map_hash = *{$self->typeglob_qualify($mapname)};
	    # If hash doesn't exist in caller's namespace,
	    # use global hash in main::
	    local *map_hash = *{'::'.$mapname} unless %map_hash;
	    $map = \%map_hash;
	}
	# Check if this key exists in the map
	return (0, $mapkey." is not known") unless exists($map->{$mapkey});
	# Check what type of value this hash entry is
	my $mapvaltype = ref($map->{$mapkey});
	# If map entry at given key is scalar, we will
	# treat parsed value as scalar. Otherwise it must
	# be an array or hash reference.
	#
	# Destination hash's key and format templates
	# can backreference these by index, in case of
	# an array, or name, in case of a hash.
	if($mapvaltype eq "" ||
	    $mapvaltype eq "ARRAY" ||
	    $mapvaltype eq "HASH") {
	    $value = $map->{$mapkey};
	} else {
	    die "Config template error: value template for \"$directive_name\" used map with unsupported value format.\n";
	}
    } elsif($value_type & CF_EXPR) {
	my ($gr, $sq, $dq, $op, $arg, $expr, $expi);
	# This private function monitors grouping
	# delimiters like quotes or brackets
	my $groupings = sub {
	    if($1 eq "'") {
		# Single quotes flag: true when quote is open,
		# false when quote is closed, ignored when we
		# already have double quotes open.
		$sq ^= 1 unless $dq;
	    } elsif($1 eq '"') {
		# Double quotes flag: true when quote is open,
		# false when quote is closed, ignored when we
		# already have single quotes open.
		$dq ^= 1 unless $sq;
	    } elsif($1 eq '(') {
		# Open bracket starts a new grouping, unless
		# we have signle or double quotes open.
		$gr++ unless ($dq || $sq);
	    } elsif($1 eq ')') {
		# Closed bracket completes grouping, unless
		# we have signle or double quotes open.
		$gr-- unless ($dq || $sq);
	    }
	    return $1;
	};
	# Build expression string
	for($gr = $sq = $dq = 0, $op = 1, $expi = $argi, $arg = $argv->[$expi++];
	    defined($arg); $expr .= $arg, $arg = $argv->[$expi++]) {

	    # Look for the operator at the end of the current argument
	    if($arg =~  /(\={1,2}|\!\=|\>{1,2}|\>\=|\<{1,2}|\<\=|[\=\!]\~|\&{1,2}|\|{1,2}|\^|\~|\!|\+{1,2}|\-{1,2}|\/|\*{1,2}|\%)$/) {
		# Operator found: operation is pending
		$op = 1;
	    # Expression is complete when all groupings
	    # are complete, all quotes are closed and
	    # there are no hanging operators
	    } elsif(!($op|$gr|$sq|$dq)) {
		# End expression assembly
		last;
	    # Otherwise, the current argument is a rvalue
	    } else {
		# Operator not found: no operation pending
		$op = 0;
	    }

	    # Count the number of incomplete groupings:
	    # in order for expression to be valid,
	    # the number of incomplete groupings (open
	    # quotes or brackets) must be 0: 
	    $arg =~ s/(\"|\'|\(|\))/&$groupings/eg;

	}
	# If we reached the end of the line and we still have
	# open qoutes or brackets or an operator without rvalue,
	# the expression is invalid.
	return (0, "defines invalid expression") if($op|$gr|$sq|$dq);
	# Advance argument index unless explicitly told not to
	$argi = $expi-1 unless $operator & V_OP_NONEXTARG;
	# Application's constants allowed in the expression ?
	if($value_type & CF_CONST) {
	    my $replace = sub {
		# Typeglob a matched word as a constant
		# (a function, in fact) in caller's namespace
		local *CONST = *{$self->typeglob_qualify($1)};
		# If that doesn't exist, typeglob a matched word
		# as a constant in application's main:: namespace
		local *CONST = *{'::'.$1} unless defined(&CONST);
		# If there is a function with such a name, call it.
		# Otherwise, return as is.
		return defined(&CONST) ? &CONST:$1;
	    };
	    # Parse words that are supposed to be
	    # application's constants or functions
	    $expr =~ s/(\w+)/&$replace/eg;
	}
	$value = $self->{'EVAL'}->reval($expr);
    } elsif($value_type & CF_LINE) {
	$value = join(' ', splice(@{$argv}, $argi));
	# Advance argument index unless explicitly told not to
	$argi = $#{$argv}+1 unless $operator & V_OP_NONEXTARG;
    } elsif($value_type & CF_STRING) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
    } elsif($value_type & CF_PATH) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	unless($value =~ /^(?:\/?[^\!\?]*)+$/i) {
	    return (0, "must be a valid filesystem path");
	}

    } elsif($value_type & CF_BOOLEAN) {
	my $boolean = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	unless($boolean =~ /(?:(y|yes|t|true|on|1)|(n|no|f|false|off|0))/i) {
	    return (0, "must be boolean");
	}
	$value = $1 ? 1:0;
    } elsif($value_type & CF_REAL) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	unless($value =~ /^([\+\-]?\d+(?:\.\d+)?)$/) {
	    return (0, "must be a real number");
	}
	if($value_type & CF_POSITIVE) {
	    if($value < 0 || ($value == 0 && ($value_type & CF_NONZERO))) {
		return (0, "must be a positive real number");
	    }
	} elsif($value_type & CF_NEGATIVE) {
	    if($value > 0 || ($value == 0 && ($value_type & CF_NONZERO))) {
		return (0, "must be a negative real number");
	    }
	}
    } elsif($value_type & CF_INTEGER) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	# Octal ?
	if($value =~ /^0\d+$/) {
	    $value = oct($value);
	# Hexadecimal ?
	} elsif($value =~ /^0[xX][0-9a-fA-F]+$/) {
	    $value = hex($value);
	# If it is not decimal,
	# then it's unknown
	} elsif($value !~ /^([\+\-]?\d+)$/) {
	    return (0, "must be an integer");
	}
	if($value_type & CF_POSITIVE) {
	    if($value < 0 || ($value == 0 && ($value_type & CF_NONZERO))) {
		return (0, "must be a positive integer");
	    }
	} elsif($value_type & CF_NEGATIVE) {
	    if($value > 0 || ($value == 0 && ($value_type & CF_NONZERO))) {
		return (0, "must be a negative integer");
	    }
	}
    } elsif($value_type & CF_INET) {
	my $inet = $argv->[$argi];
	$inet = "0.0.0.0/0" if($inet =~ /^any$/i);
	# Split string representation of the address into octets
	my ($n1, $n2, $n3, $n4, $cidr) = ($inet =~ /^(\d+)\.(\d+)\.(\d+)\.(\d+)(?:\/(\d+))?$/);
	unless(defined($n1) && defined($n2) && defined($n3) && defined($n4) &&
	       $n1 >= 0 && $n1 <= 255 && $n2 >= 0 && $n2 <= 255 &&
	       $n3 >= 0 && $n3 <= 255 && $n4 >= 0 && $n4 <= 255) {
	    # If address format is invalid, it may be a hostname,
	    # so we will check it if CF_FQDN is present in the
	    # value types bit array.
	    if($value_type & CF_FQDN) {
		my ($res, $resmsg) = $self->parse_data_type($directive_name,
							    $value_template,
							    $current_section_name,
							    $nested_section_name,
							    $value_type ^ CF_INET);
		return ($res, $res ? $resmsg:"must be either an IPv4 address or a hostname");
	    # If CF_FQDN is not present, we got bogus argument
	    } else {
		return (0, "must be a valid IPv4 address or network or \'any\'");
	    }
	}
	# CF_ADDR and CF_PREFIX are conflicting modifiers and
	# cannot be present at the same time
	if(($value_type & CF_ADDR) && ($value_type & CF_PREFIX)) {

	    die "Config template error: cannot specify both CF_ADDR and CF_PREFIX at directive \"$directive_name\"\n";

	# If value type is CF_ADDR, but we got prefix argument
	# instead, throw an error and complain.
	} elsif(defined($cidr) && ($value_type & CF_ADDR)) {

	    return (0, "must be an IPv4 address, not prefix");

	# If CIDR was specified, check if it's ok.
	# If CF_PREFIX modifier is present, argument
	# MUST be a valid prefix
	} elsif((defined($cidr) && ($cidr < 0 || $cidr > 32)) ||
	    (!defined($cidr) && ($value_type & CF_PREFIX))) {
	    return (0, "must have a valid prefix length between 0 and 32");
	}
	# If atoi modifier was specified, we will convert
	# the string representation of the address/prefix
	# into integer representation.
	if($value_type & CF_ATOI) {
	    my $addr = (($n1&0xff)<<24)|(($n2&0xff)<<16)|(($n3&0xff)<<8)|($n4&0xff);
	    # If address argument was given with trailing CIDR,
	    # we will return a network and a netmask stored in
	    # the hash.
	    if(defined($cidr)) {
		$value->{'NETWORK'} = $addr;
		$value->{'NETMASK'} = ~((2**(32-$cidr))-1) & 0xffffffff 
	    # Otherwise, we will return just the address as a scalar
	    } else {
		$value = $addr;
	    }
	# Otherwise, we will return the argument as is
	} else {
	    $value = $inet;
	}
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
    } elsif($value_type & CF_INET6) {
	my $inet = $argv->[$argi];
	$inet = "::/0" if($inet =~ /^any$/i);
	my ($prefix, $len) =  ($inet =~ /^([a-fA-F\d]{0,4}(?:\:(?!\:\:)[a-fA-F\d]{1,4}){0,6}(?:\:\:)?(?:[a-fA-F\d]{1,4}\:(?!\:\:)){0,6}[a-fA-F\d]{0,4})(?:\/(\d{1,2}))?$/);
	unless(defined($prefix)) {
		# If address format is invalid, it may be a hostname,
		# so we will check it if CF_FQDN is present in the
		# value types bit array.
		if($value_type & CF_FQDN) {
		    my ($res, $resmsg) = $self->parse_data_type($directive_name,
								$value_template,
								$current_section_name,
								$nested_section_name,
								$value_type ^ CF_INET6);
		    return ($res, $res ? $resmsg:"must be either an IPv6 address or a hostname");
		# If CF_FQDN is not present, we got bogus argument
		} else {
		    return (0, "must be a valid IPv6 address or network or \'any\'");
		}
	}
	# CF_ADDR and CF_PREFIX are conflicting modifiers and
	# cannot be present at the same time
	if(($value_type & CF_ADDR) && ($value_type & CF_PREFIX)) {

	    die "Config template error: cannot specify both CF_ADDR and CF_PREFIX at directive \"$directive_name\"\n";

	# If value type is CF_ADDR, but we got prefix argument
	# instead, throw an error and complain.
	} elsif(defined($len) && ($value_type & CF_ADDR)) {

	    return (0, "must be an IPv6 address, not prefix");

	# If prefix len was specified, check if it's ok.
	# If CF_PREFIX modifier is present, argument
	# MUST be a valid prefix
	} elsif((defined($len) && ($len < 0 || $len > 128)) ||
		(!defined($len) && ($value_type & CF_PREFIX))) {
	    return (0, "must have a valid prefix length between 0 and 128");
	}
	# Everything checks out
	$value = $inet;
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
    } elsif($value_type & CF_PORT) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	unless($value =~ /^(\d+)$/ && $value >= 0 && $value <= 65535) {
	    return (0, "must be a port");
	}
    } elsif($value_type & CF_FQDN) {
	$value = $argv->[$argi];
	# Advance argument index unless explicitly told not to
	$argi++ unless $operator & V_OP_NONEXTARG;
	unless($value =~ /^([\w\-\+]+\.)+[a-zA-Z]{2,}$/) {
	    return (0, "must be a fully qualified domain name");
	}
    } else {
	die "Config template error: value template specified unknown value type at directive \"$directive_name\"\n";
    }

    return(1, $value);
}
#
# Modify already parsed value.
#
#  This function modifies values produced by parsing the
#  configuration line arguments. It can add, delete, replace
#  or convert produced values from scalar to hash or array
#  or vice versa.
#
#   Input:	1. object reference, passed implicitly
#		2. modify parameter - it can be a hash or
#		   an array reference or an expression string
#		3. value(s) to be stored - a hash or array
#		   reference or scalar value itself
#		4. directive name
#		5. current section name
#		6. nested section name
#
#   Output:	1. rewritten value, on success
#		   original value, if failed
#
sub modify_value($$$$;$$) {
    my $self = shift;
    my $modify = shift;
    my ($value, $directive_name, $current_section_name, $nested_section_name) = @_;

    # Is modify parametar a hash reference ?
    if(ref($modify) eq "HASH") {

	# If produced values are in the hash,
	# the hash will be modified directly.
	# Otherwise, an anonymous hash will
	# be used to convert original value
	# into a hash.
	my $newval = (ref($value) eq "HASH") ? $value:{};

	# Call ourselves recursively to fill the array
	$value = $newval if $self->store_into_hash(O_OP_OVERWRITE,
						   $newval,
						   $modify,
						   @_);

    # Is modify parametar an array reference ?
    } elsif(ref($modify) eq "ARRAY") {

	# If produced values are in the array,
	my $newval = (ref($value) eq "ARRAY") ? $value:[];
	# Call ourselves recursively to fill the array
	$value = $newval if $self->store_into_array($newval,
						    $modify,
						    @_);
	
    # Is modify parametar an expression string ?
    } elsif(ref($modify) eq "") {

	# If expression is a standalone search&replace regexp,
	# prepend produced value as a lvalue and a regexp operator
	my $expr = '$VALUE =~ '.$modify if($modify =~ /^s\/.*\/.*\/[gmsixpc]+$/);
	# Evaluate the expression string
	$value = $self->eval_expression($expr, @_);

    } else {
	die "Config template error: invalid modify format at directive \"$directive_name\"\n";
    }

    return $value;
}
#
#  Determine what should be stored and how.
#
#  Template entries 'key' and 'subkey' can be literal key names
#  or expressions that allow dynamic key name generation.
#
#  By default, value types that produce value hashes or arrays
#  will be stored as references and scalar values will be stored
#  directly. However, if 'key' or 'subkey' template entries are
#  defined as key_expr => store_expr hashes, they can override
#  the default and explicitly define which parsed value goes
#  into which destination hash's entry. Both key and store
#  expressions allow backreferencing values produced by parsing
#  line arguments.
#
#   Input:	1. object reference, passed implicitly
#		2. hash reference to the operation template
#		3. value(s) to be stored - a hash or array
#		   reference or scalar value itself
#		4. directive name
#		5. current section name
#		6. nested section name
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		2. undef, on success
#		   error message, on error
#
sub store_values($$;$$$$) {
    my $self = shift;
    my $operation_template = shift;
    my ($value, $directive_name, $current_section_name, $nested_section_name) = @_;

    my $errmsg = "directive \"$directive_name\" already defined configuration entry \"";

    ## Pick our destination hash ##

    # Create destination storage space in
    # the temporary hash, if it isn't there.
    unless(defined($self->{'WORK'}{$operation_template->{'hash'}})) {
	$self->{'WORK'}{$operation_template->{'hash'}} = {};
    }
    # This will be our default destination,
    # unless hash recursion is requested
    my $dest_hash = $self->{'WORK'}{$operation_template->{'hash'}};

    # If the value template specifies the destination
    # to be relative to the current section, destination
    # hash depth will mirror recursion depth of the nested
    # configuration sections.
    if($operation_template->{'op'} & O_OP_RELATIVE) {
	# We will start from the top level
	my $recursive_hash = $self->{'WORK'}{$operation_template->{'hash'}};
	# Dive through levels of nested hashes
	foreach my $context (@{$self->{'SECTION_HIERARCHY'}}) {
	    my $ctx_section_name = $context->[CX_NAME];
	    unless(defined($recursive_hash->{$ctx_section_name})) {
		$recursive_hash->{$ctx_section_name} = {};
	    }
	    $recursive_hash = $recursive_hash->{$ctx_section_name};
	}
	# This will be our destination,
	# overriding the default
	$dest_hash = $recursive_hash;
    }

    ## Format values and store them ###

    my ($res, $resmsg) = $self->store_into_hash($operation_template->{'op'},
						$dest_hash,
						$operation_template->{'key'},
						@_);
    return($res, $resmsg) unless($res);

    return 1;
}
#
# Store into hash recursively.
#
#  This function stores values into designated hash entry.
#  If hash key is a hash reference instead of a scalar,
#  its structure, or better say, the hierarchy of elements
#  nested within, will define the structure of data stored
#  into the destination hash.
#
#   Input:	1. object reference, passed implicitly
#		2. operator
#		3. hash reference to the destination hash
#		4. destination hash key
#		5. value(s) to be stored - a hash or array
#		   reference or scalar value itself
#		6. directive name
#		7. current section name
#		8. nested section name
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		2. undef, on success
#		   error message, on error
#
sub store_into_hash($$$$;$$$$) {
    my $self = shift;
    my $op = shift;
    my $dest_hash = shift;
    my $dest_key = shift;
    my ($value, $directive_name, $current_section_name, $nested_section_name) = @_;

    my $errmsg  = "directive \"$directive_name\" already defined configuration entry \"";
    my $errmsg2 = "directive \"$directive_name\" defined invalid hash key \"";

    # If 'key' entry defines reference to a key_expr => store_expr
    # hash, we will process all pairs, storing values into matching
    # destination hash entries.
    if(ref($dest_key) eq "HASH") {

	# Loop through all key_expr => store_expr pairs
	foreach my $key_expr (keys %{$dest_key}) {

	    # Eval expression and use the result as destination hash's key
	    my $hashkey = $self->eval_expression($key_expr, @_);
	    return (0, $errmsg2.$key_expr."\"") unless(defined($hashkey) && $hashkey ne "");

	    # Get expression to be evaluated and stored as value
	    my $store_expr = $dest_key->{$key_expr};
	    # Is store expression is a hash reference ?
	    if(ref($store_expr) eq "HASH") {
		# Create nested hash if it's not already there
		$dest_hash->{$hashkey} = {} unless defined($dest_hash->{$hashkey});
		# Follow the store expr hash entries recursively. Entries
		# will be stored under the current hash key if they point
		# to a store expression strings which will be evaluated and
		# the result will be used as a value to be stored. Otherwise,
		# if entries point to the nested hashes or arrays, recursion
		# will go deeper.
		return $self->store_into_hash($op,
					      $dest_hash->{$hashkey},
					      $store_expr,
					      @_);

	    # Is store expression an array reference ?
	    } elsif(ref($store_expr) eq "ARRAY") {
		# Create nested array if it's not already there
		$dest_hash->{$hashkey} = [] unless defined($dest_hash->{$hashkey});
		# Follow the store expr array entries recursively. Entries
		# will be pushed into the current hash key if they point
		# to a store expression strings which will be evaluated and
		# the result will be used as a value to be stored. Otherwise,
		# if entries point to the nested hashes or arrays, recursion
		# will go deeper.
		return $self->store_into_array($dest_hash->{$hashkey},
					       $store_expr,
					       @_);

	    # Is store expression an expression string ?
	    } elsif(ref($store_expr) eq "") {
	        # Eval expression and use the result as the value to be stored
		my $hashval = $self->eval_expression($store_expr, @_);
		# Store value into the destination hash
		unless($self->store_value($directive_name,
					  $hashval,
					  $op,
					  $dest_hash,
					  $hashkey)) {
		    return(0, $errmsg.$hashkey."\"");
		}
	
	    # Otherwise, it is unsupported
	    } else {
		die "Config template error: invalid key format at directive \"$directive_name\"\n";
	    }
	}

    # If 'key' entry specifies a single expression,
    } else {

	# Get destination hash's first level key. Value template
	# entry 'key' can either be a key directly or an expression
	# that allows backreferencing produced values as hash keys.
	my $hashkey = $self->eval_expression($dest_key, @_);
	return (0, $errmsg2.$dest_key."\"") unless(defined($hashkey) && $hashkey ne "");

	# Store value into the destination hash
	unless($self->store_value($directive_name,
				  $value,
				  $op,
				  $dest_hash,
				  $hashkey)) {
	    return(0, $errmsg.$hashkey."\"");
	}

    }

    return 1;
}
#
# Store into array recursively.
#
#  This function stores values into designated array.
#  If array element is an array or a hash reference instead
#  of a scalar, array's structure, or better say, the hierarchy
#  of elements nested within, will define the structure of data
#  stored into the destination hash. This function is never called
#  directly, but from store_into_hash, as hashes are our primary
#  configuration storage.
#
#   Input:	1. object reference, passed implicitly
#		2. array reference to the destination array
#		4. array reference to the array of element expressions
#		5. value(s) to be stored - a hash or array
#		   reference or scalar value itself
#		6. directive name
#		7. current section name
#		8. nested section name
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
#		2. undef, on success
#		   error message, on error
#
sub store_into_array($$$;$$$$) {
    my $self = shift;
    my $dest_array = shift;
    my $elements = shift;
    my ($value, $directive_name, $current_section_name, $nested_section_name) = @_;

    # Loop through all element expressions
    foreach my $elem_expr (@{$elements}) {
	my $element;
	# Is element expression an array reference ?
	if(ref($elem_expr) eq "ARRAY") {

	    # Create anonymous array for storage
	    $element = [];
	    # Call ourselves recursively to fill the array
	    my ($res, $resmsg) = $self->store_into_array($element,
							 $elem_expr,
							 @_);
	    return ($res, $resmsg) unless $res;

	# Is element expression a hash reference ?
	} elsif(ref($elem_expr) eq "HASH") {

	    # Create anonymous hash for storage
	    $element = {};
	    my ($res, $resmsg) = $self->store_into_hash(O_OP_STORE,
							$element,
							$elem_expr,
							@_);
	    return ($res, $resmsg) unless $res;

	# Is element expression a string ?
	} elsif(ref($elem_expr) eq "") {

	    # Eval expression and use the result as the value to be stored
	    $element = $self->eval_expression($elem_expr, @_);
	    # If expression evaluates to an array, append its elements
	    # to the array in the current context. This doesn't include
	    # cases where the value produced by parsing directive's
	    # argument happens to be an arrayref and is referenced by 
	    # the expression using format template $VALUE. In that case,
	    # the value will be stored in its original form - as an array
	    # reference.
	    if(ref($element) eq "ARRAY" && $element != $value) {
		@{$dest_array} = (@{$dest_array},@{$element});
		next;
	    }
	
	# Otherwise, it is unsupported
	} else {
	    die "Config template error: invalid key format at directive \"$directive_name\"\n";
	}

	# Store value into the destination array
	push @{$dest_array}, $element;
    }

    return 1;
}
#
# Store value into destination hash.
#
#  The value passed to this function will be stored into designated
#  hash entry defined by hash key.
#
#  The way it is stored there is determined by the operator parameter:
#   - it can be stored into the hash entry unless something
#     is already stored there, in which case error is returned
#   - it can be stored into the hash entry unless something
#     is already stored there, in which case it is silently skipped
#   - it can be stored into the hash entry no matter what, possibly
#     overwritting whatever is already stored there.
#   - it can be pushed into the entry, creating an array
#
#   Input:	1. object reference, passed implicitly
#		2. directive name
#		3. value
#		4. operator
#		5. a hash reference to the destination hash
#		6. destination entry hash key
#
#   Output:	1. TRUE, on success
#		   FALSE, on error
# 
sub store_value($$$$$$) {
    my $self = shift;
    my ($directive_name, $value, $op, $dest_hash, $dest_key) = @_;

    # If operator was 'store', store the value
    # unless destination entry is already defined
    if($op & O_OP_STORE) {
	# Do not allow re-defining existing destinations
	return 0 if(defined($dest_hash->{$dest_key}));
	$dest_hash->{$dest_key} = $value;
    # If operator was 'storeonce', store the value
    # only if it wasn't stored already. Else, silently
    # skip it.
    } elsif($op & O_OP_STOREONCE) {
	unless(defined($dest_hash->{$dest_key})) {
	    $dest_hash->{$dest_key} = $value;
	}
    # If operator was 'overwrite' store the value always
    } elsif($op & O_OP_OVERWRITE) {
	$dest_hash->{$dest_key} = $value;
    } else {
	die "Config template error: unknown operator at directive \"$directive_name\"\n";
    }
    return 1;
}
#
# Perform context sensitive action.
#
#   Input:	1. object reference, passed implicitly
#		2. current directive's name
#		3. a reference to a hash of available
#		   directives.
#
#   Output:	1. TRUE, on success
#		   FALSE, on noop
#		2. Optional message to be displayed
#
sub perform_csa($$$) {
    my $self = shift;
    my ($directive_name, $directives) = @_;

    # When context sensitive action is requested,
    # it usually sets the csact flag to prevent
    # actual parsing and passes the line segment
    # from the line start upto the current buffer
    # pointer to the parser. It effectively makes
    # the last (partial) word in the passed line
    # segment the one on which we should perform
    # any context sensitive actions.
    return 0 unless($argi >= $#{$argv});

    # If cursor is standing immediately behind
    # a complete directive name, instead of the
    # requested context sensitive action, we will
    # suggest user to press space to move on to
    # the next argument.
    unless($directive_name ne '' ||
	   $self->{'FLAG_TSPACES'} ||
	   $argi == 0) {
	$self->{'FLAG_COMPLETE'} = 0;
	$self->{'FLAG_HELP'} = 1;
	return (1, "  <[space]>           Move on to the next argument");
    }

    # If context sensitive help was requested,
    if($self->{'FLAG_HELP'}) {
	my $help = "";
	# Search through all the directives in the current section
	foreach my $regex (keys %{$directives}) {
	    my @dirs = split(/[^a-zA-Z0-9\-\_]+/, $regex);
	    foreach my $dir (@dirs) {
		# We are looking for directives that begin
		# with our incomplete directive's name
		if($dir ne '' && $dir =~ /^$directive_name/) {
		    # Get directive's template
		    my $directive_template = $directives->{$regex};
		    # Help will be shown only for directives that
		    # are either global or in their native context
		    if($#{$self->{'SECTION_HIERARCHY'}} >=
		       $#{$self->{'SECTION_HIERARCHY_SAVED'}} ||
		       $self->is_global_directive($directive_template)) {
			my $dirhelp = $directive_template->{'help'};
			# Glue together context sensitive help
			# for each matching directive.
			$help .= sprintf("  %-20s%s\n",
				         $dir,
				         defined($dirhelp) ? $dirhelp:"");
		    }
		}
	    }
	}
	# Remove trailing newline
	chop($help);
	# If there is anything to display,
	# return context sensitive help message
	return(1, $help) if($help ne "");
	
	# If command completion was requested,
    } elsif($self->{'FLAG_COMPLETE'}) {
	# Search for directives matching given directive name pattern
	my @compl = $self->match_keys($directive_name, $directives);
	# Any completion candidates found ?
	if(@compl) {
	    # We can complete current command only if
	    # there is a single completion candidate.
	    if(scalar(@compl) == 1) {
		# Complete current word and we are done.
		$self->context_complete(shift(@compl));
		return 1;
	    # Otherwise, when there is more than one possible completion,
	    } else {
		# we will change context sensitive action
		# from completion to context sensitive help,
		$self->{'FLAG_COMPLETE'} = 0;
		$self->{'FLAG_HELP'} = 1;
		# and we will perform the action itself
		return $self->perform_csa(@_);
	    }
	}
    }

    return 0;
}
#
# Return context sensitive help for global directives.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. help string, on success
#		   undef, on noop
#
sub global_cshelp($) {
    my $self = shift;
    my $help;

    # Noop if context sensitive help wasnt requested
    return undef unless $self->{'FLAG_HELP'};

    # Loop through section hierarchy upwards,
    # until we reach the top section
    while($self->section_context_up) {
	# Get section context
	my $context = $self->section_context_get;
	# Search for global directives within each section context
	while(my ($regex, $template) = each %{$context->[CX_TEMPLATE]{'dirs'}}) {
	    # If this is a global directive,
	    # we want to show its help text
	    if($self->is_global_directive($template)) {
		# If directive name is a regex, we will
		# attempt to show it in human readable
		# form.
		my @dirs = split(/[^a-zA-Z0-9\-\_]+/, $regex);
		# This is directive's help message
		my $dirhelp = $template->{'help'};
		# We will display everything 
		# we got from splitting regexp 
		foreach my $dir (@dirs) {
		    # Skip empty strings
		    next unless($dir ne '');
		    # Glue together context sensitive help
		    # for each matching directive.
		    $help .= sprintf("  %-20s%s\n",
				     $dir,
				     defined($dirhelp) ? $dirhelp:"");
		}
	    }
	}
    }
    return $help;
}
#
# Evaluate expression string.
#
#  This function evaluates expression given as a string. Evaluation
#  is done with restricted set if allowed operators, using Safe module.
#  Since the only variables in these expressions can be our format
#  templates and application's function and constant names, we have to
#  substitute them with their values before evaluating the expression.
#  After substitution, expression passed to reval() should contain only
#  numbers, strings and regexps. No variable, Perl's or our own should
#  remain in the expression string.
#
#  Return value will be the result of the evaluation directly. In case
#  evaluation failed for whatever reason, result will be undefined.
#
#   Input:	1. object reference, passed implicitly
#		2. expression string
#		3. produced values - a reference to a hash or
#		   an array of values, or a scalar value itself
#		4. current directive name
#		5. current section name
#		6. nested section name
#
#   Output:	1. result of the expression eval, or
#		   undef, if evaluation failed.
#
sub eval_expression($$$$;$$) {
    my $self = shift;
    my $expr = shift;

    # Expression must be passed in the string form,
    # which means we are expecting a scalar value.
    # If we got something other than a scalar, we
    # will just return it to the caller as is
    return $expr unless ref($expr) eq "";

    # If expression is a single format template,
    # there's no need to do expression eval
    if($expr =~ /^(?<!\\)(\$\w+(?:\[[\+\-]?\d+\]|\{\w+\}){0,2})$/) {
	return $self->format_template($expr, @_);
    }

    # Array shared with Safe compartment
    # in which our expression is evaluated.
    local @evar = ();

    # This private function will xlat format templates in
    # the expression string, store results in the array
    # shared with Safe compartment in which expression
    # is evaluated, and replace each format template in
    # the expression string with perl variables that
    # reference corresponding shared array elements
    my $replace = sub {
	push @evar, $self->format_template($1, @_);
	return '$evar['.$#evar.']';
    };
    #print "before expr=\"$expr\"\n";
    # Search for format templates in the expression string
    # and replace them with perl variables that reference
    # elements of the array shared with Safe compartment
    # in which expression is evaluated.
    $expr =~ s/(?<!\\)(\$\#?\w+(?:\[\d+\]|\{\w+\}){0,2})/&$replace/eg;
    # Share array with Safe compartment
    #$self->{'EVAL'}->share_from('Config::ContextSensitive',['@evar']);
    $self->{'EVAL'}->share_from(ref($self), ['@evar']);
    # Evaluate expression in a sandbox
    my $res = $self->{'EVAL'}->reval($expr);
    #print "after expr=\"$expr\" res=\"$res\"\n";
    # For failed evals, return expression itself
    $res = $expr unless defined($res);
    # If expression was a search and replace operation,
    # we will return the modified string. Otherwise,
    # we will directly return the result of the expression
    # evaluation.
    return ($expr =~ /^\s*\S+\s*\=\~\s*s\/.*\/.*\/[gmsixpc]+$/) ? $evar[0]:$res;
}
#
# Format hash keys or values to be stored.
#
#  A template entry (a key or a value) is passed to this function
#  to be formatted if neccessary. If template entry is a format
#  template, it will be formatted, or, better say, format template
#  will be replaced with actual value. Substitution is based on a
#  couple of simple rules. This gives us the flexibility to choose
#  exactly what value goes where and how. If template entry is not
#  a format template, but a value itself, it is returned unmodified.
#
#   Input:	1. object reference, passed implicitly
#		2. format template
#		3. produced values - a reference to a hash or
#		   an array of values, or a scalar value itself
#		4. current directive name
#		5. current section name
#		6. nested section name
#
#   Output:	1. formatted value, if input was a format template
#		   original value, if input was a normal value
#
sub format_template($$;$$$$) {
    my $self = shift;
    my ($format, $value, $directive_name, $current_section_name, $nested_section_name) = @_;

    # Which of these 4 variants can be used depends on
    # the actual value type. Some value types implicitly
    # define value arrays, others value hashes, or even
    # hashes of arrays or arrays of hashes. Wrong format
    # for the given value type will yield undef.
    my $errmsg = "Config template error: format template \"$format\" assumed wrong data type at directive \"$directive_name\"\n";
    # If template entry is specified as $VALUE, resulting
    # scalar value or a hash/array  reference will be used.
    if($format eq '$VALUE') {
	return $value;
    # If template entry is specified as $VALUES{NAME},
    # resulting parsed value will be taken from a hash
    # entry named NAME.
    #
    # If template entry is specified as $VALUES{NAME}[N],
    # Nth value from the array stored in the hash entry
    # named NAME will be used.
    #
    # If template entry is specified as $VALUES{NAME}{SUBNAME},
    # resulting parsed value will be taken from a hash
    # entry defined by key NAME and subkey SUBNAME.
    } elsif($format =~ /^\$VALUES\{(\w+)\}(?:\{(\w+)\}|\[(\d+)\])?$/) {
	return undef unless defined($value);
	if(defined($1)) {
	    die $errmsg unless(ref($value) eq "HASH");
	    if(defined($2)) {
		die $errmsg unless(ref($value->{$1}) eq "HASH");
		return $value->{$1}{$2};
	    }
	    if(defined($3)) {
		die $errmsg unless(ref($value->{$1}) eq "ARRAY");
		return $value->{$1}[$3];
	    }
	    return $value->{$1};
	}
    # If template entry is specified as $VALUES[N]
    # resulting Nth parsed value will be taken from
    # the array of produced values.
    #
    # If template entry is specified as $VALUES[N]{NAME}
    # resulting value will be taken from a hash entry
    # defined by key NAME in the hash which is an Nth
    # element of the array of produced values.
    #
    # If template entry is specified as $VALUES[N][M]
    # resulting value will come from Mth element of
    # the Nth array of produced values.
    } elsif($format =~ /^\$VALUES\[(\d+)\](?:\[(\d+)\]|\{(\w+)\})?$/) {
	return undef unless defined($value);
	if(defined($1)) {
	    die $errmsg unless(ref($value) eq "ARRAY");
	    if(defined($2)) {
		die $errmsg unless(ref($value->[$1]) eq "ARRAY");
		return $value->[$1][$2];
	    }
	    if(defined($3)) {
		die $errmsg unless(ref($value->[$1]) eq "HASH");
		return $value->[$1]{$2};
	    }
	    return $value->[$1];
	}
    # If template entry is specified as $VALUES,
    # all produced values of an array or a hash
    # will be returned in the form of a reference
    # to the array of values. If value is a scalar,
    # it will be used as is. Note that if the value
    # is an array, the returned reference points to
    # the new anonymous array, not the original one.
    } elsif($format eq '$VALUES') {
	if(ref($value) eq "HASH") {
	    return [values %{$value}];
	} elsif(ref($value) eq "ARRAY") {
	    return [@{$value}];
	} else {
	    return $value;
	}
    # If format template is specified as $KEYS,
    # an array of hash keys will be used if
    # value was a hash. If value was an
    # array, an array will be used (identical
    # to $VALUES). If value is a scalar, 
    # it will be used as is
    } elsif($format eq '$KEYS') {
	if(ref($value) eq "HASH") {
	    return [keys %{$value}];
	} elsif(ref($value) eq "ARRAY") {
	    return [@{$value}];
	} else {
	    return $value;
	}
    # If format template is specified as $#VALUES,
    # number of hash values or array elements is
    # used if the value is a hash or an array,
    # respectively. If the value is a scalar,
    # 1 is returned
    } elsif($format eq '$#VALUES') {
	if(ref($value) eq "HASH") {
	    return scalar values %{$value};
	} elsif(ref($value) eq "ARRAY") {
	    return $#{$value}+1;
	} else {
	    return 1;
	}
    # If format template is specified as $#KEYS,
    # number of hash keys or array elements is
    # used if the value is a hash or an array,
    # respectively. If the value is a scalar,
    # 1 is returned
    } elsif($format eq '$#KEYS') {
	if(ref($value) eq "HASH") {
	    return scalar keys %{$value};
	} elsif(ref($value) eq "ARRAY") {
	    return $#{$value}+1;
	} else {
	    return 1;
	}
    # If template entry is specified as $ARG,
    # last processed argument will be used.
    } elsif($format eq '$ARG') {
	return $argl;
    # If template entry is specified as $ARG[N],
    # original, unparsed Nth argument from the current
    # line will be used.  If N is negative, it is used
    # as offset from the end of the current argument
    # line.
    } elsif($format =~ /^\$ARG\[([\+\-])?(\d+)\]$/) {
	# Total argument line size
	my $c = $#{$argv}+1;
	# Calculate index of the argument to return
	my $i = defined($1) ?
		    (($1 eq '-') ? ($c-$2):$2):$2;
	return ($i < 0) ? undef:$argv->[$i];
    # If template entry is specified as $ARGR[N],
    # original, unparsed Nth argument from the current
    # line will be used, where N is an offset, either
    # positive or negative, relative to the current
    # argument index.
    } elsif($format =~ /^\$ARGR\[([\+\-])?(\d+)\]$/) {
	# Our current argument index
	my $j = $argi-1;
	# Calculate argument index to return
	my $i = defined($1) ?
		    (($1 eq '-') ? ($j-$2):($j+$2)):($j+$2);
	return ($i < 0) ? undef:$argv->[$i];
    # If template entry is specified as $ARGI
    # current argument index will be used.
    } elsif($format eq '$ARGI') {
	return $argi ? ($argi-1):0;
    # If template entry is specified as $ARGN
    # next/pending argument index will be used.
    } elsif($format eq '$ARGN') {
	return $argi;
    # If template entry is specified as $ARGC
    # current argument count will be used.
    } elsif($format eq '$ARGC') {
	return $#{$argv}+1;
    # If template entry is specified as $DIRECTIVE
    # main directive name will be used.
    } elsif($format eq '$DIRECTIVE') {
	return $directive_name;
    # If template entry is specified as $SECTION[N],
    # the name of the section at the Nth depth level
    # of the section context hierarchy will be used.
    # If N is negative, it is used as offset from the
    # bottom of the section context hierarchy.
    } elsif($format =~ /^\$SECTION\[([\+\-])?(\d+)\]$/) {
	# Total section context depth
	my $d = $#{$self->{'SECTION_HIERARCHY'}}+1;
	# Calculate the section context hierarchy
	# position we want to return
	my $i = defined($1) ?
		    (($1 eq '-') ? ($d-$2):$2):$2;
	return ($i < 0) ? undef:$self->{'SECTION_HIERARCHY'}[$i][CX_NAME];
    # If template entry is specified as $SECTION
    # current section name will be used if it was
    # defined.
    } elsif($format eq '$SECTION') {
	return $current_section_name;
    # If template entry is specified as $NESTED_SECTION
    # nested section name (that is about to be parsed),
    # will be used if it was defined.
    } elsif($format eq '$NESTED_SECTION') {
	return $nested_section_name;
    # If template entry is specified as $PARENT_SECTION
    # parent section name  will be used if it was defined.
    } elsif($format eq '$PARENT_SECTION') {
	my $parent = $self->section_context_parent();
	return $parent->[CX_NAME];
    # Otherwise, whatever template entry is, will be
    # used directly
    } else {
	return $format;
    }
    return undef;
}

#############################################################################################
################################ M A I N   C L I   C O D E ##################################

#
# Read input from the CLI.
#
#  This function reads input from tty on which CLI is running,
#  char (sequence) by char (sequence) and assembles the command
#  line. When linefeed is encountered unescaped, the command line
#  is split into argument array, fit into cfline 'structure' and
#  returned to the caller.
#
#   Input:	1. object reference, passed implicitly
#
#   Output:     1. If line is complete: reference to cfline;
#		   If line is not complete: FALSE;
#		   On error: undef
#
sub read_cli_input($) {
    my $self = shift;
    my $res = 0;
    my $line;
    my $c;

    # Get a key
    sysread($self->{'TTY'}, $c, 1);
    return undef unless defined($c);
    #print "got ".ord($c)."\n";
    # Is it an escape sequence ?
    if($c eq ESC) {
	# assemble entire escape sequence
	my $k;
	sysread($self->{'TTY'}, $k, 1);
	return undef unless defined($k);
	return 0 unless($k eq "[");
	for($c .= $k; $k !~ /^[a-zA-Z]$/; $c .= $k) {
	    sysread($self->{'TTY'}, $k, 1);
	    return undef unless defined($k);
	}
	# Ignore unknown escape sequences
	return 0 unless defined($INPUT_EVENT{$c});
    }

    # If we have a handler for this
    if(defined($INPUT_EVENT{$c})) {
	# Call handler function for this terminal edit event.
	# When full line has been processed, handlers will
	# return a reference to the argument array. Otherwise,
	# they should return true if they successfully processed
	# character sequence, or false, if they failed, skipped
	# or partially handled the character sequence. Failed or
	# incomplete character sequences will be caught by our
	# default handler
	($res, $line) = $INPUT_EVENT{$c}->($self);
	# If we have a complete argument array,
	if($res && defined($line)) {
	    # Split completed line into argument array
	    my $argv = $self->split_config_line($line);
	    # If line ends with spaces, user started
	    # a new argument, which is empty at this
	    # point, so we will set the flag to make
	    # the parsing code aware of this.
	    $self->{'FLAG_TSPACES'} = ($line =~ /\s+$/);
	    # Create cfline and return it to the caller
	    return $self->cfline_new(fileno($self->{'TTY'}),
				     1,
				     $argv);
	}
    }

    # Default terminal event handler. If the specific
    # terminal event handler didn't process the current
    # character sequence, we will process it ourselves,
    # ignoring unknown special characters in the process.
    unless($res || ord($c) < 0x20) {
	# Put character into line buffer
	$self->buff_putstring($c);
	# Write character to terminal
	$self->term_putchar($c);
	# Reset the backslash escape flag
	if($self->{'FLAG_ESC'} && $c ne "\\") {
	    $self->{'FLAG_ESC'} = 0 
	}
    }

    return 0;
}

#############################################################################################
####################### T E R M I N A L   I N P U T   H A N D L E R S #######################

sub input_noop($) {
    # Don't do anything if this character
    # or escape sequence is supposed to
    # be ignored, just return success.
    # Ignored character sequences produce
    # no output in the terminal window.
    return 1;
}

sub input_cursorleft($) {
    my $self = shift;
    my $off = shift;

    if(defined($off)) {
	# Noop on null offsets
	return 1 unless $off;
    } else {
	# Default offset is 1 char
	$off = 1;
    }

    # Adjust offset never to exceed line start
    if($self->{'LINE_PTR'} - $off < 0) {
	$off = $self->{'LINE_PTR'};
    }

    # step back $off chars within the current line
    $self->{'LINE_PTR'} -= $off;
    # move cursor one cell to the left
    $self->term_cursorleft($off);

    return 1;
}

sub input_cursorright($) {
    my $self = shift;
    my $off = shift;

    if(defined($off)) {
	# Noop on null offsets
	return 1 unless $off;
    } else {
	# Default offset is 1 char
	$off = 1;
    }

    # Adjust offset never to exceed line end
    if($self->{'LINE_PTR'} + $off > $self->{'LINE_LEN'}) {
	$off = $self->{'LINE_LEN'} - $self->{'LINE_PTR'};
    }

    # Noop on null offsets
    return 1 unless $off;

    # step forward N chars within the current line
    $self->{'LINE_PTR'} += $off;
    # move cursor one cell to the right
    $self->term_cursorright($off);

    return 1;
}

sub input_linestart($) {
    my $self = shift;
    return $self->input_cursorleft($self->{'LINE_PTR'});
}

sub input_lineend($) {
    my $self = shift;
    return $self->input_cursorright($self->{'LINE_LEN'});
}

sub input_backspace($) {
    my $self = shift;
    # Noop if we are at the beginning of the line.
    return 1 unless($self->{'LINE_PTR'});
    # Step back one position within the current line
    $self->{'LINE_PTR'}--;
    # Reduce the line length by 1
    $self->{'LINE_LEN'}--;
    # Remove character at new position
    substr($self->{'LINE_BUFFER'}, $self->{'LINE_PTR'}, 1, "");
    # Repaint
    $self->term_cursorleft;
    $self->term_clrtoeol;
    if($self->{'LINE_PTR'} < $self->{'LINE_LEN'}) {
	$self->term_insertline($self->{'LINE_PTR'},
		$self->{'LINE_LEN'} - $self->{'LINE_PTR'});
    }
    return 1;
}

sub input_delete($) {
    my $self = shift;
    # Noop if we are at the end of the line.
    return 1 unless ($self->{'LINE_PTR'} < $self->{'LINE_LEN'});
    # Reduce the line length by 1
    $self->{'LINE_LEN'}--;
    # Remove character at current position
    substr($self->{'LINE_BUFFER'}, $self->{'LINE_PTR'}, 1, "");
    # Repaint
    $self->term_clrtoeol;
    if($self->{'LINE_PTR'} < $self->{'LINE_LEN'}) {
	$self->term_insertline($self->{'LINE_PTR'},
		$self->{'LINE_LEN'} - $self->{'LINE_PTR'});
    }
    return 1;
}

sub input_replaceword($;$) {
    my $self = shift;
    my $repl = shift;

    # Set buffer pointer to the beginning of the last word
    my ($wordpos, $wordlen) = $self->buff_prevword;
    # Don't bother if cursor position hasn't changed
    return 1 unless($wordlen);
    my $replen = defined($repl) ? length($repl):0;
    # Replace between buffer pointer
    # and the word delimiter
    substr($self->{'LINE_BUFFER'}, $wordpos, $wordlen, $replen ? $repl:"");
    # Set new cursor position
    $self->{'LINE_PTR'} = $wordpos;
    # Set new line length
    $self->{'LINE_LEN'} -= $wordlen - $replen;
    # Move cursor back and repaint
    $self->term_cursorleft($wordlen);
    $self->term_clrtoeol;
    if($self->{'LINE_PTR'} < $self->{'LINE_LEN'}) {
	# If replacement string is not 0-size
	if($replen) {
	    # Display replacement first, and place
	    # cursor at the end of it.
	    $self->term_putline($self->{'LINE_PTR'}, $replen);
	    # Set new cursor position
	    $self->{'LINE_PTR'} += $replen;
	}
	# Redraw the rest of the line
	$self->term_insertline($self->{'LINE_PTR'},
		$self->{'LINE_LEN'} - $self->{'LINE_PTR'});
    }
    return 1;
}


sub input_clrtoeol($) {
    my $self = shift;
    # How many chars to cut out
    my $cutlen = $self->{'LINE_LEN'} - $self->{'LINE_PTR'};
    # Don't bother if we are at the end of line
    return 1 unless($cutlen);
    # Set new line length
    $self->{'LINE_LEN'} -= $cutlen;
    # Delete from the buffer pointer
    # to the end of the line
    $self->{'LINE_BUFFER'} = substr($self->{'LINE_BUFFER'}, 0, $self->{'LINE_PTR'});
    # Clear to the end of the line
    $self->term_clrtoeol;
    return 1;
}

sub input_clrline($) {
    my $self = shift;
    # clear line
    $self->term_clrline;
    # move cursor to the beginning of the line
    $self->term_linestart;
    # repaint prompt
    $self->term_prompt;
    # reset line buffer
    $self->{'LINE_BUFFER'} = '';
    $self->{'LINE_LEN'} = 0;
    $self->{'LINE_PTR'} = 0;
    return 1;
}

sub input_clrscreen($) {
    my $self = shift;
    # Clear screen
    $self->term_clrscreen;
    # Repaint the prompt
    $self->term_prompt;
    # Repaint the current line
    $self->input_putline;
    return 1;
}

sub input_putline($) {
    my $self = shift;
    my $pos;

    $self->input_linestart;
    # Length of the line segment from current
    # buffer pointer upto the end of the line
    my $len = ($self->{'LINE_LEN'} < $self->{'INPUT_WIDTH'}) ?
		$self->{'LINE_LEN'}:$self->{'INPUT_WIDTH'};
    # Set new buffer pointer
    $self->{'LINE_PTR'} = $self->{'LINE_LEN'} - $len;
    # Write line segment on the line
    $self->term_putline($self->{'LINE_PTR'}, $len);
    # Set new buffer pointer
    $self->{'LINE_PTR'} = $self->{'LINE_LEN'};
    # Clear up to the end of the line
    $self->term_clrtoeol;
    return 1;
}

sub input_linefeed($) {
    my $self = shift;

    # Move cursor to the next line
    $self->term_newline;

    my $line = $self->{'LINE_BUFFER'};

    # If line was empty or linefeed
    # was escaped, do not process it,
    if($line eq "" || $self->{'FLAG_ESC'}) {
	# just repaint the prompt
	$self->term_prompt;
	# and reset escape flag
	$self->{'FLAG_ESC'} = 0;
	return 1;
    }

    # reset current line
    $self->{'LINE_BUFFER'} = '';
    $self->{'LINE_SAVE'} = undef;
    # reset line length
    $self->{'LINE_LEN'} = 0;
    # return cursor to the start
    $self->{'LINE_PTR'} = 0;
    # reset single and double quote flags
    $self->{'FLAG_SQ'} = 0;
    $self->{'FLAG_DQ'} = 0;

    # If line from the history has changed
    if(!defined($self->{'HISTORY'}[$self->{'HIST_INDEX'}]) ||
	$line ne $self->{'HISTORY'}[$self->{'HIST_INDEX'}]) {
	# Put the line into history
	push @{$self->{'HISTORY'}}, $line;
	# Set history index to the last saved line
	$self->{'HIST_INDEX'} = $#{$self->{'HISTORY'}};
    }

    return (1, $line);
}

sub input_historyprev($) {
    my $self = shift;

    # Don't do anything if history is empty
    return 1 unless($#{$self->{'HISTORY'}}+1);

    # If line from the history has changed
    if(!defined($self->{'LINE_SAVE'})) {

	# Save current incomplete line
	$self->{'LINE_SAVE'} = $self->{'LINE_BUFFER'};
	# Set history line as current
	$self->{'LINE_BUFFER'} = $self->{'HISTORY'}[$self->{'HIST_INDEX'}];

    # If we are not at the first entry,
    } elsif($self->{'HIST_INDEX'}) {
	# move one entry 'up'
	$self->{'HIST_INDEX'}--;
	# Set history line as current
	$self->{'LINE_BUFFER'} = $self->{'HISTORY'}[$self->{'HIST_INDEX'}];
	# otherwise,
    } else {
	# cycle the history index back
	# to the last entry
	$self->{'HIST_INDEX'} = $#{$self->{'HISTORY'}};
	# Restore incomplete line as current
	$self->{'LINE_BUFFER'} = $self->{'LINE_SAVE'};
	# Clear incomplete line buffer
	$self->{'LINE_SAVE'} = undef;
    }

    $self->{'LINE_LEN'} = length($self->{'LINE_BUFFER'});
    # Repaint the line
    $self->input_putline;

    return 1;
}

sub input_historynext($) {
    my $self = shift;

    # Don't do anything if history is empty
    return 1 unless($#{$self->{'HISTORY'}}+1);

    # If line from the history has changed
    if(!defined($self->{'LINE_SAVE'})) {

	# Save current incomplete line
	$self->{'LINE_SAVE'} = $self->{'LINE_BUFFER'};
	# Put us at the top of the history list
	$self->{'HIST_INDEX'} = 0;
	# Set history line as current
	$self->{'LINE_BUFFER'} = $self->{'HISTORY'}[0];

    # If we are not at the first entry,
    } elsif($self->{'HIST_INDEX'} < $#{$self->{'HISTORY'}}) {
	# move one entry 'down'
	$self->{'HIST_INDEX'}++;
	# Set history line as current
	$self->{'LINE_BUFFER'} = $self->{'HISTORY'}[$self->{'HIST_INDEX'}];
	# otherwise,
    } else {
	# Restore incomplete line as current
	$self->{'LINE_BUFFER'} = $self->{'LINE_SAVE'};
	# Clear incomplete line buffer
	$self->{'LINE_SAVE'} = undef;
    }

    $self->{'LINE_LEN'} = length($self->{'LINE_BUFFER'});
    # Repaint the line
    $self->input_putline;

    return 1;
}

sub input_help($) {
    my $self = shift;

    # Return noop if ? was escaped or under quotes
    if($self->{'FLAG_SQ'} || $self->{'FLAG_DQ'} || $self->{'FLAG_ESC'}) {
	return 0;
    }

    # Set context sensitive action flag
    $self->{'FLAG_CSA'} = 1;
    # Set context sensitive help flag
    $self->{'FLAG_HELP'} = 1;

    return (1, substr($self->{'LINE_BUFFER'}, 0, $self->{'LINE_PTR'}));
}

sub input_completion($) {
    my $self = shift;

    # Return noop if TAB was escaped or under quotes
    if($self->{'FLAG_SQ'} || $self->{'FLAG_DQ'} || $self->{'FLAG_ESC'}) {
	return 0;
    }

    # If we are at the beginning of the line,
    # report success, but do nothing
    return 1 unless $self->{'LINE_PTR'};

    # Check where the buffer pointer is ...
    my $s = substr($self->{'LINE_BUFFER'}, $self->{'LINE_PTR'} - 1, 2);
    my @c = unpack("(a)*", $s);

    # Dont try to complete if the buffer pointer
    # is not at the end of the word.
    return 1 unless($c[0] ne ' ' && (!defined($c[1]) || $c[1] eq ' '));

    # Set context sensitive action flag
    $self->{'FLAG_CSA'} = 1;
    # Set command completion flag
    $self->{'FLAG_COMPLETE'} = 1;

    return (1, substr($self->{'LINE_BUFFER'}, 0, $self->{'LINE_PTR'}));
}


sub input_interrupt($) {
    my $self = shift;
    # On ctrl-c, send SIGINT to ourselves
    #kill 2, $$;
    $self->end;
    return 1;
}

sub input_singlequotes($) {
    my $self = shift;
    # Return noop if current char is a single quote
    # that is escaped or inside open double quotes
    unless($self->{'FLAG_DQ'} || $self->{'FLAG_ESC'}) {
	# Toggle single quotes flag
	$self->{'FLAG_SQ'} ^= 1;
    }
    return 0;
}

sub input_doublequotes($) {
    my $self = shift;
    # Return noop if current char is a double quote
    # that is escaped or inside open single quotes
    unless($self->{'FLAG_SQ'} || $self->{'FLAG_ESC'}) {
	# Toggle double quotes flag
	$self->{'FLAG_DQ'} ^= 1;
    }
    return 0;
}

sub input_backslash($) {
    my $self = shift;
    # Toggle backslash escape flag
    $self->{'FLAG_ESC'} ^= 1;
    return 0;
}

#############################################################################################
################## B U F F E R   M A N A G E M E N T   F U N C T I O N S ####################

#
#   Input:	1. object reference passed implicitly
#		2. string to be inserted
#		3. length of the string (optional)
#		4. offset (aka buffer pointer) into
#		   the current line buffer (optional)
#
#   Output:	1. length of the inserted string
#
sub buff_insertstring($$;$$) {
    my $self = shift;
    my $str = shift;
    my ($len, $pos) = @_;

    if(defined($len)) {
	$str = substr($str, $len);
    } else {
	# Default length is the full length of the string
	$len = length($str);
    }
    # Default length is the full length of the string
#    $len = length($str) unless defined($len);
    # Default buffer ptr is the current buffer pointer
    $pos = $self->{'LINE_PTR'} unless defined($pos);

    # If we are at the beginning of the line buffer,
    # we can just prepend the string.
    if(!$pos) {
	    # Prepend string to the line buffer
	    $self->{'LINE_BUFFER'} = $str.$self->{'LINE_BUFFER'};
    # If the buffer pointer is positioned in the middle
    # of the buffer, we will have to insert the string.
    } elsif($pos < $self->{'LINE_LEN'}) {
	    # Insert the string into the current line buffer
	    my $insstr = $str.substr($self->{'LINE_BUFFER'}, $pos);
	    my $inslen = $self->{'LINE_LEN'} - $pos + $len;
	    substr($self->{'LINE_BUFFER'}, $pos, $inslen, $insstr);
    # If we are at the end of line buffer,
    # we can just append string.
    } else {
	    # Append string to the line buffer
	    $self->{'LINE_BUFFER'} .= $str;
    }
    # Increase line lenght
    $self->{'LINE_LEN'} += $len;
    # Return the length of the inserted string
    return $len;
}
#
#   Input:	1. object reference passed implicitly
#		2. string to be inserted
#		3. length of the string (optional)
#		4. offset (aka buffer pointer) into
#		   the current line buffer (optional)
#
#   Output:	1. length of the put string
#
sub buff_putstring($$;$$) {
    my $self = shift;
    my $len = $self->buff_insertstring(@_);
    # Advance buffer pointer
    $self->{'LINE_PTR'} += $len;
    # Return the length of the put string
    return $len;
}
#
#  Output (scalar):	1. preceeding word
#	  (list):	1. buffer pointer to the beginning
#			   of the preceeding word.
#			2. length of the preceeding word
#			3. preceeding word
#
#	  (if failed):	1. undef if no preceeding word exists
#			   (already at the beginning of the buffer)
#
sub buff_prevword($) {
    my $self = shift;
    my ($wordpos, $prevpos);

    # Don't bother if we are at the beginning of the buffer
    return undef unless($self->{'LINE_PTR'});

    # Skip trailing whitespaces
    for($wordpos = $self->{'LINE_PTR'}, $prevpos = $wordpos--;
	$wordpos && substr($self->{'LINE_BUFFER'}, $wordpos, 1) eq " ";
	$wordpos--) {}

    if($wordpos) {
	# Find previous occurance of whitespace which is a word delimiter,
	# starting from our current buffer position
	my $newpos = rindex($self->{'LINE_BUFFER'}, " ", $wordpos);
	#print "newpos=$newpos\n";
	# Set new buffer pointer to the beginning of previous word
	$wordpos = ($newpos > 0) ? $newpos+1:0;
    }

    # In list context return buffer pointer to the beginning
    # of the previous word and the number of characters we 
    # skipped back.
    # In scalar context, just return the buffer pointer
    # to the beginning of the previous word.
    my $wordlen = $prevpos - $wordpos;
    my $word = substr($self->{'LINE_BUFFER'},$wordpos, $wordlen);
    return wantarray ? ($wordpos, $wordlen, $word):$word;
}

#############################################################################################
################### T E R M I N A L   D I S P L A Y   F U N C T I O N S #####################

sub term_resetsize($) {
    my $self = shift;
    my $winsize = '';
    # Get new terminal window size
    ioctl($self->{'TTY'}, &TIOCGWINSZ, $winsize);
    my ($rows, $cols, $xpix, $ypix) = unpack('S4', $winsize);
    # Configure our work space
    $self->term_setsize($cols, $rows);
}

sub term_setsize($$$) {
    my $self = shift;
    my ($cols, $rows) = @_;

    # Noop if dimensions are not defined properly
    return unless($cols && $rows);

    # Save terminal window params
    $self->{'TERM_COLS'} = $cols;
    $self->{'TERM_ROWS'} = $rows;
    # Adjust our work space params
    $self->{'INPUT_WIDTH'} = $self->{'TERM_COLS'} - $self->{'PROMPT_LEN'} - 10;
}

sub term_prompt($) {
    my $self = shift;
    # Noop if CLI is going (or is already) down
    return unless $self->is_cli;
    # move cursor to the beginning of the terminal line
    $self->term_cr;
    # display prompt
    syswrite($self->{'TTY'}, $self->{'PROMPT'}, $self->{'PROMPT_LEN'});
    # Reset cursor position
    $self->{'CURSOR_X'} = 0;
}

sub term_cr($) {
    my $self = shift;
    # move cursor to the beginning of the terminal line
    syswrite($self->{'TTY'}, TCMD_CR, 1);
}

sub term_newline($) {
    my $self = shift;
    # Send CRLF to the terminal
    syswrite($self->{'TTY'}, TCMD_CRLF, 2);
}

sub term_logout($) {
    my $self = shift;
    # Send EOT to the terminal
    syswrite($self->{'TTY'}, EOT, 1);
}

sub term_cursorleft($) {
    my $self = shift;
    my $off = shift;
    my $delta = 0;

    if(defined($off)) {
	# Noop on null offsets
	return unless $off;
    } else {
	# Default offset is 1 char
	$off = 1;
    }
    # Look for the horizontal page break
    my $hpagebreak = ($self->{'LINE_PTR'} && !($self->{'LINE_PTR'} % $self->{'INPUT_WIDTH'}));
    # If cursor is not at the left edge of the terminal,
    # or the current horizontal page break ...
    if($self->{'CURSOR_X'} && !$hpagebreak) {
	# Our cursor delta cannot exceed
	# the left edge of the terminal.
	$delta = ($off < $self->{'CURSOR_X'}) ? $off:$self->{'CURSOR_X'};
	if($delta) {
	    # Move cursor delta characters to the left
	    if($delta > 1) {
		syswrite($self->{'TTY'}, "\x1b[".$delta."D");
	    } else {
		syswrite($self->{'TTY'}, "\b"x$delta, $delta);
	    }
	    # Set new cursor position
	    $self->{'CURSOR_X'} -= $delta;
	}
    }
    # Scroll the line right if cursor hasn't moved
    # because it has reached either the left edge
    # of the terminal window or a horizontal page
    # delimiter.
    if($delta < $off) {
	# Scroll entire horizontal page if possible
	if($hpagebreak) {
	    # Move cursor to the beginning of the terminal window
	    syswrite($self->{'TTY'}, "\x1b[".($self->{'CURSOR_X'})."D");
	    # Reset cursor position
	    $self->{'CURSOR_X'} = 0;
	    # Write the current horizontal page
	    $self->term_putline($self->{'LINE_PTR'} - $self->{'INPUT_WIDTH'});
	# Otherwise, scroll one character at a time
	} else {
	    $self->term_scrollright;
	}
    }
}

sub term_cursorright($) {
    my $self = shift;
    my $off = shift;
    my $delta = 0;

    if(defined($off)) {
	# Noop on null offsets
	return unless $off;
    } else {
	# Default offset is 1 char
	$off = 1;
    }
    # Look for the horizontal page break
    my $len = $self->{'LINE_LEN'} - $self->{'LINE_PTR'};
    my $hpagebreak = ($len && !($len % $self->{'INPUT_WIDTH'}));
    # If cursor is not at the right edge of the terminal,
    # or the current horizontal page break ...
    if($self->{'CURSOR_X'} < $self->{'INPUT_WIDTH'} && !$hpagebreak) {
	# Our cursor delta cannot exceed
	# the right edge of the terminal.
	$delta = ($self->{'CURSOR_X'} + $off < $self->{'INPUT_WIDTH'}) ?
		    $off:($self->{'INPUT_WIDTH'} - $self->{'CURSOR_X'});
	if($delta) {
	    # Move cursor delta characters to the right
	    if($delta > 3) {
		syswrite($self->{'TTY'}, "\x1b[".$delta."C");
	    } else {
		my $sub = substr($self->{'LINE_BUFFER'}, $self->{'LINE_PTR'} - $delta);
		syswrite($self->{'TTY'}, $sub, $delta);
	    }
	    # Set new cursor position
	    $self->{'CURSOR_X'} += $delta;
	}
    }
    # Scroll the line left if cursor hasn't moved
    # because it has reached either the right edge
    # of the terminal window or a horizontal page
    # delimiter.
    if($delta < $off) {
	# Scroll entire horizontal page if possible
	if($hpagebreak) {
	    # Move cursor to the beginning of the terminal window
	    syswrite($self->{'TTY'}, "\x1b[".($self->{'CURSOR_X'})."D");
	    # Reset cursor position
	    $self->{'CURSOR_X'} = 0;
	    # Write the current horizontal page
    	    $self->term_insertline($self->{'LINE_PTR'});
	# Otherwise, scroll one character at a time
	} else {
	    $self->term_scrollleft;
	}
    }
}

sub term_scrollleft($) {
    my $self = shift;
    # Make sure we wont use negative buffer pointer
    my $pos = ($self->{'LINE_PTR'} > $self->{'CURSOR_X'}) ?
		($self->{'LINE_PTR'} - $self->{'CURSOR_X'}):0;
    # Length of the line segment from current
    # buffer pointer upto the end of the line
    my $slen = $self->{'LINE_LEN'} - $pos;
    # Length of the segment shown on the screen
    my $len = ($slen < $self->{'INPUT_WIDTH'}) ? $slen:$self->{'INPUT_WIDTH'};
    # Move cursor to the left edge of the terminal widow
    $self->term_cursorleft($self->{'CURSOR_X'});
    # Write the line segment to the terminal
    $self->term_putline($pos, $len);
}

sub term_scrollright($) {
    my $self = shift;
    # Length of the line segment from current
    # buffer pointer upto the end of the line
    my $slen = $self->{'LINE_LEN'} - $self->{'LINE_PTR'};
    # Length of the segment shown on the screen
    my $len = $self->{'INPUT_WIDTH'} - $self->{'CURSOR_X'};
    if($slen < $len) {
	$self->term_clrtoeol;
	$len = $slen;
    }
    # Write the line segment to the terminal
    $self->term_insertline($self->{'LINE_PTR'}, $len);
}

sub term_linestart($) {
    my $self = shift;
    # move cursor to the beginning of the command line
    $self->term_cursorleft($self->{'LINE_LEN'});
}

sub term_lineend($) {
    my $self = shift;
    # move cursor to the end of the line
    $self->term_cursorright($self->{'LINE_LEN'});
}

sub term_putchar($) {
    my $self = shift;
    my $c = shift;
    # Until we reach the right edge
    # of the terminal window,
    if($self->{'CURSOR_X'} < $self->{'INPUT_WIDTH'}) {
	# Write char at current cursor position
	syswrite($self->{'TTY'}, $c);
	# Advance cursor position
	$self->{'CURSOR_X'}++;
	# If char was inserted instead of appended
	if($self->{'LINE_LEN'} - $self->{'LINE_PTR'}) {
	    # Scroll trailing chars right
	    $self->term_scrollright;
	}
    # When we reach the right edge
    # of the terminal window,
    } else {
	# Scroll entire line left
	$self->term_scrollleft;
    }
}

sub term_putline($) {
    my $self = shift;
    $self->term_writeline(1, @_);
}

sub term_insertline($) {
    my $self = shift;
    $self->term_writeline(0, @_);
}

sub term_writeline($) {
    my $self = shift;
    my ($adv, $off, $len) = @_;

    # Default offset and length
    $off = 0 unless defined($off);
    $len = $self->{'LINE_LEN'} - $off unless defined($len);

    # Adjust lenght to virtual terminal size
    if($self->{'CURSOR_X'} + $len > $self->{'INPUT_WIDTH'}) {
	$len = $self->{'INPUT_WIDTH'} - $self->{'CURSOR_X'};
    }

    # Line segment to be written to the terminal
    my $line = ($off || $len < $self->{'LINE_LEN'}) ?
		substr($self->{'LINE_BUFFER'}, $off, $len):$self->{'LINE_BUFFER'};

    # Send current line to the terminal
    syswrite($self->{'TTY'}, $line, $len);
    # Should we advance
    if($adv) {
	# Adjust cursor position
	$self->{'CURSOR_X'} += $len;
    } elsif($len) {
	# Restore cursor position
	syswrite($self->{'TTY'}, "\x1b[".$len."D");
    }
}

sub term_clrtoeol($) {
    my $self = shift;
    # clear line from cursor position
    # to the end of the line
    syswrite($self->{'TTY'}, TCMD_CLRRIGHT);
}

sub term_clrline($) {
    my $self = shift;
    # clear entire line
    syswrite($self->{'TTY'}, TCMD_CLRLINE);
}

sub term_clrscreen($) {
    my $self = shift;
    # clear screen
    syswrite($self->{'TTY'}, TCMD_CLRSCR);
    # move cursor to the first line
    syswrite($self->{'TTY'}, TCMD_TOP);
}

#############################################################################################
################## C O N T E X T   M A N A G E M E N T   F U N C T I O N S ##################

sub is_cli() {
    my $self = shift;
    return ($self->{'FLAG_CLI_UP'});
}

sub is_context_action() {
    my $self = shift;
    return ($self->{'FLAG_CLI_UP'} && $self->{'FLAG_CSA'});
}

sub context_action_reset($) {
    my $self = shift;
    $self->{'FLAG_CSA'} = 0;
    $self->{'FLAG_HELP'} = 0;
    $self->{'FLAG_COMPLETE'} = 0;
}

sub context_complete($$) {
    my $self = shift;
    my $word = shift;
    # Replace partial with complete word
    $self->input_replaceword($word);
}

#############################################################################################
############################ U T I L I T Y   F U N C T I O N S ##############################

#
# Split configuration line into argument array.
#
#   Input:	1. object reference, passed implicitly
#		2. configuration line string
#
#   Output:	1. reference to argument array
#
sub split_config_line($$) {
    my $self = shift;
    my $line = shift;

    # If someone cares to try this with regexp,
    # by all means, be my guest. Im done banging
    # my head trying to do it the Perl way and
    # split configuration line with a single regex
    # while supporting quoted strings and escape
    # characters in and out of quoted strings.

    my @argv = ();
    my $arg = '';
    my $sq = 0;
    my $dq = 0;
    my $esc = 0;

    # Split the line into discrete characters
    # and loop over every single one of them.
    # Unpack is used here because it is a bit
    # faster than split()
    foreach my $c (unpack("(a)*", $line)) {
	# Is current char a whitespace that is
	# not escaped or inside open single or
	# double quotes ?
	if($c =~ /\s/ && !($dq || $sq || $esc)) {
	    # Skip if argument is still empty
	    next if($arg eq '');
	    # If argument is non-empty string,
	    # store it into argument array
	    push @argv, $arg;
	    # Reset the argument to empty string
	    $arg = '';
	# Is current char a single quote that is
	# not escaped or inside open double quotes ?
	} elsif($c eq "'" && !($dq || $esc)) {
	    $sq ^= 1;
	# Is current char a double quote that is
	# not escaped or inside open single quotes ?
	} elsif($c eq '"' && !($sq || $esc)) {
	    $dq ^= 1;
	# Is current char a backslash (escape char),
	# that is not escaped itself ?
	} elsif($c eq '\\' && !$esc) {
	    $esc = 1;
	# If nothing of the above, just append the
	# character to the current argument and reset
	# escape flag
	} else {
	    # Handle special character escapes
	    if($esc) {
		if($c eq 'a') {
		    $c = "\a";
		} elsif($c eq 'b') {
		    $c = "\b";
		} elsif($c eq 'e') {
		    $c = "\e";
		} elsif($c eq 'f') {
		    $c = "\f";
		} elsif($c eq 'n') {
		    $c = "\n";
		} elsif($c eq 'r') {
		    $c = "\r";
		} elsif($c eq 't') {
		    $c = "\t";
		}
		$esc = 0;
	    }
	    $arg .= $c;
	}
    }
    # If argument is a non-empty string after
    # the loop is finished, store it into 
    # argument array
    push @argv, $arg if($arg ne "");

    return \@argv;
}
#
# Create fully qualified typeglob name
#
#  Returns a string that can be used in typeglobs. If passed glob
#  string is not fully qualified, the function will prepend callers
#  namespace to it. Otherwise, it will return glob string as is.
#
#   Input:	1. glob string
#
#   Output:	1. fully qualified glob string
#
sub typeglob_qualify($$) {
    my $self = shift;
    my $glob = shift;

    # Sections are 'modular'. They can come from different
    # perl modules, so we will use per-section caller
    # field to fully qualify unqualified variable names
    my $ctx = $self->section_context_get;
    my $caller = defined($ctx) ?
		    $ctx->[0]->{'caller'}:$self->{'CALLER'};
    # Return already fully qualified name as is
    return ($glob =~ /::/) ?
		$glob:(defined($caller) ? $caller:'').'::'.$glob;
}
#
# Create new cfline 'struct'
#
#  This is an array that mimics a simple C structure that holds
#  neccessary data for each loaded configuration line. It carries
#  a reference to the argument array for the current configuration
#  line, its line number, and a reference to the config file name
#  this config line belongs to.
#
#  Cfline is used instead of a simple 2-dimensional arrays of raw
#  arguments because we need flexibility to move single configuration
#  lines or blocks of lines around and process them out of order, if
#  needed.
#
#   Input:	1. object reference, passed implicitly
#		2. a scalar reference to the config file name
#		   this line belongs to
#		3. line number of this line
#		4. an array reference to the argument array
#
#   Output:	1. an array reference to the cfline 'struct'
#
sub cfline_new($$$$) {
    my $self = shift;
    return [undef, undef, @_];
}
#
# Push cfline to the tail of the list
#
#   Input:	1. object reference, passed implicitly
#		2. an array reference to the cfline
#
#   Output:	none
#
sub cfline_push($$) {
    my ($self, $cfline) = @_;

    $cfline->[CF_LINE_PREV] = $self->{'CONFIG'}[CF_CONF_TAIL];
    $cfline->[CF_LINE_NEXT] = undef;

    $self->{'CONFIG'}[CF_CONF_TAIL][CF_LINE_NEXT] = $cfline if defined($self->{'CONFIG'}[CF_CONF_TAIL]);
    $self->{'CONFIG'}[CF_CONF_HEAD] = $cfline unless defined($self->{'CONFIG'}[CF_CONF_HEAD]);

    $self->{'CONFIG'}[CF_CONF_TAIL] = $cfline;

    # Add simple index to this linked list
    # with directive name as key and a cfline
    # reference as value.
    $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}{$cfline->[CF_LINE_NUM]} = $cfline;
}
#
# Shift cfline on the head of the config list out
#
#   Input:	1. object reference, passed implicitly
#
#   Output:	1. an array reference to the cfline
#
sub cfline_shift($) {
    my $self = shift;

    my $cfline = $self->{'CONFIG'}[CF_CONF_HEAD];
    return undef unless defined($cfline);

    $self->{'CONFIG'}[CF_CONF_HEAD] = $cfline->[CF_LINE_NEXT];
    if(defined($self->{'CONFIG'}[CF_CONF_HEAD])) {
	$self->{'CONFIG'}[CF_CONF_HEAD][CF_LINE_PREV] = undef;
    } else {
	$self->{'CONFIG'}[CF_CONF_TAIL] = undef;
    }

    # Delete index
    delete $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}{$cfline->[CF_LINE_NUM]};
    unless(scalar keys %{$self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}}) {
	delete $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]};
    }

    return $cfline;
}
#
# Put cfline to the head of the config list
#
#   Input:	1. object reference, passed implicitly
#		2. an array reference to the cfline
#
#   Output:	none
#
sub cfline_unshift($$) {
    my ($self, $cfline) = @_;

    $cfline->[CF_LINE_NEXT] = $self->{'CONFIG'}[CF_CONF_HEAD];
    $cfline->[CF_LINE_PREV] = undef;

    $self->{'CONFIG'}[CF_CONF_HEAD][CF_LINE_PREV] = $cfline if defined($self->{'CONFIG'}[CF_CONF_HEAD]);

    $self->{'CONFIG'}[CF_CONF_TAIL] = $cfline unless defined($self->{'CONFIG'}[CF_CONF_TAIL]);

    $self->{'CONFIG'}[CF_CONF_HEAD] = $cfline;

    # Add simple index to this linked list
    # with directive name as key and a cfline
    # reference as value.
    $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}{$cfline->[CF_LINE_NUM]} = $cfline;
}
#
# Extract cfline from anywhere in the config list
#
#   Input:	1. object reference, passed implicitly
#		2. an array reference to the cfline
#
#   Output:	none
#
sub cfline_extract($$) {
    my ($self, $cfline) = @_;

    if(defined($cfline->[CF_LINE_NEXT])) {
	$cfline->[CF_LINE_NEXT][CF_LINE_PREV] = $cfline->[CF_LINE_PREV];
    } else {
	$self->{'CONFIG'}[CF_CONF_TAIL] = $cfline->[CF_LINE_PREV];
    }

    if(defined($cfline->[CF_LINE_PREV])) {
	$cfline->[CF_LINE_PREV][CF_LINE_NEXT] = $cfline->[CF_LINE_NEXT];
    } else {
	$self->{'CONFIG'}[CF_CONF_HEAD] = $cfline->[CF_LINE_NEXT];
    }

    # Delete index
    delete $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}{$cfline->[CF_LINE_NUM]};
    unless(scalar keys %{$self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]}}) {
	delete $self->{'CONFIG'}[CF_CONF_INDEX]{$cfline->[CF_LINE_ARGV][0]};
    }
}
#
# Find cfline for given directive name.
#
#  If only directive name is given, returns an array of all
#  cflines for that directive. If both the directive and the
#  line number are given, returns a cfline for that directive
#  found on a configuration line with given line number.
#
#   Input:	1. object reference, passed implicitly
#		2. directive name
#		3. optional line number
#
#   Output:	1. a reference to the array of cfline references,
#		   if linenum was not specified, or
#		   an array reference to the found cfline
#
sub cfline_find($$;$) {
    my ($self, $directive_name, $linenum) = @_;

    return defined($linenum) ?
	    $self->{'CONFIG'}[CF_CONF_INDEX]{$directive_name}{$linenum}:
	    (values %{$self->{'CONFIG'}[CF_CONF_INDEX]{$directive_name}});
}
#
# Split configuration linked list into 2 segments.
#
#  First segment is from the head of original list upto (but excluding)
#  given cfline. Second segment is from the current cfline to the
#  tail of the original list. Index hash is not split.
#
#   Input:	1. an array reference to the original config list
#		2. an array reference to the delimiting cfline
#
#   Output:	1. an array reference to the first config list segment
#		2. an array reference to the second config list segment
#
sub config_split($$) {
    my ($config, $cfline) = @_;
    my $new_config = [undef, $config->[CF_CONF_TAIL], $config->[CF_CONF_INDEX]];

    if(defined($cfline->[CF_LINE_PREV])) {
	$cfline->[CF_LINE_PREV][CF_LINE_NEXT] = undef;
    } else {
	$config->[CF_CONF_HEAD] = undef;
    }

    $config->[CF_CONF_TAIL] = $cfline->[CF_LINE_PREV];

    $new_config->[CF_CONF_HEAD] = $cfline;
    $cfline->[CF_LINE_PREV] = undef;

    return ($config, $new_config);
}
#
# Merge 2 config linked lists into one.
#
#  Second config linked list segment will be appended to the tail of the
#  first config linked list segment.
#
#   Input:	1. an array reference to the first
#		   config list segment
#		3. an array reference to the second
#		   config list segment
#
#   Output:	1. an array reference to the resulting
#		   merged config list
#
sub config_concat($$) {
    my ($front, $back) = @_;

    if(defined($front->[CF_CONF_TAIL])) {
	$front->[CF_CONF_TAIL][CF_LINE_NEXT] = $back->[CF_CONF_HEAD];
    } else {
	$front->[CF_CONF_HEAD] = $back->[CF_CONF_HEAD];
    }

    if(defined($back->[CF_CONF_HEAD])) {
	$back->[CF_CONF_HEAD][CF_LINE_PREV] = $front->[CF_CONF_TAIL];
    }

    $front->[CF_CONF_TAIL] = $back->[CF_CONF_TAIL];

    undef @{$back};
    undef $back;

    return $front;
}
#
# Determine if directive is global.
#
#  This function will return TRUE if given directive or any of its
#  subdirectives is flagged with D_OP_GLOBAL operator.
#
#   Input:	1. object reference, passed implicitly
#		2. a hash reference to the directive template
#
#   Output:	1. TRUE, if directive is global
#		   FALSE, if directive isn't global
#
sub is_global_directive($$) {
    my ($self, $directive_template) = @_;
    my $vtarray;

    # Return true if this directive is global
    return 1 if($directive_template->{'op'} & D_OP_GLOBAL);

    # Search through directive's arguments

    # Turn current directive into an array if it's not already.
    if(defined($directive_template->{'arg'})) {
	if(ref($directive_template->{'arg'}) eq "HASH") {
	    $vtarray = [$directive_template->{'arg'}];
	} elsif(ref($directive_template->{'arg'}) eq "ARRAY") { 
	    $vtarray = $directive_template->{'arg'};
	} else {
	    # This one should never be reached
	    # because section_defaults() should
	    # catch template errors.
	    die "Invalid directive template argument type\n";
	}
    }
    # Search through mandatory and non-mandatory directives
    foreach my $argument (@{$vtarray}) {
	next unless($argument->{'template'} == TT_SECTION);
	foreach my $template (values %{$argument->{'dirs'}}) {
	    return 1 if($self->is_global_directive($template));
	}
    }
    # Search through optional directives
    foreach my $template (values %{$directive_template->{'opt'}}) {
	return 1 if($self->is_global_directive($template));
    }
    # Directive is not a global one
    return 0;
}
#
# Update a deptree node with new dependencies.
#
#  If the node doesn't already exist, it will be created.
#  Existing nodes will be updated with additional dependencies.
#  A single dependency can be passed as a scalar or an arrayref
#  to a single-element array. Multiple dependencies can only
#  be passed as array elements, by reference.
#
#  Deptree is only logically a tree. It's actual 'physical'
#  format is flat - it is a hash of all nodes from any of
#  the deptree levels that have any dependencies. A node can
#  be dependent on one or more nodes anywhere in the tree.
#  Physically, a node's dependencies are stored as elements
#  in an array referenced by node's hash entry:
#
#        $deptree->{node} = [ dep1, dep2, ... depN ];
#
#   Input:	1. object reference, passed implicitly
#		2. a hash reference to the deptree
#		3. target node
#		4. one or more dependencies:
#		   - if scalar, it can only be one dependency
#		   - if arrayref, it can be one or more dependencies
#
#   Output:	nothing
#
sub deptree_push($$$$) {
    my $self = shift;
    my ($deptree, $node, $dependencies) = @_;

    if(ref($dependencies) eq "ARRAY") {
	# Copy the array elements, because they are going
	# to be destroyed during dependency checking.
	$deptree->{$node} = defined($deptree->{$node}) ?
				[ @{$deptree->{$node}}, @{$dependencies} ]:
				[ @{$dependencies} ];
    } elsif(ref($dependencies) eq "") {
	# Entry is a scalar, so we should make an array
	# out of it and store its array reference.
	$deptree->{$node} = [] unless defined($deptree->{$node});
	push @{$deptree->{$node}}, $dependencies;
    } else {
	die "Config template error: invalid dependency configuration at \"".$node."\"\n";
    }
}
#
# Follow directive dependencies.
#
#  This function follows and verifies directive dependencies
#  along all deptree branches. If cross-dependency is found,
#  program is forced to exit.
#
#  Deptree is only logically a tree. It's actual 'physical'
#  format is flat - it is a hash of all nodes from any of
#  the deptree levels that have any dependencies. A node can
#  be dependent on one or more nodes anywhere in the tree.
#  Physically, a node's dependencies are stored as elements
#  in an array referenced by node's hash entry:
#
#        $deptree->{node} = [ dep1, dep2, ... depN ];
#
#  Dependency forms a logical connection (a branch) between
#  two nodes and branches must not form any kind of loop,
#  direct or indirect when traversing the path from node to
#  node downstream.
#
#  To check for loops, when walking the branch from the current
#  node down the tree, all nodes along the traversed path are
#  recorded. No node is allowed to appear twice in this recorded
#  path of parent nodes.
#
#  If it does, a loop exists on that segment of the deptree.
#
#   Input:	1. object reference, passed implicitly
#		2. a hash reference to the deptree
#
#   Output:	TRUE, if all is clear
#		FALSE, if cross-dependency exists
#
sub deptree_is_looped($$) {
    my $self = shift;
    my $deps = shift;

    # Walk the dependency tree
    while(my ($node, $dependencies) = each %{$deps}) {
	# Go down this top level node's branch
	return 0 unless &deptree_follow_branches($deps, $node);
    }
    return 1;
}
#
# Branch from the current node.
#
#  This function follows all dependencies branching from
#  the current node.
#
#   Input:	1. a hash reference to the hash-packed deptree
#		2. the current node's name
#		3+ the stack of parent node names (dependency path)
#
#   Output:	TRUE, if all is clear
#		FALSE, if cross-dependency exists
#
sub deptree_follow_branches($$;@) {
    my $deps = shift;
    my $node = shift;

    # Follow all branches from the current node,
    # processing all current node's dependencies.
    # Each segment of the tree will be processed
    # only once - by shifting dependencies out of
    # their nodes, we prevent iterating over already
    # processed segments of the tree.
    while(my $dependency = shift @{$deps->{$node}}) {
	# Check if loop exists on this branch.
	return 0 unless &deptree_check_loop($deps, $dependency, $node, @_);
    }
    # Remove already checked branches from the tree
    # to prevent looping over empty dependencies,
    # reducing the number of iterations. Since we
    # reached this point, they obviosly do not lead
    # to any loops, so any further reference to them
    # will imply a loop free path.
    delete $deps->{$node} unless defined($deps->{$node}[0]);

    return 1;
}
#
# Check if a loop exists at the current node.
#
#  This function removes leaf nodes from consideration
#  and checks if the current node already exists on
#  the stack of parent nodes (the dependency path).
#
#   Input:	1. a hash reference to the hash-packed deptree
#		2. the current node's name
#		3+ the stack of parent node names
#
#   Output:	TRUE, if all is clear
#		FALSE, if cross-dependency exists
#
sub deptree_check_loop($$;@) {
    my $deps = shift;
    my $node = shift;

    # Leaf nodes, that have dependencies which are not
    # dependent on anything themselves, are implicitly
    # loop free.
    return 1 unless defined($deps->{$node});
    # Loop exists when the current node's dependencies
    # themselves depend on the current node or it's
    # parents. Current node must not appear in the array
    # of it's parent nodes. Being a parent to yourself is
    # recursive and impossible and therefore constitutes
    # a loop condition.
    return 0 if grep(/^$node$/, @_);
    # We move on down this tree segment by branching
    # from the current node.
    return &deptree_follow_branches($deps, $node, @_);
}
#
# Dump entire destination hash into a formatted string.
#
#   Input:	1. object reference, NOT passed implicitly here
#		2. the destination hash name
#		3. a hash reference to the destination store
#
#   Output:	1. single, multiline string
#		   undef, if no output
#
sub dump_config($$) {
    my ($self, $dest, $store) = @_;
    my $hash;

    # Do we have a hash assigned to a named destination ?
    if(defined($store->{$dest})) {
	# Copy config to the temporary hash
	$hash = $store->{$dest};
    # Otherwise,
    } else {
	# We will be dumping a global hash
	local *global_dest_hash = *{$self->typeglob_qualify($dest)};
	local *global_dest_hash = *{'::'.$dest} unless %global_dest_hash;
    	$hash = \%global_dest_hash;
    }
    # Dump the config
    return &dump_hash($hash, 1);
}
#
# Dump the hash.
#
#   Input:	1. a hash reference
#		2. current recursion level, needed for
#		   output indentation.
#
#   Output:	1. single, multiline string
#		   undef, if no output
#
sub dump_hash($$) {
    my ($hash, $level) = @_;
    my $first = "";
    my $middle = "";
    my $last = "";

    # Make a local copy of the hash that we will be dumping
    # beacuse we will be deleting entries and we don't want
    # to damage the original data.
    my %work = %{$hash};

    while(my ($directive, $value) = each %work) {
	# Determine what type hash value we got
	my $ref = ref($value);
	# Format standalone directive
	if($ref eq "") {
	    # Dump this directive's value
	    $first .= (' 'x$level).$directive.(defined($value) ? " = ".$value:"")."\n";
	    # If this directive also begins the section,
	    # process the section immediately
	    if(defined($value) && ref($work{$value}) eq "HASH") {
		$first .= (' 'x$level)."Nested hash \'".$value."\':\n";
		$first .= &dump_hash($work{$value}, $level+2);
		# Since we processed this hash entry out of order,
		# we have to prevent dumping it again 
		delete $work{$value};
	    }
	# Format standalone array
	} elsif($ref eq "ARRAY") {
	    my $array = &dump_array($value, $level+2);
	    if(defined($array)) {
		$middle .= (' 'x$level).$directive." = (".$array.")\n";
	    }
	# Format standalone section
	} elsif($ref eq "HASH") {
	    $last .= (' 'x$level)."Nested hash \'".$directive."\':\n";
	    $last .= &dump_hash($value, $level+2);
	    # Since this directive may later appear as
	    # some other directive's value, we must prevent
	    # the code above from dumping the hash again.
	    delete $work{$directive};
	}
    }

    return ($first ne "" || $middle ne "" || $last ne "") ?
	    $first.$middle.$last:undef;
}
#
# Dump the array.
#
#   Input:	1. an array reference
#		2. current recursion level, needed for
#		   output indentation.
#
#   Output:	1. single, multiline string
#		   undef, if no output
#
sub dump_array($$) {
    my ($array, $level) = @_;
    my $first = "";
    my $middle = "";
    my $last = "";

    foreach my $element (@{$array}) {
	my $ref = ref($element);
	if($ref eq "") {
	    $first .= (defined($element) ? $element:"").",";
	} elsif($ref eq "ARRAY") {
	    my $array .= &dump_array($element, $level+2);
	    $first .= '('.$array.'),' if defined($array);
	} elsif($ref eq "HASH") {
	    $last .= "\n".&dump_hash($element, $level+2);
	    my $lvlprev = $level-2;
	    $last .= ' 'x$lvlprev;
	}
    }

    chop $first;

    return ($first ne "" || $last ne "") ?
	    $first.$last:undef;
}
#
# Deep copy the hash.
#
#  This function performs a deep copy of the hash,
#  recursing into all levels, copying all entries.
#
#   Input:	1. source hash reference
#		2. destination hash reference
#
#   Output:	none
#
sub deepcopy_hash($$) {
    my ($src_hash, $dst_hash) = @_;

    foreach my $key (keys %{$src_hash}) {
	if(ref($src_hash->{$key}) eq "HASH") {
	    $dst_hash->{$key} = {} unless defined($dst_hash->{$key}) &&
					  ref($dst_hash->{$key}) eq "HASH";
	    &deepcopy_hash($src_hash->{$key}, $dst_hash->{$key});
	} elsif(ref($src_hash->{$key}) eq "ARRAY") {
	    $dst_hash->{$key} = [] unless defined($dst_hash->{$key}) &&
					  ref($dst_hash->{$key}) eq "ARRAY";
	    &deepcopy_array($src_hash->{$key}, $dst_hash->{$key});
	} else {
	    $dst_hash->{$key} = $src_hash->{$key};
	}
    }
}
#
# Deep copy the array.
#
#  This function performs a deep copy of the array,
#  recursing into all dimensions, copying all elements.
#
#   Input:	1. source array reference
#		2. destination array reference
#
#   Output:	none
#
sub deepcopy_array($$) {
    my ($src_array, $dst_array) = @_;
    my $copy;

    foreach my $element (@{$src_array}) {
	if(ref($element) eq "HASH") {
	    $copy = {};
	    &deepcopy_hash($element, $copy);
	} elsif(ref($element) eq "ARRAY") {
	    $copy = [];
	    &deepcopy_array($element, $copy);
	} else {
	    $copy = $element;
	}
	push @{$dst_array}, $copy;
    }
}
#
# Find hash keys matching a given pattern.
#
#  This is similar to grep, only we are searching through hash keys and
#  if a hash key is a regex, the function will assume that regex consists
#  of synonims for some term, so it will try to expand it into multiple
#  array elements, each containing a single synonim. For example, when 
#  searching through
#
#   ('red','/white|light/','/black|dark/','blue', ... )
#
#  the function will try to match 'red', then expand
#
#   '/white|light/'
#
#  into
#
#   ('white','light')
#
#  and look for match within. Matches will be appended to the result array.
#  Then it will expand 
#
#   '/black|dark/'
#
#  into
#
#   ('black','dark')
#
#  and look for match within. Then it will try to match 'blue' and so on ...
#
#   Input:	1. search pattern string
#		2. a reference to the hash to be searched.
#
#   Output:	1. (list context)   array of matching hash keys
#		   (scalar context) number of matching hash keys
#
sub match_keys($$\%) {
    my $self = shift;
    my ($pattern, $hashref) = @_;
    my @matches = ();

    # Search through all hash keys
    foreach my $regex (keys %{$hashref}) {
	# If current hash key is a regexp, try to split it
	# into separate strings, look for a match and append
	# the result to the output array.
	@matches = (@matches, (grep(/^$pattern/, (split(/[^a-zA-Z0-9\-\_]+/, $regex)))));
    }

    my $num_matches = scalar(@matches);

    # In list context return matching hash keys,
    # in scalar context return the number of
    # matching hash keys.
    return wantarray ? (@matches):$num_matches;
}

#############################################################################################
######################################## M A C R O S ########################################

sub SECTION {
    my $ref = { 'template' => TT_SECTION, 'dirs' => {@_}, 'caller' => caller() };
    return $ref;
}

sub REQUIRE {
    my $ref = { 'template' => TT_SECTION, 'op' => S_OP_REQUIRE, 'dirs' => {@_} };
    return $ref;
}

sub ALLOW {
    my $ref = { 'template' => TT_SECTION, 'op' => S_OP_ALLOW, 'dirs' => {@_} };
    return $ref;
}

sub OPTIONAL {
    my $ref = { 'template' => TT_SECTION, 'op' => S_OP_OPTIONAL, 'dirs' => {@_} };
    return $ref;
}

sub DIRECTIVE {
    my $directive_name = lc(shift);

    my $directive_template = { 'template' => TT_DIRECTIVE };

    while(my $what = shift) {
	if(ref($what) eq "ARRAY") {
	    $directive_template->{'deps'} = $what;
	} elsif(ref($what) eq "HASH") {
	    if($what->{'template'} == TT_VALUE) {
		push @{$directive_template->{'arg'}}, $what;
	    } elsif($what->{'template'} == TT_SECTION) {
		if(defined($what->{'op'})) {
		    if($what->{'op'} & (S_OP_REQUIRE|S_OP_ALLOW)) {
			push @{$directive_template->{'arg'}}, $what;
		    } elsif($what->{'op'} & S_OP_OPTIONAL) {
			$directive_template->{'opt'} = $what;
		    } else {
                        die "Configuration template error: unknown section template operator\n";
		    }
		} else {
		    $directive_template->{'section'} = $what;
		}
	    }
	} elsif(ref($what) eq "") {
	    $directive_template->{$what} = shift;
	}
    }

    return wantarray ? ( $directive_name => $directive_template ):
		       $directive_template;
}

sub HIDDEN {
    my $directive_name = shift;
    my $ref = DIRECTIVE($directive_name, @_);
    $ref->{'op'} |= D_OP_HIDDEN;
    return wantarray ? ( $directive_name => $ref ):$ref;
}

sub GLOBAL {
    my $directive_name = shift;
    my $ref = DIRECTIVE($directive_name, @_);
    $ref->{'op'} |= D_OP_GLOBAL;
    return wantarray ? ( $directive_name => $ref ):$ref;
}

sub CATCHALL {
    return '/.*/', 'op' => D_OP_NONEXTARG;
}

sub DEPS {
    return [@_];
}

sub IF {
    my ($expr, $ref) = @_;
    $ref->{'cond'} = $expr;
    return $ref;
}

sub ARG {
    my $type = shift;
    my $ref = {
	'template' => TT_VALUE,
	'type' => $type,
	'ops' => [],
    };
    while(my $what = shift) {
	if(ref($what) eq "HASH") {
	    push @{$ref->{'ops'}}, $what;
	} else {
	    $ref->{$what} = shift;
	}
    }
    return $ref;
}

sub MAP {
    return ARG(CF_MAPPED, @_);
}

sub SECTION_NAME {
    return ARG(CF_STRING|CF_SECTION_NAME, @_);
}

sub STORE {
    my $ref = {@_};
    $ref->{'template'} = TT_OPER;
    $ref->{'op'} = O_OP_STORE;
    return $ref;
}

sub STOREONCE {
    my $ref = {@_};
    $ref->{'template'} = TT_OPER;
    $ref->{'op'} = O_OP_STOREONCE;
    return $ref;
}

sub OVERWRITE {
    my $ref = {@_};
    $ref->{'template'} = TT_OPER;
    $ref->{'op'} = O_OP_OVERWRITE;
    return $ref;
}

sub DELETE {
    my $hashname = shift;
    my $ref = {
	'template' => TT_VALUE,
	'type' => CF_NONE,
	'ops' => {
	    'op' => O_OP_DELETE,
	    'hash' => $hashname,
	    @_
	}
    };
    return $ref;
}

sub MODIFY($) {
    my $ref = {
	'template' => TT_OPER,
	'op' => O_OP_MODIFY,
	'mod' => shift
    };
    return $ref;
}

sub SKIP(;$) {
    my $type = shift;
    my $ref = {
	'template' => TT_VALUE,
	'type' => defined($type) ? $type:CF_STRING,
	'op' => V_OP_NOOP,
    };
    return $ref;
}

sub OPER {
    return ARG(CF_NONE, @_);
}

sub FROM($) {
    return 'map' => shift;
}

sub TO($) {
    return 'hash' => shift;
}

sub KEY($) {
    return 'key' => shift;
}

#sub MODIFY($) {
#    return 'modify' => shift;
#}

sub POSTPARSER(\&;){
    return 'postparser' => shift;
}

sub DEFAULT($) {
    return 'default' => shift;
}

sub HELP($) {
    return 'help' => shift;
}

1;
__END__

=head1 NAME

Config::ContextSensitive - Parser for context sensitive scoped config files

=head1 SYNOPSIS

  use Config::ContextSensitive;			# object methods only
  use Config::ContextSensitive qw(:macros);	# object methods and 'macros'
						# for building configuration
						# templates

  # Create new configuration object
  my $conf = Config::ContextSensitive->new($conf_template);

  # Change internal directive keyword for
  # exiting the current section
  $conf->internal_directive_names('END_SECTION' => 'leave');

  # Assign a hash to a named destination
  $conf->assign_destination('NAMED_DESTINATION', \%DEST_HASH);

  # Assign an anonymous hash to a named destination
  $conf->assign_destination('NAMED_DESTINATION', {});

  # The same as above, only an anonymous hash
  # will be automatically created
  $conf->assign_destination('NAMED_DESTINATION');

  # Load and parse configuration file
  # and commit configuration
  $conf->load('/my/config/file.conf');

  # Load and parse configuration file
  $conf->loadonly('/my/config/file.conf');

  # Commit pending configuration
  $conf->commit;

  # Retrieve a reference to the destination hash
  my $dest_hash = $conf->get_destination('NAMED_DESTINATION');

=head1 DESCRIPTION

This is a parser for configuration files loosely resembling Cisco IOS configs.
Syntax in these files is not fixed, but has arbitrary number of directives per
line and each directive can have none or many arguments. It is also scoped,
because it can be divided into various sections that can be nested within one
another.

Context sensitivity comes from the fact that each directive on a configuration
line determines which directive or directives can come after it or even which
section will begin with this line.

Because of that, we have to explicitly define how each line is going to be parsed
and where the parsed data should be stored. For that purpose we use a bunch of
nested hashes that we call templates. All parsed data is stored into one or more
hashes that can be assigned as destinations or created for that purpose by
the parser. If no assignment is made, global hashes are used.

We can even dynamically expand the configuration template itself during parsing.
Using simple callback that allows the application to examine parsed values and
change them if neccessary, we can produce new templates from within the callback
and return them to be used as soon as the current configuration line is done.
This makes it easy for modular applications to load 'plugins' which can 'register'
their own configuration sections and have them processed on the fly.

=head1 CONFIGURATION FILE FORMAT

Each line begins with a keyword that we call main directive. Directive is followed
by one or more arguments or sub-directives:

    some_directive argument_1 argument_2 some_subdirective subargument_1 ....

Configuration files are scoped, which means config lines can be grouped together
inside a block that we call 'section':


    some_directive   argument_1 ... argument_n
	some_directive_inside_section   argument_1 ... argument_n
	some_other_directive_inside_section   argument_1 ... argument_n
	  ...
	last_directive_inside_section   argument_1 ... argument_n

    some_directive_outside_section argument_1 ... argument_n


Sections do not have visible delimiters like braces or BEGIN-END statements.

Section begins when some directive, that is specified in the template as the section
starter is encountered in the configuration file. All lines that come after it are
considered a part of that section. Sections can be nested within other sections.

Section ends when a line that begins with a directive that is unknown within current
section context is encountered. Unknown line is then returned to the parent section
for parsing. If no section (including the top-level) can parse the line, it is
considered an unsupported/unknown directive, and the configuration parsing fails.

Indentation is only for convenience. It has no effect on parsing, as all leading and
trailing blank characters are removed prior to parsing line arguments.

Configuration files can recursively include one another, using special internal 
directive

         include_file  "path/to/the/config/file"

Also, configuration files can include entire directories of config files, using another
internal directive

         include_dir    "path/to/the/config/dir"

Files can be included from any part of configuration file. Also, files can be included
from other files, even multiple times, as long as there is no loop in inclusion. If you
try to include file B.conf from A.conf and then again A.conf from B.conf, it will throw
an error message. Also

         A.conf---include--->B.conf---include--->C.conf--
         A                                              |
         ------------------- include --------------------

won't work either, not even if you try to trick it using symlinks. However, hardlinks
are hard to follow, so there is a fixed maximum recursion level of 10.

All parsed configuration data is stored in temporary hashes until configuration files
are processed successfully. Only then, the produced data is copied into their final
destination hashes. This is to prevent corrupting live configuration on application
HUP/reload in case new configuration contains errors and has to bail out sooner than
expected.

=head2 An example of a simple configuration file.

  # These are top-level section directives
  log_to_syslog yes
  log_to_console no
  syslog_facility daemon
  syslog_level info

  subnet 10.11.12.0/24

  # This directive begins an anonymous section
  radius
     server someserver.somedomain.net
     auth port 1812
     acct port 1813

  # This directive begins a named section.
  # It also requires 2 arguments. Config lines
  # can have many directives, mandatory and
  # optional, each requiring none or more 
  # arguments.
  database somedb mysql
     server somedb.somedomain.net

  # Include anoter configuration file
  include_file "/next/database.conf"

  # Include all config files from designated
  # directory
  include_dir "/etc/someapp/conf"

=head1 CONFIGURATION TEMPLATES

Each directive can have from none to many arguments, a mandatory subdirective and/or 
optional subdirectives, can begin a new nested section and can depend on other main
directives from the same context.

Each argument can be stored into the destination hash as is, or parsed in some way,
depending on the desired value type. Parsing an argument can result in one or more
produced values. Single value results are stored as scalars into their designated 
destination hash entries, while multiple values come in form of a hash or an array
and are, by default, stored as hash or array references, respectively. If neccessary,
we can control how and where specific values of a produced array or hash will be stored.

Each argument can have a default argument defined. Default arguments are parsed according
to the same rules the real configuration lines are, so they can also produce one or more
values and are stored according to the same rules the real configuration values are.
Default values are applied before the real configuration, so whatever argument had
a default setting, but was left out from the configuration, will still have its defaults
in the resulting configuration.

All this is configured through the use of configuration templates. There are 2 ways to
define a configuration template: the default, hash way and the optional, macro way.

Configuration template consists of several types of templates:

=over 4

=item *

section templates

=item *

directive templates

=item *

value templates

=item *

format templates

=back

=head2 SECTION TEMPLATES

A section template is a hash with directive names as hash keys, and hash references to
directive templates as hash values.

 my %SECTION_TEMPLATE = (
	'template'	=> 'SECTION',
	'dirs'		=> {
	    'directive_1'	=> { directive_template_1 },
	    'directive_2'	=> { directive_template_2 },
            ...
	    'directive_N'	=> { directive_template_N }
	}
 );

All section templates, regardless of their position within the hierarchy (top, bottom
or anywhere in the middle) have exactly the same format. They are used both vertically
(define parsing rules for various configuration lines) and horizontally (used to define
optional subdirectives of a directive).

=head2 DIRECTIVE TEMPLATES

A directive template is a hash with template parameters as hash keys and template parameter
values as hash values:

 my  %DIRECTIVE_TEMPLATE = (
	'template'	=> 'DIRECTIVE',
	'arg'	=> [ { value_template_1 },{ value_template_2 }, ... ],
	'sub'	=> { mandatory_subdirective_section_template },
        'opt'	=> { optional_subdirective_section_template },
	'section'	=> { nested_section_template },
	'deps'		=> [ 'directive_name_1','directive_name_2',... ],
	'hidden'	=> boolean_flag
 );

=over 20

=item B<    values>

(OPTIONAL) Holds a hash reference to a single value template, if a directive requires a single
argument, or a reference to an array of value template references, if a directive requires
multiple arguments. Directive can also require no arguments, in which case this parameter
can be omitted.

=item B<    subdirectives>

(OPTIONAL) Holds a hash reference to a section template defining mandatory subdirectives.
Section template is used here to offer a choice of several mandatory subdirectives out of
which one has to be defined on the configuration line. If a directive doesn't have any
mandatory subdirectives, this parameter can be left undefined.

=item B<    optional>

(OPTIONAL) Holds a hash reference to a section template defining optional subdirectives.
Optional subdirectives are processed one after another until an unknown subdirective is
encountered on the configuration line. Processing is then returned to the previous recursion
level, which may be able to parse the subdirective itself. If noone can parse the directive,
it is considered an unknown directive which results in error.

=item B<    section>

(OPTIONAL) Holds a hash reference to the section template for the subordinate section.
Defining this in any directive template, marks that directive as a section starter, and once
the configuration line has been fully processed, parser will enter the defined subsection.

=item B<    deps>

(OPTIONAL) Holds a reference to the array of directive names this particular directive
depends on. During configuration parsing, when a directive which depends on others is encountered,
its dependencies will be processed first. Only main directives can have dependencies
and they must come from the same context.

If you need a directive to depend on other directive that is inside a section, make the directive
dependent on the directive that begins that section. This is because nested section will be 
automatically processed along with its starter directive, which belongs to the dependent
directive's context.

If you need a directive to depend on some directive's subdirective, make the directive dependent
on the main directive of that configuration line.

=item B<    hidden>

(OPTIONAL) Boolean flag whose non-0 (true) value defines this directive template as
internal/hidden. If this directive name is encountered in the configuration, it will be treated as
if it were unknown directive. This option is useful if we want to set some internal configuration 
values without allowing them to be changed from the configuration file. If that is the case, we 
should define a complete directive template and its matching unique directive name, define the value
templates and set their default values, and then set hidden flag to hide the directive from
the parser.

=back

=head2 VALUE TEMPLATES

A value template is a hash with template parameters as hash keys and template parameter
values as hash values:

 my %VALUE_TEMPLATE = (
	'template'	=> 'VALUE',
	'type'		=> value_data_type,
	'map'		=> 'source_hash_name',
	'mapkey'	=> 'key_format',
	'hash'		=> 'destination_hash_name',
	'key'		=> 'key_format' or
			   { 'key_format' => 'store_format', ... },
	'subkey'	=> 'subkey_format' or 
			   { 'subkey_format' => 'store_format', ... },
	'prepend'	=> 'prepend_val_format' or
			   { 'prepend_key_format' => 'prepend_val_format', ... } or
			   [ 'prepend_val_format, ... ]
	'append'	=> 'append_val_format' or
			   { 'append_key_format' => 'append_val_format', ... } or
			   [ 'append_val_format, ... ]
	'op'		=> bitfield,
	'postparser'	=> \&postparser_func
 );

=over 20

=item B<    type>

(MANDATORY) How should the argument be interpreted and stored. Certain data types and data type
modifiers can be ORed together to produce the desired result. For example, CF_ARRAY type produces
an array of all arguments up to the end of the line, keeping all elements as they are. However,
if we OR this type with, say, CF_INTEGER, then all array elements would have to be integers, or
else the parsing would fail. We can go even further and expect all array elements to be positive
integers by defining type as CF_ARRAY|CF_INTEGER|CF_POSITIVE. See CONSTANTS below for detailed 
description of each type and modifier.

=item B<    map>

(OPTIONAL) Used if 'type' is CF_MAPPED. The name of the hash to be used for lookup. 
Parsed argument value is not stored directly into the destination hash, but is used
as a hash key for the hash configured here. Values taken from the 'map' hash are then
stored into the destination hash according to general storage rules. Put it simply,
it does:

            $dest_hash_ref->{$key} = $map_hash_ref->{$parsed_argument};

=item B<    hash>

(OPTIONAL) If this template defines an argument whose parsed value should be stored,
this is the name of the destination hash which will receive parsed data. Note that hashes
are specified in this template only as strings, hash names, not as hash or hash reference
data types directly. If the destination hash references were assigned to these names, than
those hashes will be used. Otherwise, these names will be used directly as global hash names
and those global hashes will be used as destinations.

=item B<    key>

(OPTIONAL) First level destination hash key. This can be the name of a key directly,
a format template or a reference to a hash with explicit key_format => store_format
mappings. Both key_format and store_format can be explicit key/value or they can be
format templates:

	    'key' => { key1_fmt => val1_fmt, key2_fmt => val2_fmt, ...}

=item B<    subkey>

(OPTIONAL) Second level destination hash key. This can be the name of a subkey directly,
a format template or a reference to a hash with explicit key_format => store_format mappings.
Both key_format and store_format can be explicit key or value or they can be format templates.

		     'subkey' => { subkey1_fmt => val1_fmt,  ... }

=item B<    overwrite>

(OPTIONAL) Boolean flag that allows overwriting destination hash entries. By default, defining
the same directive more than once is not allowed. If entry in the destination hash already exists,
configuration parsing will fail and the parser will report an error. If this flag is set to
non-0 ('true'), parser will ignore the existence of the destination entry and store produced
values anyway, overwriting any previous value.

=item B<    default>

(OPTIONAL) The default argument. It will be parsed and applied before the actual configuration.
If configuration file doesn't specify this directive, this will be the directive's default value
in the resulting configuration. It must be defined as string, which will be parsed in the same
manner as the real configuration line. Produced default values will also be stored the same way
the real configuration values would.

=item B<    postparser>

(OPTIONAL) A reference to the custom post-parser function. After argument parsing is done,
this optional parameter can be used to do additional custom parsing or some sort of custom sanity
checks for produced values. But it can also do much more ... If a value template specifies 'type'
ORed together with CF_SECTION modifier, then the return value will be used as the nested section
template. This allows us to dynamically expand configuration template during configuration parsing.
For example, a configuration line may specify a full path to a module or a plugin, which can be
loaded and initialized from the post-parser callback, returning a section template that defines
the configuration for that module/plugin. That section will begin immediataly after the current
line is done.

The post-parser callback function takes these parameters:

	1. configuration directive name,
	2. reference to hash or array of values
	   or value itself, depending on the
	   value type,
	3. hash reference to the destination hash
	4. hash reference to the map hash (if any)
	4. current section name (if any)
	5. nested section name (if any)

It returns:

	1. TRUE, if post-parsing was successful
	   FALSE, if post-parsing failed
	2. optional custom-parsed value, on success
	   optional error message, on error

It is completely fine just to return TRUE or FALSE. However, if post-parser wants to return an
error message, it should return FALSE as the first, and the message string as the second return
parameter.

If post-parser wants not only to examine, but to modify parsed values, it should return TRUE as
the first, and the values as the second return parameter. A scalar value should be returned as
is. A hash or an array should be returned as references. After that, whatever gets stored into
the destination hash depends on the definition of 'key' and 'subkey' parameters.

=back

=head2 FORMAT TEMPLATES

Format template is a form of a format string that parser uses to determine which data to use as
a hash key, a subkey or a value. Parsing some argument types can produce more than one value. 
For example, CF_STRING will be stored as is - a single string. The same goes for CF_INTEGER or,
say, CF_BOOLEAN. However, argument types like, say, CF_INET combined with modifiers like CF_ATOI,
will take a single string argument - a subnet - a.b.c.d/m and produce 2 integer values: network
and subnet.

By default, if parsing produces a single value, it will be stored as a scalar into the destination
hash. If parsing produces multiple values in form of a hash, it will be stored as a hash reference
into the destination hash. If parsing produces multiple values in form of an array, it will be
stored as an array reference into the destination hash. Some argument types may even produce hashes
of arrays or arrays of hashes.

If we want to change this default behavior, we can use format templates to explicitly define which
produced value goes into which destination hash entry.

Hashed values produced by argument parsing can be referenced by format template by their key names.
Some argument types that produce fixed hash formats (like CF_INET|CF_ATOI) implicitly define their
key names. You can learn about them either by reading documentation or by looking at the parser code.
Other argument types (like CF_MAPPED) produce hashes by taking contents of 'map' hashes which are
user (or rather, you, the coder) defined, so you should know their structure - whether they are
single or multilevel hashes and what are their keys called.

Arrays of values produced by argument parsing can be referenced by format template by their array
indexes. Here, you don't have to worry about particular key names, but you do have to know how many
array elements there are.

Supported format template keywords are:

=over 8

=item *

If we wish to store all keys of a produced hash, we use 'KEYS', which creates an array of hash key
names and stores the reference to it into the destination hash.

=item *

If we wish to store all values of a produced hash, we use 'VALUES', which creates an array of hash
values and stores the reference to it into the destination hash.

=item *

If we wish to store the produced values as is (scalars are stored directly, hashes and arrays are
stored as references), we use 'VALUE'.

=item *

If referencing values in a produced hash, we use 'VALUES.hashkeyname' (eg. VALUES.NETWORK).

=item *

If referencing values in a produced array, we use 'VALUES.arrayindex' (eg. VALUES.1).

=item *

If referencing values in a produced 2-level hash, we use 'VALUES.hashkeyname.hashkeysubname'
(eg. VALUES.databases.type).

=item *

If referencing values in a produced hash of arrays, we use 'VALUES.hashkeyname.arrayindex'
(eg. VALUES.senders.3).

=item *

If referencing values in a produced 2-dimensional array, we use 'VALUES.arrayindex1.arrayindex2'
(eg. VALUES.3.5).

=item *

If referencing values in a produced array of hashes, we use 'VALUES.arrayindex.hashkeyname'
(eg. VALUES.3.senders).

=item *

If referencing a directive name, we use 'DIRECTIVE' keyword.

=item *

If referencing raw configuration line arguments, we use 'ARG.arrayindex' (eg. ARG.0).

=item *

If referencing current section name, we use 'SECTION' keyword.

=item *

If referencing subordinate section that we are about to enter when current line is
fully parsed, we use 'NESTED_SECTION' keyword.

=back

The actual use-cases for these include (but are not limited to):

=over 8

=item *

If destination hash's key name is supposed to the same as the directive name,
we can specify destination hash key as 'key' => 'DIRECTIVE' which will make
hash key name mirror the directive name.

=item *

If we have multidimensional destination hashes, we may want to use original
argument as first level hash key, and store produced values into second level
keys. In that case, we want to pick original raw argument from the config line
using ARG.index , where index is 0 up to the number of arguments in the current
configuration line. 

=item *

If we have multidimensional destination hashes, we may want to have their
structure mirror the structure of the configuration section, so we may want to
name the section and use that name as the first level hash key and store produced
values into designated subkeys. For this we will use keyword 'SECTION' which will
be formatted as current section's name (if any).

=item *

If we are starting a named section, but the section starter is a multi-argument
line, we may want to define the section name in one argument, and store other
parsed arguments on the current line the same way we will store arguments within 
the section (remember that parser won't actually begin the new section before the 
last argument on the current line is parsed), say, use section name as the first
level key. However, using 'SECTION' as the format template in arguments on the 
current line would yield undesirable results, because it refers to the current
section, not the one that is about to start. For that purpose we have 'NESTED_SECTION'
keyword that will enable arguments on the current line to reference the name of
the section that is yet to begin.

=back

=head2 An example of the template required for parsing the above example file

 my %CF_TEMPLATE = (
    'log_to_syslog' => {
	'arg' => {
	    'type'    => CF_BOOLEAN,
	    'hash'    => 'CONFIG',
	    'key'     => 'DIRECTIVE'
	}
    },
    'log_to_console' => {
	'arg' => {
	    'type'    => CF_BOOLEAN,
	    'hash'    => 'CONFIG',
	    'key'     => 'DIRECTIVE'
	}
    },
    'syslog_facility' => {
	'arg' => {
	    'type'    => CF_MAPPED,
	    'map'     => 'FASC',
	    'hash'    => 'CONFIG',
	    'key'     => 'fasc'
	}
    },
    'syslog_level' => {
	'arg' => {
	    'type'    => CF_MAPPED,
	    'map'     => 'LVL',
	    'hash'    => 'CONFIG',
	    'key'     => 'lvl'
	}
    },
    'subnet' => {
	'arg' => {
	    'type'    => CF_SUBNET,
	    'hash'    => 'SUBNETS',
	    'key'     => 'ARG.0',
	    'subkey'  => { 'VALUES.NETWORK' => 'VALUES.NETMASK'}
	}
    },
    'search_databases' => {
	'arg' => {
	    'type'    => CF_MAPPED,
	    'map'     => 'DATABASE',
	    'hash'    => 'SUBNETS',
	    'key'     => { 'DIRECTIVE' => 'KEYS' },
	},
	'deps' => [ 'database' ]
    },
    'radius' => {
	'section' => {
	    'server' => {
		'arg' => {
		    'type'    => CF_STRING,
		    'hash'    => 'CONFIG',
		    'key'     => 'rad_server'
		}
	    },
	    'auth' => {
		'sub' => {
		    'port' => {
			'arg' => {
			    'type'    => CF_PORT,
			    'hash'    => 'CONFIG',
			    'key'     => 'auth_port'
			    'default  => '1812'
			}
		    }
		}
	    },
	    'acct' => {
		'sub' => {
		    'port' => {
			'arg' => {
			    'type'    => CF_PORT,
			    'hash'    => 'CONFIG',
			    'key'     => 'acct_port'
			    'default  => '1813'
			}
		    }
		}
	    }
	}
    },
    'database' => {
	'arg' => [ {
	    'type'    => CF_SECTION_NAME,
	},{
	    'type'    => CF_STRING,
	    'hash'    => 'DATABASE',
	    'key'     => 'NESTED_SECTION',
	    'subkey'  => 'dbtype'
	} ]
	'section' => {
	    'server'	=> {
		'arg' => {
		    'type'    => CF_STRING,
		    'hash'    => 'DATABASE',
		    'key'     => 'SECTION',
		    'subkey'  => 'dbsrv'
		}
	    },
	    port' => {
		'arg' => {
		    'type'    => CF_PORT,
		    'hash'    => 'DATABASE',
		    'key'     => 'SECTION',
		    'subkey'  => 'dbport'
		}
	    }
	}
    }
 );

=head1 MACROS

Okay, that wasn't that short, was it ? Fortunately, there is another way ... a MACRO way!
Well, sort of. There are no true macros in Perl, but a set of small functions can do the job.
These 'macros' are not exported into the caller's namespace by default, so you have to either
explicitly state every macro name in the 'use' statement, or use export tag 'macros' to include
them all at the same time:

   use Config::ContextSensitive qw(:macros);

Now, the above template can be defined as follows:

 my $CF_TEMPLATE = SECTION(
     DIRECTIVE('log_to_syslog', STORE(AS CF_BOOLEAN, TO 'CONFIG', KEY 'DIRECTIVE')),
     DIRECTIVE('log_to_console', STORE(AS CF_BOOLEAN, TO 'CONFIG', KEY 'DIRECTIVE')),
     DIRECTIVE('syslog_facility', MAP(FROM 'LVL', TO 'CONFIG', KEY 'fasc')),
     DIRECTIVE('syslog_level', MAP(FROM 'LVL', TO 'CONFIG', KEY 'fasc')),
     DIRECTIVE('subnet', STORE(AS CF_SUBNET, TO 'SUBNETS', KEY 'ARG.0', SUBKEY { VALUES.NETWORK => VALUES.NETMASK })),
     DIRECTIVE('search_databases', MAP(FROM 'DATABASE', TO 'CONFIG', KEY { 'DIRECTIVE' => 'KEYS' }), DEPS('database')),
     DIRECTIVE('radius',
     SECTION(
          DIRECTIVE('server', STORE(AS CF_STRING, TO 'CONFIG', KEY 'rad_server')),
          DIRECTIVE('auth', DIRECTIVE('port' STORE(AS CF_PORT, TO 'CONFIG', KEY 'auth_port', DEFAULT '1812'))),
          DIRECTIVE('acct', DIRECTIVE('port' STORE(AS CF_PORT, TO 'CONFIG', KEY 'acct_port', DEFAULT '1813'))),
     )),
     DIRECTIVE('database', SECTION_NAME, STORE(AS CF_STRING, TO 'DATABASE', KEY 'NESTED_SECTION', SUBKEY 'db_type'),
     SECTION(
         DIRECTIVE('server', STORE(AS CF_STRING, TO 'DATABASE', KEY 'SECTION', SUBKEY 'dbsrv')),
         DIRECTIVE('port', STORE(AS CF_PORT, TO 'DATABASE', KEY 'SECTION', SUBKEY 'dbport'))
     ))
 );

It may take time to adjust and remember how 'macros' interact with each other, but once you do,
it will be far easier to define templates this way. Here's the explanation of each of used
macros:

=over 20

=item B<    SECTION()>

Defines a section, or rather, whatever is nested within this macro defines a section
and this macro wraps it up.

In list context it is equivalent to 

  'section' => { section_template }

In scalar context, it is equivalent to

  { section_template }

Receives output from multiple DIRECTIVE() macros and returns a single entry hash
(a key => value pair) in list context, or a hash reference to the section
template in scalar context.

It can be used standalone, in case it begins the topmost section of the configuration,
or nested within DIRECTIVE() macros.

It's place within configuration template typically looks like this:

  SECTION(
    DIRECTIVE( ....),
    DIRECTIVE( ....),
    DIRECTIVE( ....
      SECTION(
        DIRECTIVE( ... ),
        DIRECTIVE( ... )
      ),
    )
  );

=item B<    DIRECTIVE()>

Defines a directive template. It can be used within SECTION() and nested within another
DIRECTIVE(), HIDDEN() or OPTIONAL(). If it is nested within SECTION() it defines the main
directive. Nesting it within another DIRECTIVE() macro implies mandatory subdirective.
If we wish to define optional subdirective, we have to use OPTIONAL() macro.

In list context, it is the equivalent to

  directive_name => { directive_template }

In scalar context, it is equivalent to 

  { directive_template }

Receives a directive name as a mandatory parameter, and output from MAP(), STORE(), HIDDEN(),
DEPS(), DIRECTIVE() and OPTIONAL() macros in any order.

It returns a single entry hash (a key => value pair) in list context, or a hash reference
to the directive template, in scalar context. Typical usage would be:

  DIRECTIVE('directive_name', STORE( ... ), STORE( ...),
   DIRECTIVE('mandatory_subdirective1_name', STORE( ... )),
   DIRECTIVE('mandatory_subdirective2_name', STORE( ... )),
   DIRECTIVE('mandatory_subdirective3_name', STORE( ... ),
    OPTIONAL('mandatory_subdirective3's_optional_subdirective1_name', STORE( ... )))
    OPTIONAL('mandatory_subdirective3's_optional_subdirective2_name', STORE( ... )))
  ),
  HIDDEN('internal_directive_name', STORE( ... )),

=item B<    OPTIONAL()>

It is the same as DIRECTIVE() macro, except it sets 

  'opt' => 1

inside the directive template, making the directive optional. It works only on subdirectives,
since main directives cannot be optional, obviosly. If used as a main directive template,
'opt' is ignored. Typical usage is shown above.

=item B<    HIDDEN()>

It is the same as DIRECTIVE() macro, except it sets 

  'hidden' => 1

inside the directive template, making the directive 'invisible' to the configuration parser,
but still visible to the default values parser. It is used when we need to define internal
configuration entries, but prevent them from being changed from configuration file. Typical
usage is shown above.

=item B<    DEPS()>

Defines an array of directive names this directive depends on.

In list context, it is equivalent to 

  'deps' => [ ... ]

In scalar context, it is equivalent to

  [ ... ]

Receives the names of directives this directive depends on and returns a single entry hash
(key => value pair) in list context, or a reference to the array of names in scalar context.

It can be used only within DIRECTIVE(), HIDDEN() and OPTIONAL() macros. Typical usage 
would be:

  DIRECTIVE('directive_name', STORE( ...), DEPS('dep1_name','dep2_name',...))

=item B<    SECTION_NAME>

Defines a value template that only specifies directive's argument type to be nested section name
and takes no parameters.

In list context, it is equivalent to

  'arg' => { 'type' => CF_SECTION_NAME }

In scalar context, it is equivalent to

  { 'type' => CF_SECTION_NAME }

It returns a single entry hash (a key => value pair) in list context, or a hash reference
to the value template, in scalar context.

It can be used only within DIRECTIVE(), HIDDEN() and OPTIONAL() macros. Typical usage 
would be:

  DIRECTIVE('directive_name', SECTION_NAME, SECTION( ...))

but it can also be:

  DIRECTIVE('directive_name', SECTION_NAME, STORE( ...),
   DIRECTIVE('mandatory_subdirective1_name', STORE( ... )),
   DIRECTIVE('mandatory_subdirective2_name', STORE( ... )),
   DIRECTIVE('mandatory_subdirective3_name', SECTION( ... ))
  )

=item B<    STORE()>

Defines a value template. For every argument that a directive requires, we need to specify
one STORE(), MAP() or OVERWRITE() macro. Receives output from macros AS(), FROM(), TO(),
KEY(), SUBKEY(), POSTPARSER() and DEFAULT() and builds the value template.

In list context, it is equvalent to 

  'arg' => { value_template }

In scalar context, it is equivalent to 

  { value_template }

It can be used only within DIRECTIVE(), HIDDEN() and OPTIONAL() macros.

Typically it is defined as

  STORE(AS(CF_type), TO(hash_name), KEY(hash_key), SUBKEY(hash_2nd_level_key), DEFAULT('default_value'))

=item B<    MAP()>

This is the same as STORE() except it sets

  'type' => CF_MAPPED

within value template. It maps an entry in the map hash, defined by the directive's argument, to the
entry in the destination hash, defined by the hash key and, optionally, subkey. Typically it is defined
as

  MAP(FROM(map_hash_name), TO(hash_name), KEY(hash_key), SUBKEY(hash_2nd_level_key))

=item B<    OVERWRITE()>

This is the same as STORE() except it sets

  'overwrite' => 1

within value template. Allows parsed argument to be re-defined in the destination hash. Typical usage
is the same as for STORE() or MAP():

  OVERWRITE(AS(CF_type), TO(hash_name), KEY(hash_key), SUBKEY(hash_2nd_level_key), DEFAULT('default_value'))

=item B<    AS()>

Defines the data type of the argument.

It is equivalent to

  'type' => CF_xxxxxx

It can be used within STORE() and MAP() macros. Receives CF_ data type constants
ORed together and returns a single entry hash (key => value pair).

=item B<    FROM()>

Defines a hash name of the global hash to be used as map.

It is equivalent to

  'map' => 'MAPNAME'

within a value template. It can be used within STORE() and MAP() macros.
Receives a hash name of the global hash to be used as map for CF_MAPPED
data types and returns a single entry hash (key => value pair).

=item B<    TO()>

Defines a name of the hash to be used as destination for produced values.

It is equivalent to

  'hash' => 'DESTINATION_NAME'

within a value template. It can be used within STORE(), OVERWRITE() and MAP()
macros. Receives a name of the hash to be used as destination for produced
values and returns a single entry hash (key => value pair).

=item B<    KEY()>

Defines a format template or a name of a hash key to be used as the first
level key for the destination hash.

It is equivalent to

  'key' => 'HASH_KEY_NAME'

within a value template. It can be used within STORE(), OVERWRITE() and MAP()
macros. Receives a format template or a name of a hash key and returns a single
entry hash (key => value pair).

=item B<    SUBKEY()>

Defines a format template or a name of a hash subkey to be used as the second 
level key for the destination hash.

It is equivalent to

  'subkey' => 'HASH_SUBKEY_NAME'

within a value template. It can be used within STORE(), OVERWRITE() and MAP()
macros. Receives a format template or a name of a hash subkey to be used as
the second level key for the destination hash and returns a single entry hash
(key => value pair).

=item B<    POSTPARSER()>

Defines a reference to the function that will be used as post-parser callback.

It is equivalent to

  'postparser' => \&postparser_callback_sub

within a value template. It can be used within STORE(), OVERWRITE() and MAP()
macros. Receives a reference to the function that will be used as post-parser 
callback and returns a single hash entry (key => value pair).

=item B<    DEFAULT()>

Defines a string that will be parsed into a default value for the given argument.

It is equivalent to

  'default' => 'default_argument_value'

within a value template. It can be used within STORE(), OVERWRITE() and MAP()
macros. Receives a string that will be parsed into the default value for the
given argument and returns a single hash entry (key => value pair).

=back

Yes, it looks overbloated for such a simple task like nesting a few hashes.
Thats why it is not enabled by default. Still, if you don't want to think about
all those curly brackets, this is your way out. A few brackets still remain, tho :)

None of this is actually human readable, but it is ment to be used by coders,
not humans anyway :D

=head1 SEE ALSO

Nothing directly related to this project. You may want to take a look at other
configuration parsing modules:

    Config::Context
    Config::Context::ConfigScoped
    Config::Scoped
    Config::ApacheFormat

=head1 AUTHOR

Marko Dinic, E<lt>marko@yu.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2018 by Marko Dinic, All Rights Reserved

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.


=cut
