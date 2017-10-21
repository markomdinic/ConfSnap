#
# api::base::vcs.pm
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

package api::base::vcs;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use Cwd;
use Time::ParseDate;

##########################################################################################

use api::util::git;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. hashref to the global configuration
#
#   Output:	1. api::base::vcs object reference
#
sub new($$) {
    my ($class, $conf) = @_;

    # Global configuration must be defined
    return undef unless(defined($conf) &&
			ref($conf) eq "HASH");

    # VCS repository base dir must be defined
    return undef unless(defined($conf->{'datadir'}) &&
			$conf->{'datadir'} ne "" &&
			-d $conf->{'datadir'});

    # Initialize GIT
    my $git = api::util::git->new();
    return undef unless defined($git);

    # Create base VCS object
    my $self = bless({ 'git' => $git }, $class);

    # Initialize repository
    $self->init_repository($conf->{'datadir'});

    return $self;
}
#
# Change working dir
#
#  This method changes working directory to a specified filesystem path
#  and returns previous working directory. If invoked without parameters,
#  it will change working directory to current repository base dir.
#
#  This method is here for convenience - it wraps two common directory
#  navigation functions, plus verifies that specified directory really
#  is a directory before attempting to jump to it.
#
#   Input:	1. self object reference
#		2. (optional) full path to new working directory
#
#   Output:	1. full path to previous working directory
#		   undef, if failed
#
sub cd($;$)
{
    my ($self, $path) = @_;

    # If new working directory isn't specified,
    # use our repository base dir
    $path = $self->{'repo'} unless(defined($path) && $path ne "");

    # Either way, if path isn't a proper directory - abort
    return undef unless -d $path;

    my $wd = getcwd();
    chdir($path);
    return $wd;
}
#
# Check if given path is a VCS repository
#
#  This method confirms that the given path is a path to an initialized
#  GIT repository dir. If given path is not a proper directory or it is
#  not initialized with GIT metadata, the check will fail.
#
#   Input:	1. self object reference
#		2. repository path to check
#
#   Output:	1. TRUE, if path is a VCS repository
#		   FALSE, if path is NOT a VCS repository
#
sub is_repository($$)
{
    my ($self, $path) = @_;

    # Path MUST be specified
    return 0 unless(defined($path) && $path ne "");

    # Change to specified path
    my $wd = $self->cd($path);
    return 0 unless(defined($wd) && $wd ne "");

    # Check GIT status
    my $res = $self->{'git'}->status();

    # Change back to previous working dir
    chdir($wd);

    return (defined($res) && $res ne "" ) ? 1:0;
}
#
# Initialize specified directory as a VCS repository
#
#  This method checks if specified path is a VCS repository
#  and if it is - simply reports success. If specified path
#  is NOT a VCS repository, it will initialize it with GIT
#  metadata and, if all goes well, report success. Specified
#  path MUST be a proper directory.
#
#   Input:	1. self object reference
#		2. full path to a VCS repository
#
#   Output:	1. TRUE, if initialization suceeded
#		   or path is already a VCS repository
#		   FALSE, if initialization failed
#		   or path is NOT a proper directory
#
sub init_repository($$)
{
    my ($self, $path) = @_;
    my $res = 1;

    # Path MUST be a proper directory
    return 0 unless(defined($path) &&
		    $path ne "" &&
		    -d $path);

    # If path is not a GIT repository ...
    if(!$self->is_repository($path)) {
	# ... initialize it as one
	$res = $self->{'git'}->init($path);
    }

    # If repository is initialized ...
    if($res) {
	# ... strip trailing '/', if any
	$path =~ s/[\/]+$//g;
	# ... keep full path to the repository
	$self->{'repo'} = $path;
    }

    return $res;
}
#
# Unstage a staged file or directory
#
#  This method will unstage previosly staged file or even
#  entire directories. It's a wrapper for the specific form
#  of GIT reset command that only unstages staged changes.
#
#   Input:	1. self object reference
#		2. full path to a filesystem object to unstage
#
#   Output:	1. TRUE, if suceeded
#		   FALSE, if failed
#
sub reset($;$)
{
    my ($self, $path) = @_;

    return $self->{'git'}->reset($path);
}
#
# Retrieve list of files affected by the last change
#
#  This method compares current state of repository with
#  the previous one and returns the list of files that
#  have their content changed.
#
#   Input:	1. self object reference
#
#   Output:	1. TRUE, if suceeded
#		   FALSE, if failed
#
sub changed_files($)
{
    my $self = shift;

    # Change to repo dir
    my $wd = $self->cd();

    # Get short log output
    my @files = $self->{'git'}->log('-1 --name-only --oneline');
    # Skip commit message line
    shift @files;

    # Change back to prev dir
    chdir($wd);

    return @files;
}
#
# Commit changes to history
#
#  This method adds specified filesystem objects to the staging
#  area and then records changes made to their content in history.
#
#  If no files or directories are specified, entire VCS repository
#  will be staged.
#
#  If no changes have occured in any of the specified objects since
#  last commit, staged objects will be unstaged and commit operation
#  will be aborted.
#
#  If remote repository (origin server) is configured, this method
#  will synchronize with it prior to commiting local changes. After
#  successful commit, it will push changes to the remote repository.
#
#   Input:	1. self object reference
#		2. commit message (comment string)
#		3+ (optional) array of paths to changed files
#		              to be commited to history.
#
#   Output:	1. TRUE, if suceeded
#		   FALSE, if failed
#
sub commit($$;@)
{
    my $self = shift;
    my $message = shift;

    # Don't waste time
    return 0 unless(defined($message) && $message ne "");

    # Change to repo dir
    my $wd = $self->cd();

    # Assume everything is ok, by default
    my $res = 1;

    # Remote repository configured ?
    if(defined($self->{'conf'}->{'remote_repository_url'}) &&
       $self->{'conf'}->{'remote_repository_url'} ne "") {
	# Check if our local repository has
	# the remote repository configured
	my $remote = $self->{'git'}->remote_show();
	# If our local VCS repository doesn't have
	# remote repository configured ...
	unless(defined($remote) && ref($remote) eq "HASH") {
	    # ... Add remote repository URL
	    $self->{'git'}->remote_add($self->{'conf'}->{'remote_repository_url'});
	    # If our local VCS repository is configured
	    # with some other remote repository ...
	} elsif((defined($remote->{'fetch'}) && $remote->{'fetch'} ne $self->{'conf'}->{'remote_repository_url'}) ||
	        (defined($remote->{'push'}) && $remote->{'push'} ne $self->{'conf'}->{'remote_repository_url'})) {
	    # ... replace remote repository URL
	    $self->{'git'}->remote_set($self->{'conf'}->{'remote_repository_url'});
	}
	# Put changed content into stash
	$res = $self->{'git'}->stash();
	if($res) {
	    # Synchronize with remote repository
	    $res = $self->{'git'}->pull("master");
	    if($res) {
		# Restore stashed changes
		$res = $self->{'git'}->unstash();
	    }
	}
    }

    # If everything is still in order,
    # add changes to the staging area
    if($res) {
	# If no files or directories are given,
	# perform commit on entire repository
	push @_, "." unless(scalar(@_) > 0);

	# Add all given targets to staging area
	foreach my $target (@_) {
	    # If target path contains repo dir,
	    # make it relative to it
	    $target =~ s/^$self->{'repo'}(?:\/|$)/\.\//g;
	    # Add target to the staging area
	    $res = $self->{'git'}->add($target);
	    unless($res) {
		# If a single addition failed,
		# reset staging area, and abort
		$self->{'git'}->reset();
		last;
	    }
	}
    }

    if($res) {
	# Commit staged changes
	$res = $self->{'git'}->commit($message);
	# If commit was successfull and we have
	# the remote repository configured ...
	if($res &&
	   defined($self->{'conf'}->{'remote_repository_url'}) &&
	   $self->{'conf'}->{'remote_repository_url'} ne "") {
	    # ... push changes to the remote repository
	    $res = $self->{'git'}->push("master");
	    # If push failed ...
	    unless($res) {
		# ... forget everything that just happened
		$self->{'git'}->reset("--hard", "HEAD^1");
	    }
	}
    }

    # Change back to prev dir
    chdir($wd);

    return $res;
}
#
# Get the ID of snapshot at specific point in history
#
#  This method returns the snapshot ID (hash) for the snapshot
#  at the exact position (index) in change history.
#
#  It can either be global, for the entire VCS repository, or
#  localized to some specific part (directory or even a single
#  file) of the VCS repository, if specific paths are supplied.
#
#   Input:	1. self object reference
#		2. snapshot index
#		3+ (optional) the list of specific paths within
#		   VCS repository to constrain the search to.
#
#   Output:	1. Snapshot ID (hash), if found
#		   undef, if failed
#
sub snapshot_at($$;@)
{
    my $self = shift;
    my $index = shift;

    my $snapshot;

    # Change to repo dir
    my $wd = $self->cd();

    # Get commits from specified index
    my @commits = $self->{'git'}->list($index, @_);
    if(@commits) {
	# Commit at specified index
	$snapshot = shift @commits;
    }

    # Change back to prev dir
    chdir($wd);

    return $snapshot;
}
#
# Get the ID of snapshot taken at or before specified time
#
#  This method returns the snapshot ID (hash) for the snapshot
#  at or before specified point in time.
#
#  It can either be global, for the entire VCS repository, or
#  localized to some specific part (directory or even a single
#  file) of the VCS repository, if specific paths are supplied.
#
#   Input:	1. self object reference
#		2. flexible datetime string
#		3+ (optional) the list of specific paths within
#		   VCS repository to constrain the search to.
#
#   Output:	1. Snapshot ID (hash), if found
#		   undef, if failed
#
sub snapshot_before($$;@)
{
    my $self = shift;
    my $datetime = shift;

    my $snapshot;

    # Change to repo dir
    my $wd = $self->cd();

    # Get commits before specified date/time
    my @commits = $self->{'git'}->list_before(parsedate($datetime), @_);
    if(@commits) {
	# Commit immediately before
	# specified date/time
	$snapshot = shift @commits;
    }

    # Change back to prev dir
    chdir($wd);

    return $snapshot;
}
#
# Get the ID of snapshot taken at or after specified time
#
#  This method returns the snapshot ID (hash) for the snapshot
#  at or after specified point in time.
#
#  It can either be global, for the entire VCS repository, or
#  localized to some specific part (directory or even a single
#  file) of the VCS repository, if specific paths are supplied.
#
#   Input:	1. self object reference
#		2. flexible datetime string
#		3+ (optional) the list of specific paths within
#		   VCS repository to constrain the search to.
#
#   Output:	1. Snapshot ID (hash), if found
#		   undef, if failed
#
sub snapshot_after($$;@)
{
    my $self = shift;
    my $datetime = shift;

    my $snapshot;

    # Change to repo dir
    my $wd = $self->cd();

    # Get commits after specified date/time
    my @commits = $self->{'git'}->list_after(parsedate($datetime), @_);
    if(@commits) {
	# Commit immediately after
	# specified date/time
	$snapshot = pop @commits;
    }

    # Change back to prev dir
    chdir($wd);

    return $snapshot;
}
#
# Get snapshot's timestamp
#
#  This method returns the date and time the specified
#  snapshot was taken, in UNIX timestamp format.
#
#   Input:	1. self object reference
#		2. snapshot ID
#
#   Output:	1. UNIX timestamp
#		   undef, if failed
#
sub snapshot_timestamp($$)
{
    my ($self, $snapshot) = @_;

    # Change to repo dir
    my $wd = $self->cd();

    # Get snapshot's (commit's) timestamp
    my $timestamp = $self->{'git'}->timestamp($snapshot);

    # Change back to prev dir
    chdir($wd);

    return $timestamp;
}
#
# Retrieve the list of snapshots present in VCS repository
#
#  This method retrieves the list of snapshots kept in repository.
#  It can either be global, for the entire VCS repository, or
#  localized to some specific part (directory or even a single
#  file) of the VCS repository, if specific paths are supplied.
#
#  Snapshots are sorted in descending order, listing the most
#  current snapshot first, with the index of 0, previous snapshot
#  with the index of 1, etc.
#
#   Input:	1. self object reference
#		2+ (optional) the list of specific paths within
#		   VCS repository to constrain listing to.
#
#   Output:	1. (scalar context) formatted listing of snapshots
#		   as a single string, undef if failed
#		   (list context) formatted listing of snapshots
#		   as an array of lines, empty array if failed
#
sub snapshot_list($;@)
{
    my $self = shift;

    # Change to repo dir
    my $wd = $self->cd();

    my @log = $self->{'git'}->log('--pretty=format:"%h   %cd"', '--', @_);

    # Change back to prev dir
    chdir($wd);

    my $index = 0;

    # How many digits does our index have ?
    my $width_index_num = length(scalar(@log));
    # Word "Index" has 5 letters, and we have 2 spaces
    # between column headers, thus, index number must
    # be padded up to 5 spaces ...
    $width_index_num = 5 if($width_index_num < 5);
    # ... and column width cannot be less than 7
    my $width_index_col = $width_index_num + 2;

    # Format list header
    my @list = (sprintf("%-".$width_index_col."s%-10s%-32s", "Index", "ID", "Date"));
    push @list, "=" x ($width_index_col + 42);
    # Format the list itself
    foreach my $line (@log) {
	push @list, sprintf("%-".$width_index_num."d  ".$line, $index++);
    }

    return wantarray ? (@list):join("\n", @list)."\n";
}
#
# Change VCS repo to a state it had in specified point in time
#
#  This method temporarily changes content of the VCS directory
#  to the snapshot closest to the specified time. Optional time
#  parameter specifies whether we want the snapshot immediately
#  before or immediately after the specified UNIX timestamp by
#  using prefix signs '<' or '>' respectively.
#
#  If invoked without a time parameter, this method changes
#  VCS repository back to it's most current state.
#
#   Input:	1. self object reference
#		2. (optional) snapshot id (commit hash)
#
#   Output:	1. TRUE, if suceeded
#		   FALSE, if failed
#
sub checkout($;$)
{
    my ($self, $snapshot) = @_;
    my $commit;
    my $res = 0;

    # Change to repo dir
    my $wd = $self->cd();

    $res = (defined($snapshot) && $snapshot ne "") ?
		    # Checkout selected commit
		    $self->{'git'}->checkout($snapshot):
		    # If no id was given, checkout master branch
		    $res = $self->{'git'}->checkout('master');

    # Change back to prev dir
    chdir($wd);

    return $res;
}
#
# Make a diff between 2 specified snapshots
#
#  This method displays the diff between 2 specified snapshots.
#  Any snapshot that exists in change history for a path can be
#  compared to any other snapshot for the same path.
#
#   Input:	1. self object reference
#		2. path to the part of the VCS repository
#		   we are interested in
#		3. ID (hash) of the first snapshot to be compared
#		4. ID (hash) of the second snapshot to be compared
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub diff($$$$)
{
    my ($self, $path, $snapshot1, $snapshot2) = @_;
    my $diff;

    return 0 unless(defined($path) && $path ne "" &&
		    defined($snapshot1) && $snapshot1 ne "" &&
		    defined($snapshot2) && $snapshot2 ne "");

    # Change to repo dir
    my $wd = $self->cd();

    my (@diff_lines, $diff_text);

    if(wantarray) {
	@diff_lines = $self->{'git'}->diff($snapshot1, $snapshot2, $path);
    } else {
	$diff_text = $self->{'git'}->diff($snapshot1, $snapshot2, $path);
    }

    # Change back to prev dir
    chdir($wd);

    return wantarray ? @diff_lines:$diff_text;
}

1;
