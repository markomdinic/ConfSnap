#
# api::util::git.pm
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

package api::util::git;

##########################################################################################

use strict;
use warnings;

##########################################################################################

#
# Module constructor
#
#  This function is called in order to
#  create and initialize GIT object.
#
#   Input:	1. class name (passed implicitly)
#		2. (optional) full path to git executable
#
#   Output:	1. FIFO queue object reference
#
sub new($;$)
{
    my ($class, $git) = @_;

    # If path to git executable wasn't given as input,
    # we need to look for it in most likely places
    unless(defined($git) && $git ne "" && -x $git) {

	my @likely_git_locations = qw(/usr/bin /usr/local/bin /bin /opt/bin /usr/sbin /usr/local/sbin /sbin /opt/sbin /usr/local/git/bin /usr/local/git /opt/git/bin /opt/git);

	# Look for git executable in likely locations first
        foreach my $path (@likely_git_locations) {
	    $path .= '/git';
	    if(-x $path) {
		$git = $path;
		last;
	    }
	}

	# If git wasn't found in any of the likely places,
	# ask system for assistance ...
	unless(defined($git) && $git ne "" && -x $git) {
	    # Use whereis command to find git
	    my @whereis = `whereis git`;
	    w: foreach my $line (@whereis) {
		# Strip trailing newline
		chop($line);
		# Split each line of whereis output
		# into a list of paths
		my @paths = split(/\s+/, $line);
		foreach my $path (@paths) {
		    if(-x $path) {
			$git = $path;
			last w;
		    }
		}
	    }
	}

	# Give up if git executable still hasn't been found
	return undef unless(defined($git) && $git ne "" && -x $git);

    }

    my $self = {
	'cmd' => $git
    };

    return bless($self, $class);
}
#
# Generic git wrapper
#
#  This function provides a Perl look&feel to the git command.
#  Takes the same arguments as git command itself.
#
#   Input:	1. self object reference (passed implicitly)
#		2+ git command line arguments
#
#   Output:	1. git command output, if successful
#		   undef, if failed
#
sub git($;@)
{
    my $self = shift;

    # Don't waste my time
    return 0 unless(scalar(@_) > 0 &&
		    defined($self->{'cmd'}) &&
		    $self->{'cmd'} ne "" &&
		    -x $self->{'cmd'});

    # Format full git command line
    my $cmd = $self->{'cmd'}." ".join(' ', @_)." 2>/dev/null";

    # Run git command and return it's stdout
    return `$cmd`;
}
#
# git init
#
#  Performs repository initialization
#
#   Input:	1. self object reference (passed implicitly)
#		2. full path to future git repository
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub init($$)
{
    my ($self, $path) = @_;

    return 0 unless(defined($path) && $path ne "" && -d $path);

    return defined($self->git('init', $path)) ? 1:0;
}
#
# git config
#
#  This method sets GIT repository-specific config options.
#
#   Input:	1. self object reference (passed implicitly)
#		2. config option name
#		3. config option value
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub config($$)
{
    my ($self, $option, $value) = @_;

    return 0 unless(defined($option) && $option ne "" && defined($value));

    return defined($self->git('config', $option, $value)) ? 1:0;
}
#
# git status
#
#  Reports current status of the active branch.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. git status text, if succeeded
#		   undef, if failed
#
sub status($)
{
    my $self = shift;

    return $self->git('status');
}
#
# git branch
#
#  This method creates new branch.
#
#   Input:	1. self object reference (passed implicitly)
#		2. branch name
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub branch($$)
{
    my ($self, $branch) = @_;

    return 0 unless(defined($branch) && $branch ne "");

    return defined($self->git('branch', $branch)) ? 1:0;
}
#
# git checkout
#
#  This method switches the active branch or creates
#  temporary, read-only version of the branch when
#  checking out specific commit.
#
#   Input:	1. self object reference (passed implicitly)
#		2. branch name or commit hash
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub checkout($$)
{
    my ($self, $target) = @_;

    return 0 unless(defined($target) && $target ne "");

    return defined($self->git('checkout', $target)) ? 1:0;
}
#
# git add
#
#  This method adds one or more files or directories
#  to the staging area, to be commited or discarded
#
#   Input:	1. self object reference (passed implicitly)
#		2. path to the directory or file
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub add($$)
{
    my ($self, $path) = @_;

    return 0 unless(defined($path) && $path ne "" &&
		    (-d $path || -f $path));

    return defined($self->git('add', '-A', $path)) ? 1:0;
}
#
# git rm
#
#  This method removes one or more files or directories
#  from the current repository branch.
#
#   Input:	1. self object reference (passed implicitly)
#		2. path to the directory or file
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub rm($$)
{
    my ($self, $path) = @_;

    return 0 unless(defined($path) && $path ne "" &&
		    (-d $path || -f $path));

    return defined($self->git('rm', '-rf', $path)) ? 1:0;
}
#
# git stash
#
#  Put pending changes to a stack to prevent them from being
#  lost due to other repo operations (like checkout or pull).
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub stash($)
{
    my $self = shift;

    return defined($self->git('stash')) ? 1:0;
}
#
# git stash pop
#
#  Restore stashed changes. It will perform merge with changes
#  made to the repository after our own changes were stashed.
#  If our changes conflict with changes made in the meantime,
#  this operation will fail.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub unstash($)
{
    my $self = shift;

    return defined($self->git('stash', 'pop')) ? 1:0;
}
#
# git reset
#
#  Without path as parameter, this method unstages all files
#  previosly added to the staging area. When path is given,
#  it unstages specified file only. Contents of unstaged files
#  are not modified in any way.
#
#   Input:	1. self object reference (passed implicitly)
#		2. path to the directory or file
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub reset($;$)
{
    my ($self, $path) = @_;

    if(defined($path) && $path ne "" &&
       (-d $path || -f $path)) {
	return $self->git('reset', $path);
    }

    return defined($self->git('reset')) ? 1:0;
}
#
# git push
#
#  Push commited changes to the origin server. This is a dumbed
#  down version of git push, as it performs only the basic push,
#  without any optional parameters.
#
#   Input:	1. self object reference (passed implicitly)
#		2. remote branch to push to
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub push($$)
{
    my ($self, $branch) = @_;

    return 0 unless(defined($branch) && $branch ne "");

    return defined($self->git('push', 'origin', $branch)) ? 1:0;
}
#
# git pull
#
#  Pull most recent state of given branch.
#
#   Input:	1. self object reference (passed implicitly)
#		2. (optional) remote branch to pull
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub pull($;$)
{
    my ($self, $branch) = @_;

    return 0 unless(defined($branch) && $branch ne "");

    return defined($self->git('pull', 'origin', $branch)) ? 1:0;
}
#
# git commit
#
#  This method commits changes made to files added to the staging
#  area. Once commited, they are permanently recorded in history
#  and the staging area is emptied.
#
#   Input:	1. self object reference (passed implicitly)
#		2. comment for the commit
#		3. (optional) commit author's full name
#		4. (optional) commit author's email
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub commit($$;$$)
{
    my ($self, $comment, $author, $email) = @_;

    return 0 unless(defined($comment) && $comment ne "");

    # Commit must have a comment
    my $args = "-m \"".$comment."\"";
    # Optionally, it can have author name and email
    if(defined($author) && $author ne "" &&
       defined($email) && $email ne "") {
	# Add name and email do the arguments
	$args .= " --author=\"".$author." <".$email.">\"";
    }

    return defined($self->git('commit', $args)) ? 1:0;
}
#
# git log
#
#  This method returns current branch's commit log.
#
#   Input:	1. self object reference (passed implicitly)
#		2+ git-log command's optional arguments
#
#   Output:	1. commit log text, in scalar context
#		   array of commit log lines, in list context
#		   undef/empty array, if failed
#
sub log($;@)
{
    my $self = shift;

    my $log = $self->git('log', @_);

    return wantarray ? ((defined($log) && $log ne "") ? split(/[\n\r]+/, $log):()):$log;
}
#
# git show (-s --format=%ct)
#
#  This method returns given commit's timestamp.
#
#   Input:	1. self object reference (passed implicitly)
#		2. commit ID
#
#   Output:	1. commit's timestamp,
#		   undef, if failed
#
sub timestamp($$)
{
    my ($self, $commit) = @_;

    return $self->git('show', '-s', '--format=%ct', $commit);
}
#
# git diff
#
#  This method returns diff between
#   a) staging area and the most recent commit,
#      if called without arguments
#   b) staging area and the given commit,
#      if called with a single argument
#   c) two given commits, if called with two arguments
#
#   Input:	1. self object reference (passed implicitly)
#		2. (optional) git-diff command's optional arguments
#
#   Output:	1. diff text, in scalar context
#		   array of diff lines, in list context
#		   undef/empty array, if failed
#
sub diff($;$$)
{
    my $self = shift;

    my $diff = $self->git('diff', @_);

    return wantarray ? ((defined($diff) && $diff ne "") ? split(/[\n\r]+/, $diff):()):$diff;
}
#
# git rev-list
#
#  This method returns a list of commits starting
#  from given index (step) in commit history, 0 being
#  the most current commit, 1 being previous commit,
#  and so on ... If no index is given, list starts
#  at most current commit (index 0 is assumed).
#
#   Input:	1. self object reference (passed implicitly)
#		2. (optional) commit history index
#		3+ (optional) paths to show commits for
#
#   Output:	1. (list context)   an array of commit hashes,
#                                   empty if failed
#		   (scalar context) a string of commit hashes,
#		                    undef, if failed
#
sub list($;$@)
{
    my $self = shift;
    my $index = shift;

    $index = 0 unless(defined($index) && $index =~ /^\d+$/ && $index >= 0);

    my $list = $self->git('rev-list', '--skip='.$index, 'master', '--', @_);

    return wantarray ? ((defined($list) && $list ne "") ? split(/[\n\r]+/, $list):()):$list;
}
#
# git rev-list --before
#
#  This method returns a list of commits recorded
#  before given time. All time formats supported
#  by standard C library are supported by git.
#
#   Input:	1. self object reference (passed implicitly)
#		2. time string
#		3+ (optional) paths to show commits for
#
#   Output:	1. (list context)   an array of commit hashes,
#                                   empty if failed
#		   (scalar context) a string of commit hashes,
#		                    undef, if failed
#
sub list_before($$;@)
{
    my $self = shift;
    my $time = shift;

    return 0 unless(defined($time) && $time ne "");

    my $list = $self->git('rev-list', '--before='.$time, 'master', '--', @_);

    return wantarray ? ((defined($list) && $list ne "") ? split(/[\n\r]+/, $list):()):$list;
}
#
# git rev-list --after
#
#  This method returns a list of commits recorded
#  after given time. All time formats supported
#  by standard C library are supported by git.
#
#   Input:	1. self object reference (passed implicitly)
#		2. time string
#		3+ (optional) paths to show commits for
#
#   Output:	1. (list context)   an array of commit hashes,
#                                   empty if failed
#		   (scalar context) a string of commit hashes,
#		                    undef, if failed
#
sub list_after($$)
{
    my $self = shift;
    my $time = shift;

    return 0 unless(defined($time) && $time ne "");

    my $list = $self->git('rev-list', '--after='.$time, 'master', '--', @_);

    return wantarray ? ((defined($list) && $list ne "") ? split(/[\n\r]+/, $list):()):$list;
}
#
# git remote set-url
#
#  This method unconditionally sets the new origin URL
#  for current repository, replacing the previous one.
#
#   Input:	1. self object reference (passed implicitly)
#		2. URL to set as origin
#
#   Output:	none
#
sub remote_set($$)
{
    my ($self, $url) = @_;

    return unless(defined($url) && $url ne "");

    $self->git('remote', 'set-url', 'origin', $url);
}
#
# git remote add
#
#  This method adds a new origin URL for current repository,
#  if it hasn't already been defined.
#
#   Input:	1. self object reference (passed implicitly)
#		2. URL to set as origin
#
#   Output:	none
#
sub remote_add($$)
{
    my ($self, $url) = @_;

    return unless(defined($url) && $url ne "");

    $self->git('remote', 'add', 'origin', $url);
}
#
# git remote show
#
#  This method returns information about configured origin
#  for current local repository, packed as a hash.
#
#   Input:	1. self object reference (passed implicitly)
#
#   Output:	1. reference to hash with origin information, if succeeded
#		   undef, if failed
#
sub remote_show($)
{
    my $self = shift;

    my @show = $self->git('remote', 'show', 'origin');
    return undef unless(@show);

    my %remote = ();

    while(@show) {
	# Get a line of git-remote show output
	my $line = shift @show;

	# Split the line into parameter name and parameter value
	my ($param, $value) = ($line =~ /^\s*([^:]+)\s*:\s*(.+?)\s*$/);
	next unless(defined($param) && $param ne "" &&
		    defined($value) && $value ne "");
	# Fetch URL parameter ?
	if($param =~ /^Fetch\s+URL$/i) {
	    $remote{'fetch'} = $value;
	# Push URL parameter ?
	} elsif($param =~ /^Push\s+URL$/i) {
	    $remote{'push'} = $value;
	# What branch our HEAD is on ?
	} elsif($param =~ /^HEAD\s+branch$/i) {
	    $remote{'HEAD'} = $value;
	# What is the default remote branch ?
	} elsif($param =~ /^Remote\s+branch$/i) {
	    $remote{'remote'} = [ split(/\s+/, shift @show) ];
	}
    }

    return scalar(keys %remote) > 0 ? \%remote:undef;
}

1;
