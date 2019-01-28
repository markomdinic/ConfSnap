#
# api::component::storage.pm
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

package api::component::storage;

##########################################################################################

use strict;
use warnings;

##########################################################################################

use File::Path;

##########################################################################################

#
# Module constructor
#
#   Input:	1. class name (passed implicitly)
#		2. hashref to the global configuration
#
#   Output:	1. api::component::storage object reference
#
sub new($)
{
    my ($class, $conf) = @_;

    return undef unless(defined($conf) && ref($conf) eq "HASH");

    return bless({}, $class);
}
#
# Store content into file
#
#   Input:	1. self object reference
#		2. path to the destination file
#		3. content to be stored
#
#   Output:	1. TRUE, if succeeded
#		   FALSE, if failed
#
sub store($$$)
{
    my ($self, $path, $content) = @_;
    my $file;

    # Top level data dir must exist
    # and cannot be the root dir
    my $dest_dir = $self->{'conf'}->{'datadir'};
    return 0 unless(defined($dest_dir) &&
		    $dest_dir ne "" &&
		    $dest_dir ne "/" &&
		    -d $dest_dir);

    # Split path into directory and file parts
    my ($dir, $filename) = ($path =~ /^(?:\/?(.+?)\/)?([^\/]+)$/);
    # If given path contains a directory ...
    if(defined($dir) && $dir ne "") {
	# ... put together the final destination directory
	$dest_dir .= '/'.$dir;
	# If destination directory doesn't exist ...
	unless(-d $dest_dir) {
	    # ... create it
	    mkpath($dest_dir, 0, 755);
	}
    }

    # Open file for (over)writing
    open($file, '>'.$dest_dir.'/'.$filename)
	or return 0;

    # Write content to file
    syswrite($file, $content);

    # Close file
    close($file);

    return 1;
}
#
# Retrieve content from file
#
#   Input:	1. self object reference
#		2. path to the source file
#
#   Output:	1. content itself, if succeeded
#		   undef, if failed
#
sub retrieve($$)
{
    my ($self, $path) = @_;
    my ($file, $content);

    # Top level data dir must exist
    # and cannot be the root dir
    my $data_dir = $self->{'conf'}->{'datadir'};
    return 0 unless(defined($data_dir) &&
		    $data_dir ne "" &&
		    $data_dir ne "/" &&
		    -d $data_dir);

    # Split path into directory and file parts
    my ($dir, $filename) = ($path =~ /^(?:\/?(.+?)\/)?([^\/]+)$/);
    # If given path contains a directory ...
    if(defined($dir) && $dir ne "") {
	# ... put together base data directory
	$data_dir .= '/'.$dir;
    }

    my $file_path = $data_dir.'/'.$filename;

    # Determine file size
    my @stat = stat($file_path);
    return undef unless @stat;

    # Open file for reading
    open($file, $file_path)
	or return undef;

    # Read content from file
    sysread($file, $content, $stat[7])
	or $content = undef;

    # Close file
    close($file);

    return $content;
}

1;
