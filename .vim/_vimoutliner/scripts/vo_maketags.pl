#!/usr/bin/perl -w
# #######################################################################
# vo_maketags.pl: Vim outline tagging system, main program, version 0.1.2
#   Copyright (C) 2001-2003 by Steve Litt (slitt@troubleshooters.com)
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
# Steve Litt, slitt@troubleshooters.com, http://www.troubleshooters.com
# #######################################################################

# #######################################################################
# #######################################################################
# #######################################################################
# HISTORY
# V0.1.0 Pre-alpha
#     Starting at a "top level" indent-defined Vim outline, this
#     program finds all "tags" defined as headlines starting with
#     _tag_, and containing a subheadline containing the file
#     to which the tag should jump. This program creates a tags
#     file.
#Steve Litt, 5/28/2001
#End of version 0.1.0
#
# V0.1.1 Pre-alpha
#     Bug fixes, including ../ resolution
#
#Steve Litt, 5/28/2001
#End of version 0.1.1
#
#
# V0.1.2 Pre-alpha
#	More bug fixes, and facility to create a new outline
#	file from a tag whose corresponding file doesn't yet
#	exist.  
#Steve Litt, 5/30/2001
#End of version 0.1.2
#
# V0.1.3 Pre-alpha
#	More bug fixes. This was the first version released
#	file from a tag whose corresponding file doesn't yet
#	exist.  
#Steve Litt, 6/01/2001
#End of version 0.1.3
#
# V0.2.0 Pre-alpha
#Steve Litt, 12/03/2002
#	This file unchanged. The overall Vimoutliner version
#	0.2.0 has extensive improvements, including intuitive
#	collapse/expand.
#End of version 0.2.0
#END OF HISTORY
#
#
# V0.1.2 Pre-alpha
#	More bug fixes, and facility to create a new outline
#	file from a tag whose corresponding file doesn't yet
#	exist.  
#Steve Litt, 5/30/2001
#End of version 0.1.2
#END OF HISTORY
#
# #######################################################################


use strict;
use vars qw($TAGFILENAME);
use Cwd;

$TAGFILENAME = $ENV{"HOME"} . "/.vimoutliner/vo_tags.tag";

sub process1Outline($$); #Early prototype the recursive routine
sub makeDirectory($);    #Early prototype the recursive routine

sub makeTagFileStartingAt($)
	{
	unless(@ARGV == 1)
		{
		usage();
		die;
		}
	my($absoluteFileName) =  deriveAbsoluteFileName(Cwd::cwd(), $_[0]);
	
	my(%processedFiles) = ();
	recordFileAsProcessed($absoluteFileName,\%processedFiles);
	unlink $TAGFILENAME;
	process1Outline($absoluteFileName, \%processedFiles);
	sortAndDeleteDupsFromTagFile();
	}

sub sortAndDeleteDupsFromTagFile()
	{
	my($TEMPTAGFILENAME) = "$ENV{'HOME'}/temptagfile.tag";
	system("sort $TAGFILENAME | uniq > $TEMPTAGFILENAME"); 
	system("rm $TAGFILENAME");
	system("mv $TEMPTAGFILENAME $TAGFILENAME");
	}


sub process1Outline($$)
	{
	my($fileName) = $_[0];
	my($processedFilesHashRef) = $_[1];
	
	unless(fileExists($fileName))
		{
		makeDirectory($fileName);
		makeEmptyFile($fileName);
		}

	print "Begin processing file $fileName.\n";

	my($baseDirectory) = getBaseDirectory($fileName);
	my(%tags) = getTagsFromFile($fileName);
	my(@tagKeys) = keys(%tags);
	my($tagKey);
	foreach $tagKey (@tagKeys)
		{
		my($absoluteFileName);
		if(isAbsoluteFilePath($tags{$tagKey}))
			{
			$absoluteFileName = $tags{$tagKey};
			}
		else
			{
			$absoluteFileName =
			 deriveAbsoluteFileName($baseDirectory, $tags{$tagKey});
			}
		appendTagToTagFile($tagKey,$absoluteFileName);
 		if(notProcessedYet($absoluteFileName, $processedFilesHashRef))
 			{
			recordFileAsProcessed($absoluteFileName,$processedFilesHashRef);
 			process1Outline($absoluteFileName, $processedFilesHashRef);
 			}
		}
	}

sub appendTagToTagFile($$)
	{
	open(TAGFILE, ">>$TAGFILENAME");
	print TAGFILE "$_[0]	$_[1]	:1\n";
	close(TAGFILE);
	}


sub makeEmptyFile($)
	{
	open(OUTLINEFILE, ">" . $_[0]);
	close(OUTLINEFILE);
	}


sub makeDirectory($)
	{
	my($completeFileName) = $_[0];
	my($directoryName) = ($completeFileName =~ m/^(.*?)[^\/]*$/);
	unless($directoryName eq "")
		{
		my($temp) = ($directoryName =~ m/^(.*).$/);
		makeDirectory($temp);
		print "Creating $directoryName...";
		if(mkdir $directoryName)
			{
			print " succeeded.\n";
			}
		else
			{
			print " no action: $!.\n";
			}
		}
	}

sub fileExists($)
	{
	my($outlineFileName) = $_[0];
	my($success) = open(OUTLINEFILE, "<" . $outlineFileName);
	if($success)
		{
		close(OUTLINEFILE);
		return(1);
		}
	else
		{
		return(0);
		}
	}

sub getTagsFromFile($)
	{
	my($outlineFileName) = $_[0];
	my(%tags);
	my($tagString) = "";
	my($success) = open(OUTLINEFILE, "<" . $outlineFileName);
	unless($success)
		{
		print "Failed to open $outlineFileName\n";
		return(());
		}
	while(<OUTLINEFILE>)
		{
		my($line) = $_;
		chomp($line);
		if($line =~ m/^\s*(_tag_\S+)/)
			{
			$tagString = $1;
			}
		elsif($tagString ne "")
			{
			$line =~ m/^\s*(\S+)/;
			my($filename) = $1;
			$tags{$tagString} =
			  deriveAbsoluteFileName(getBaseDirectory($_[0]), $1);
			$tagString = "";
			}
		}	
	return(%tags);
	}

sub recordFileAsProcessed($$)
	{
	my($absoluteFileName) = $_[0];
	my($processedFilesHashRef) = $_[1];
	${$processedFilesHashRef}{$absoluteFileName} = "1";
	}

sub notProcessedYet($$)
	{
        my($absoluteFileName) = $_[0];
	my(%processedFiles) = %{$_[1]};
	if(defined($processedFiles{$absoluteFileName}))
		{
		return(0);
		}
	else
		{
		return(1);
		}
	}

sub dia($)
	{
	print "dia " . $_[0] . "\n";
	}


sub isAbsoluteFilePath($)
	{
	if($_[0] =~ m/^\//)
		{
		return 1;
		}
	else
		{
		return 0;
		}
	}

sub getFileNameOnly($)
	{ 
	my($fileString); 
	if ($_[0] =~ m/.+\/(.*)$/)
		{
		$fileString= $1
		}
	else
		{
		$fileString = $_[0];
		}

	return $fileString;
	}

sub getBaseDirectory($)
	{ 
	my($dirString) = ($_[0] =~ m/(.+\/).*$/);
	return $dirString;
	}

sub deriveAbsoluteFileName($$)
	{
	my($absoluteFileName);
	my($baseDirectory) = $_[0];
	my($passedFileName) = $_[1];
	unless($baseDirectory =~ m/\/$/)
		{
		$baseDirectory= $baseDirectory . "/"; 
		}
	if($passedFileName =~ m/^\//)
		{
		$absoluteFileName = $passedFileName;
		}
	else
		{
		$absoluteFileName = $baseDirectory . $passedFileName;
		}

	$absoluteFileName =~ s/\/\.\//\//g;  #remove all "./";
	deleteDoubleDots($absoluteFileName);

	return($absoluteFileName);
	}

sub deleteDoubleDots($)
	{
	while($_[0] =~ m/\.\./)
		{
		$_[0] =~ s/\/[^\/]*\/\.\.//;
		}
	}

sub usage()
	{
	print "\nUsage is:\n";
	print "otltags topLevelOutlineFileName\n\n";
	}
	

makeTagFileStartingAt($ARGV[0])

