#! /usr/bin/perl -w

# doxytag.pl
#
# $Id: doxytag.pl,v 1.3 2001/04/28 23:49:58 ryants Exp $
#
# This perl script creates an easier-to-parse tag file from Doxygen-created HTML files.
#
# Copyright (C) 2001 Ryan T. Sammartino
# http://members.home.net/ryants/
# ryants@home.com
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


my @html_files = glob "*.html";

foreach $html_file (@html_files) {
  open F, $html_file or die "Could not open $html_file for reading: $!\n";

  my $compound_entry_done = 0;
  my $compound_for_this_file = $html_file;

  while (<F>) {
    if (/<a\s+name\s*=\s*\"([\w\d]+)\"\s+doxytag\s*=\s*\"([\w\d\.]+)::(\w+)\"/) {
      my $anchor = $1;
      my $compound = $2;
      my $member = $3;

      if (!$compound_entry_done) {
	print "$compound\t$html_file";
	if ($html_file =~ /^class_$compound/) {
	  print "\tclass $compound\n";
	} elsif ($html_file =~ /^struct_$compound/) {
	  print "\tstruct $compound\n";
	} elsif ($html_file =~ /^union_$compound/) {
	  print "\tunion $compound\n";
	} elsif ($html_file =~ /^namespace_$compound/) {
	  print "\tnamespace $compound\n";
	} else {
	  print "\tfile $compound\n";
	}
	$compound_for_this_file = $compound;
	$compound_entry_done = 1;
      }

      print "$member\t$html_file#$anchor\t$compound\:\:$member\n";
    } elsif (/<a\s+name\s*=\s*\"([\w\d]+)\"\s+doxytag\s*=\s*\"([\w\d\.]+)\"/) {
      my $anchor = $1;
      my $member = $2;

      print "$member\t$html_file#$anchor\t",
	"$compound_for_this_file\:\:$member\n";
    }
  }

  close F;
}
