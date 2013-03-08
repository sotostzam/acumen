#!/usr/bin/perl

use strict;
use warnings;

use File::Find;

if (! -e "READY_FOR_CENSOR" ) {
    print "This script is a destructive.  It will remove code.\n\n";
    print "Will not continue unless the file \"READY_FOR_CENSOR\" is created\n\n";
    exit (1);
}

print "Removing all XXX* files\n";
system("find . -iname 'xxx*' -print0 | xargs -0 rm -rf");

File::Find::find(sub {
    return unless /^.*\.scala\z/si;
    my $file = $_;
    local $/ = undef;
    open F, "$file" or die "Unable to open $file";
    $_ = <F>;
    my $res = s/^.*\/\/ *NRL.*\n//mg;
    return unless $res > 0;
    open F, ">$file" or die "Cannot right to $file";
    print "Censoring $file\n";
    print F $_;
    close F;
}, 'src/');

unlink "READY_FOR_CENSOR";

