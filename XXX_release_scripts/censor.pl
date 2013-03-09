#!/usr/bin/perl

use strict;
use warnings;

$/ = undef;  # see perlvar man page for meaning

use File::Find;

if (! -e "READY_FOR_CENSOR" ) {
    print "This script is a destructive.  It will remove code.\n\n";
    print "Will not continue unless the file \"READY_FOR_CENSOR\" is created\n\n";
    exit (1);
}

$SIG{__DIE__} = sub {
    die @_ if $^S;
    local $_ = $_[0];
    chop $_;
    print "$_  Aborting.\n";
    exit 2;
};


print "Removing all XXX* files\n";
my $find_err_string = "Problem when finding XXX Files to remove";
my $xargs_err_string = "Problem when removing XXX Files";

open F, "find . -iname 'xxx*' -print0 | " or die $find_err_string;
my $find_res = <F>;
close F or die $find_err_string;

open F, "| xargs -0 rm -rf" or die $xargs_err_string;
print F <F>;
close F or die $xargs_err_string;

File::Find::find(sub {
    return unless /^.*\.scala\z/si;
    my $path = $File::Find::name;
    my $file = $_;
    open F, "$file" or die "Unable to read $path";
    $_ = <F>;
    my $res = s/^.*\/\/ *NRL.*\n//mg;
    return unless $res > 0;
    open F, ">$file" or die "Cannot right to $path";
    print "Censoring $file\n";
    print F $_;
    close F;
}, 'src/');

unlink "READY_FOR_CENSOR";

