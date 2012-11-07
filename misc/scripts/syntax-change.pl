#!/usr/bin/perl

# Scipt used to fix up acumen progs. after syntax change: [=] to = and
#  := to =.  Use with extreme care.  Designed to be used on version
# control files and thus won't backup files that it is modifying.

while (<STDIN>) {
    chomp;
    my $f = $_;
    print "$f\n";
    open F, "$f";
    my $res = '';
    while (<F>) {
        unless (/val / && $f =~ /.scala$/) {
            my $rest = '';
            if (/(.*)( for .*)/s) {
                $_ = $1; $rest = $2;
            }
            s/([^~<>\[=])\=([^\]=])/$1:=$2/g;
            $_ .= $rest;
            s/\[\=\]/=/g;
        }
        $res .= $_;
    }
    open F, ">$f";
    print F $res
}
