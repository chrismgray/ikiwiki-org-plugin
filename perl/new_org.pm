#!/usr/bin/perl
package IkiWiki::Plugin::new_org;

use warnings;
use strict;
use IkiWiki 3.00;
use File::Temp qw/ tempfile unlink0 /;

my $org_file_regexp = /\.org$/;

sub run_func_in_emacs($$) {
    my $func = shift;
    my $content = shift;
    my ($tf1, $tn1) = tempfile(); # Perl output, emacs input
    my ($tf2, $tn2) = tempfile(); # Perl input, emacs output
    my ($tf3, $tn3) = tempfile(); # Signal file
    print $tf1, $content;
    system("emacs -s org-ikiwiki-compiler --eval \"(ikiwiki-org-$func $tn1 $tn2 $tn3)\"");
    # Wait for emacs to finish
    while (-e $tn3) {
	usleep(100);
    }
    my $ret = <$tf2>;
    # After debugging
    # unlink0($tf1, $tn1);
    # unlink0($tf2, $tn2);
    return $ret;
}

sub import {
    system("emacsclient -s org-ikiwiki-compiler --eval \"(+ 1 1)\"");
    if ($? != 0) {
	system("emacs --daemon --eval \"(progn (require 'ikiwiki-org-plugin) (setq server-name \\\"org-ikiwiki-compiler\\\") (server-start))\"");
	if ($? != 0) {
	    print STDERR, "Failed to start emacs. Will not continue with new_org setup.\n";
	    return
	}
    }
    hook(type => "htmlize", id => "org", call => \&htmlize);
    hook(type => "linkify", id => "org", call => \&linkify, first => 1);
    hook(type => "scan", id => "org", call => \&scan);
}

sub linkify(@) {
    my %params = @_;
    my $page_file_name = $pagesources{$params{page}};
    if ($page_file_name =~ $org_file_regexp) {
	return $params{content} = run_func_in_emacs("linkify", $params{content});
    }
    return $params{content}
}

sub scan(@) {
    my %params = @_;
    my $page_file_name = $pagesources{$params{page}};
    if ($page_file_name =~ $org_file_regexp) {
	my @lines = split /\n/, run_func_in_emacs("scan", $params{content});
	foreach my $line (@lines) {
	    chomp $line;
	    add_link($params{page}, $line);
	}
    }
}

sub htmlize(@) {
    my %params = @_;
    return run_func_in_emacs("htmlize", $params{content});
}
