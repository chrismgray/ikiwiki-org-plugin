#!/usr/bin/perl
package IkiWiki::Plugin::new_org;

use warnings;
use strict;
use IkiWiki 3.00;
use File::Temp qw/ tempfile unlink0 /;
use IPC::Open2;

my $org_file_regexp = qr/\.org$/;

sub run_func_in_emacs($$;$) {
    my $func = shift;
    my $content = shift;
    my $args = shift;
    my ($tf1, $tn1) = tempfile("/tmp/ikiwiki-org-1-XXXXX"); # Perl output, emacs input
    my ($tf2, $tn2) = tempfile("/tmp/ikiwiki-org-2-XXXXX"); # Perl input, emacs output
    binmode($tf1, ":utf8");
    binmode($tf2, ":utf8");
    print $tf1 $content;
    if (!defined $args) { $args = ""; }
    # WARNING: possible security hole
    $args =~ s/'/'\\''/g;
    my $argstring = qq/unset ALTERNATE_EDITOR; emacsclient -s org-ikiwiki-compiler --eval '(ikiwiki-org-$func "$tn1" "$tn2" $args)'/;
    my $pid = open2(*IN, *OUT, $argstring);
    # Wait for emacs to finish
    waitpid($pid, 0);
    my @ret = <$tf2>;
    unlink0($tf1, $tn1);
    unlink0($tf2, $tn2);
    return @ret;
}

sub import {
    my $pid = open2(*IN, *OUT, "unset ALTERNATE_EDITOR; emacsclient -s org-ikiwiki-compiler --eval nil");
    waitpid($pid, 0);
    if ($? != 0) {
	$pid = open2(*IN, *OUT, "emacs --daemon --eval \"(progn (require 'ikiwiki-org-plugin) (setq server-name \\\"org-ikiwiki-compiler\\\") (server-start))\"");
    waitpid($pid, 0);
	if ($? != 0) {
	    print STDERR "Failed to start emacs. Will not continue with new_org setup.\n";
	    return;
	}
    }
    hook(type => "htmlize", id => "org", call => \&htmlize, first => 1);
    hook(type => "linkify", id => "org", call => \&linkify, first => 1);
    hook(type => "scan", id => "org", call => \&scan);
}

sub linkify(@) {
    my %params = @_;
    my $page_file_name = $pagesources{$params{page}};
    if ($page_file_name =~ $org_file_regexp) {
	my $hash_adds = "";
	my @ls = @{$links{$params{page}}};
	foreach my $link (@ls) {
	    my $bp = bestlink($params{page}, $link);
	    if ($bp) {
		$hash_adds .= qq/ (puthash "$link" "$bp" page-hash)/;
	    }
	}
	my $page_hash = "(let ((page-hash (make-hash-table :test 'equal))) $hash_adds page-hash)";
	return $params{content} = join('', run_func_in_emacs("linkify", $params{content}, "\"$params{destpage}\" $page_hash"));
    }
    return $params{content}
}

sub scan(@) {
    my %params = @_;
    my $page_file_name = $pagesources{$params{page}};
    if ($page_file_name =~ $org_file_regexp) {
	my @lines = run_func_in_emacs("scan", $params{content});
	foreach my $line (@lines) {
	    chomp $line;
	    if ($line =~ /#\+TITLE: (.*)$/) {
		$pagestate{$params{page}}{meta}{title} = $1;
	    } elsif ($line =~ /#\+AUTHOR: (.*)$/) {
		$pagestate{$params{page}}{meta}{author} = $1;
	    } else {
		add_link($params{page}, $line);
	    }
	}
    }
}

sub htmlize(@) {
    my %params = @_;
    return join('', run_func_in_emacs("htmlize", $params{content}));
}
