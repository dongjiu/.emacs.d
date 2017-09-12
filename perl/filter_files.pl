#!/usr/bin/perl
use v5.14;

$| = 1;

my $regex = shift;
while (<STDIN>) {
  chomp;
  my $file = $_;
  say $file if not $regex or file_contains($file, $regex);
}

sub file_contains {
  my ($file, $regex) = @_;

  return 0 unless -f $file;

  open my $fh, '<', $file or return report_error($file, $!);

  my $match = 0;
  while (<$fh>) {
    if(/$regex/) {
      $match = 1;
      last;
    }
  }
  close $fh;

  return $match;
}

sub report_error {
  my ($file, $error) = @_;
  say "failed to open file: $file. $error";
  return 0;
}
