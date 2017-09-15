package AzureDb;

use v5.14;
use DBI;
use MIME::Base64;
use Date::Format;
use Data::Dumper;

use constant {
              CHUNK_SIZE => 1024 * 1024,
              LONG_READ_LEN => 1024 * 1024 * 1.5,
             };

sub sql_server_dbh {
  my ($server, $user, $password, $database) = @_;
  DBI->connect("dbi:ODBC:Driver={SQL Server};Server=$server;Database=$database;UID=$user;PWD=$password");
}

sub upload_file {
  my ($dbh, $file, $file_id, $file_name) = @_;

  my $last_modified_time = time();
  my $sth = $dbh->prepare("insert into [file] (file_id, file_name, last_modified_time) values (?, ?, ?)");
  $sth->bind_param(1, $file_id);
  $sth->bind_param(2, $file_name);
  $sth->bind_param(3, $last_modified_time);
  $sth->execute;

  upload_file_chunks($dbh, $file, $file_id);
}

sub upload_file_chunks {
  my ($dbh, $file, $file_id) = @_;

  open my $fh, '<', $file or die "Cannot open file $file";
  binmode $fh;
  my $chunk_number = 0;
  my $buffer;
  while (read($fh, $buffer, CHUNK_SIZE)) {
    ++$chunk_number;
    my $chunk_string = encode_base64($buffer, '');
    upload_file_chunk($dbh, $file_id, $chunk_number, $chunk_string);
  }
}

sub upload_file_chunk {
  my ($dbh, $file_id, $chunk_number, $chunk_string) = @_;

  my $sth = $dbh->prepare("insert into file_chunk (file_id, chunk_number, chunk_string) values (?, ?, ?)");
  $sth->bind_param(1, $file_id);
  $sth->bind_param(2, $chunk_number);
  $sth->bind_param(3, $chunk_string);
  $sth->execute;
}

sub pull_file {
  my ($dbh, $file_id, $output_file) = @_;

  die "File $output_file already exists." if -f $output_file;

  my $sql = "select chunk_string from file_chunk where file_id = ? order by chunk_number";
  my $sth = $dbh->prepare($sql);
  $sth->bind_param(1, $file_id);
  $sth->{'LongReadLen'} = LONG_READ_LEN;
  $sth->execute;

  open my $fh, '>>', $output_file;
  binmode $fh;
  my $found = 0;
  while (my $row = $sth->fetchrow_hashref) {
    $found = 1;
    print $fh decode_base64($row->{chunk_string});
  }
  unless ($found) {
    close $fh;
    unlink $output_file;
    die "File $file_id not found.";
  }
}

sub delete_file {
  my ($dbh, $file_id) = @_;

  delete_file_chunks($dbh, $file_id);

  my $sth = $dbh->prepare("delete from [file] where file_id = ?");
  $sth->bind_param(1, $file_id);
  $sth->execute;
}

sub delete_file_chunks {
  my ($dbh, $file_id) = @_;

  my $sth = $dbh->prepare("delete from file_chunk where file_id = ?");
  $sth->bind_param(1, $file_id);
  $sth->execute;
}

sub list_files {
  my ($dbh) = @_;

  my $file_tags = list_file_tags($dbh);

  my $sth = $dbh->prepare("select file_id, file_name, last_modified_time from [file]");
  $sth->execute;

  while (my ($file_id, $file_name, $last_modified_time) = $sth->fetchrow_array) {
    say '-' x 80;
    say "file_id:\t$file_id";
    say "file_name:\t$file_name";
    my $last_modified = time2str('%Y-%m-%d %H:%M:%S', $last_modified_time);
    say "last_modified_time:\t$last_modified";
    say "tags:\t" . join(', ', map { $_->{tag_name} } @{ $file_tags->{$file_id} });
    say '-' x 80;
  }
}

sub create_tag {
  my ($dbh, $tag_code, $tag_name) = @_;

  my $sth = $dbh->prepare("insert into tag (tag_code, tag_name) values (?, ?)");
  $sth->bind_param(1, $tag_code);
  $sth->bind_param(2, $tag_name);
  $sth->execute;
}

sub change_tag_name {
  my ($dbh, $tag_code, $tag_name) = @_;

  my $sth = $dbh->prepare("update tag set tag_name = ? where tag_code = ?");
  $sth->bind_param(1, $tag_name);
  $sth->bind_param(2, $tag_code);
  $sth->execute;
}

sub list_tags {
  my ($dbh) = @_;

  my $sth = $dbh->prepare("select tag_code, tag_name from tag");
  $sth->execute;
  while (my ($tag_code, $tag_name) = $sth->fetchrow_array) {
    say '-' x 80;
    say "tag_code: $tag_code";
    say "tag_name: $tag_name";
    say '-' x 80;
  }
}

sub add_file_tag {
  my ($dbh, $file_id, $tag_code) = @_;

  my $sth = $dbh->prepare("insert into file_tag (file_id, tag_code) values (?, ?)");
  $sth->bind_param(1, $file_id);
  $sth->bind_param(2, $tag_code);
  $sth->execute;
}

sub list_file_tags {
  my ($dbh) = @_;

  my $sth = $dbh->prepare("select file_tag.file_id, tag.tag_code, tag.tag_name
 from file_tag join tag on tag.tag_code = file_tag.tag_code");
  $sth->execute;

  my %file_tags;
  while (my ($file_id, $tag_code, $tag_name) = $sth->fetchrow_array) {
    push @{ $file_tags{$file_id} }, { tag_code => $tag_code, tag_name => $tag_name };
  }
  \%file_tags;
}

sub remove_file_tag {
  my ($dbh, $file_id, $tag_code) = @_;
  my $sth = $dbh->prepare("delete from file_tag where file_id = ? and tag_code = ?");
  $sth->bind_param(1, $file_id);
  $sth->bind_param(2, $tag_code);
  $sth->execute;
}

1;
