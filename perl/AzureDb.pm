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
  while (my $row = $sth->fetchrow_hashref) {
    print $fh decode_base64($row->{chunk_string});
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

  my $sth = $dbh->prepare("select file_id, file_name, last_modified_time from [file]");
  $sth->execute;

  while (my ($file_id, $file_name, $last_modified_time) = $sth->fetchrow_array) {
    say '-' x 80;
    say "file_id:\t$file_id";
    say "file_name:\t$file_name";
    my $last_modified = time2str('%Y-%m-%d %H:%M:%S', $last_modified_time);
    say "last_modified_time:\t$last_modified";
    say '-' x 80;
  }
}

1;
