package AzureDb;

use v5.14;
use DBI;
use MIME::Base64;
use Data::Dumper;

#my $server = "donzhu.database.windows.net";

sub sql_server_dbh {
  my ($server, $user, $password, $database) = @_;
  DBI->connect("dbi:ODBC:Driver={SQL Server};Server=$server;Database=$database;UID=$user;PWD=$password");
}

sub get_file_content {
  my ($dbh, $directory, $file_name) = @_;

  my $sth = $dbh->prepare("select encrypted_content from encrypted_file where directory = ? and file_name = ?");
  $sth->bind_param(1, $directory);
  $sth->bind_param(2, $file_name);

  $sth->{'LongReadLen'} = 1024 * 1024;
  $sth->execute;
  while (my $row = $sth->fetchrow_hashref) {
    return $row->{encrypted_content};
  }
}

sub convert_base64_to_file {
  my ($base64_string, $file) = @_;

  open my $fh, '>', $file;
  binmode $fh;
  print $fh decode_base64($base64_string);
}

sub encode_file_to_base64 {
  my ($file) = @_;

  open my $fh, '<', $file;
  binmode $fh;
  my $raw_string = do { local $/ = undef; <$fh>; };
  encode_base64($raw_string, '');
}

sub upload_file {
  my ($dbh, $directory, $file_name, $file_content) = @_;

  my $sql = "insert into encrypted_file (directory, file_name, encrypted_content, last_modified_time) values ('$directory', '$file_name', '$file_content', getdate())";
  $dbh->do($sql);
}

sub delete_file {
  my ($dbh, $directory, $file_name) = @_;

  my $sth = $dbh->prepare("delete from encrypted_file where directory = ? and file_name = ?");
  $sth->bind_param(1, $directory);
  $sth->bind_param(2, $file_name);
  $sth->execute;
}

1;
