use v5.14;
use AzureDb;
use Getopt::Long;

my $server = "donzhu.database.windows.net";
my $user = "donzhu";
my $database = "donzhu_db";

my $action = shift;
die "No action specified." unless $action;

given ($action) {
  when ('upload') {
    my ($file, $directory, $upload_name, $password);
    GetOptions("password=s" => \$password,
               "file=s" => \$file,
               "directory=s" => \$directory,
               "upload_name=s" => \$upload_name);
    die "--file not specified." unless $file;
    die "--directory not specified." unless $directory;
    die "--upload_name not specified." unless $upload_name;
    die "--password not specified." unless $password;

    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    my $file_content = AzureDb::encode_file_to_base64($file);
    AzureDb::upload_file($dbh, $directory, $upload_name, $file_content);
  }
  when ('pull') {
    my ($directory, $file_name, $output_file, $password);
    GetOptions("password=s" => \$password,
               "directory=s" => \$directory,
               "output_file=s" => \$output_file,
               "file_name=s" => \$file_name);
    die "--directory not specified." unless $directory;
    die "--file_name not specified." unless $file_name;
    die "--output_file not specified." unless $output_file;
    die "--password not specified." unless $password;

    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    my $file_content = AzureDb::get_file_content($dbh, $directory, $file_name);
    AzureDb::convert_base64_to_file($file_content, $output_file);
  }
  when ('delete') {
    my ($directory, $file_name, $password);
    GetOptions("password=s" => \$password,
               "directory=s" => \$directory,
               "file_name=s" => \$file_name);
    die "--directory not specified." unless $directory;
    die "--file_name not specified." unless $file_name;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::delete_file($dbh, $directory, $file_name);
  }
  default {
    die "Unknown action $action";
  }
}
