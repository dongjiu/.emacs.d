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
    my ($file, $file_id, $file_name, $password);
    GetOptions("password=s" => \$password,
               "file=s" => \$file,
               "file_id=s" => \$file_id,
               "file_name=s" => \$file_name);
    die "--file not specified." unless $file;
    die "--file_id not specified." unless $file_id;
    die "--file_name not specified." unless $file_name;
    die "--password not specified." unless $password;

    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::upload_file($dbh, $file, $file_id, $file_name);
  }
  when ('pull') {
    my ($file_id, $output_file, $password);
    GetOptions("password=s" => \$password,
               "file_id=s" => \$file_id,
               "output_file=s" => \$output_file);
    die "--file_id not specified." unless $file_id;
    die "--output_file not specified." unless $output_file;
    die "--password not specified." unless $password;

    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::pull_file($dbh, $file_id, $output_file);
  }
  when ('delete') {
    my ($file_id, $password);
    GetOptions("password=s" => \$password,
               "file_id=s" => \$file_id);
    die "--file_id not specified." unless $file_id;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::delete_file($dbh, $file_id);
  }
  when ('list') {
    my ($password);
    GetOptions("password=s" => \$password);
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::list_files($dbh);
  }
  when ('newtag') {
    my ($password, $tag_code, $tag_name);
    GetOptions("password=s" => \$password,
               "tag_code=s" => \$tag_code,
               "tag_name=s" => \$tag_name);
    die "--tag_code not specified." unless $tag_code;
    die "--tag_name not specified." unless $tag_name;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::create_tag($dbh, $tag_code, $tag_name);
  }
  when ('renametag') {
    my ($password, $tag_code, $tag_name);
    GetOptions("password=s" => \$password,
               "tag_code=s" => \$tag_code,
               "tag_name=s" => \$tag_name);
    die "--tag_code not specified." unless $tag_code;
    die "--tag_name not specified." unless $tag_name;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::change_tag_name($dbh, $tag_code, $tag_name);
  }
  when ('tags') {
    my ($password);
    GetOptions("password=s" => \$password);
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::list_tags($dbh);
  }
  when ('addfiletag') {
    my ($password, $file_id, $tag_code);
    GetOptions("password=s" => \$password,
               "file_id=s" => \$file_id,
               "tag_code=s" => \$tag_code);
    die "--file_id not specified." unless $file_id;
    die "--tag_code not specified." unless $tag_code;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::add_file_tag($dbh, $file_id, $tag_code);
  }
  when ('rmfiletag') {
    my ($password, $file_id, $tag_code);
    GetOptions("password=s" => \$password,
               "file_id=s" => \$file_id,
               "tag_code=s" => \$tag_code);
    die "--file_id not specified." unless $file_id;
    die "--tag_code not specified." unless $tag_code;
    die "--password not specified." unless $password;
    my $dbh = AzureDb::sql_server_dbh($server, $user, $password, $database);
    AzureDb::remove_file_tag($dbh, $file_id, $tag_code);
  }
  default {
    die "Unknown action $action";
  }
}
