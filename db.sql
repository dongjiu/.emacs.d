create table encrypted_file (
    directory varchar(128),
    file_name varchar(30),
    encrypted_content varchar(max),
    last_modified_time datetime2,
    primary key (directory, file_name)
);

insert into encrypted_file (directory, file_name, encrypted_content, last_modified_time)
values ('/', 'test.txt', 'hello', getdate());

select * from encrypted_file;

select encrypted_content from encrypted_file
where directory = 'test' and file_name = 'README.md'
