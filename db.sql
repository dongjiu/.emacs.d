drop table file_chunk;
drop table [file];

create table [file] (
    file_id varchar(50),
    file_name varchar(50),
    last_modified_time int,
    primary key (file_id)
);

create table file_chunk (
    file_id varchar(50),
    chunk_number int,
    chunk_string varchar(max),
    primary key (file_id, chunk_number),
    constraint File_ConsistsOf_Chunk
        foreign key (file_id)
        references [file] (file_id)
);

select * from [file];

select * from [file_chunk];
