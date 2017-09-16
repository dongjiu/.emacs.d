drop table file_tag;
drop table tag_parent;
drop table tag;
drop table file_chunk;
drop table [file];

create table [file] (
    file_id varchar(50) not null,
    file_name varchar(50) not null,
    last_modified_time int not null,
    primary key (file_id)
);

create table file_chunk (
    file_id varchar(50) not null,
    chunk_number int not null,
    chunk varbinary(max) not null,
    primary key (file_id, chunk_number),
    constraint File_ConsistsOf_Chunk
        foreign key (file_id)
        references [file] (file_id)
);

select * from [file];
truncate table [file_chunk];
truncate table [file];
select * from [file_chunk];

create table tag (
    tag_code char(5) not null,
    tag_name varchar(30) not null,
    primary key (tag_code)
);

create table tag_parent (
    tag_code char(5) not null,
    parent_tag_code char(5) not null,
    primary key (tag_code, parent_tag_code),
    constraint Tag_MayHave_Parent
        foreign key (tag_code)
        references tag (tag_code),
    constraint Tag_MayBeParentOf_Tag
        foreign key (parent_tag_code)
        references tag (tag_code)
);

create table file_tag (
    file_id varchar(50) not null,
    tag_code char(5) not null,
    primary key (file_id, tag_code),
    constraint File_IsAttched_Tag
        foreign key (file_id)
        references [file] (file_id),
    constraint Tag_Classifies_File
        foreign key (tag_code)
        references tag (tag_code)
);

select * from tag;
select * from file_tag;
