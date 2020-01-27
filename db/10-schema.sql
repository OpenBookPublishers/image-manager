
create table copyright (
       licence_status varchar(30) primary key,
       allowed boolean not null
);

create table chapter (
       chapter_uuid char(36) primary key,
       chapter_name text not null,
       figure_id varchar(20) not null,
       ordinal integer not null
);

create table image (
       hash varchar(50) primary key,
       image_name text not null,
       format varchar(10) not null,
       resolution varchar(20) not null,
       res_category varchar(10) not null,
       chapter_uuid char(36) not null references chapter(chapter_uuid),
       rank integer not null,
       licence_status varchar(30) not null references copyright(licence_status),
       placeholder boolean not null,
       unique (chapter_uuid, rank)
);

create table detail (
       detail_type varchar(30) primary key,
       detail_name varchar(30) not null
);

create table image_details (
       hash varchar(50) references image(hash),
       detail_type varchar(30) not null references detail(detail_type),
       detail_value text not null,
       unique (hash, detail_type)
);
       
create view image_chapter as
select * from image
  left join chapter using (chapter_uuid);
