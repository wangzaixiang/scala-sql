create database scalasql;

use scalasql;

drop table if exists users;

create table users(
  name varchar(20) not null,
  email varchar(32),
  age int
);

insert into users values('user1', 'wangzaixiang@gmail.com', 40);
insert into users values('user2', 'rainbo.liu@gmail.com', 38);
