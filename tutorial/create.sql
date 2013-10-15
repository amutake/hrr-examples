/* create.sql */

create schema TUTORIAL;

create table TUTORIAL.item (
  item_id integer not null,
  item_name varchar(255) not null,
  price integer not null,
  primary key (item_id)
);

insert into TUTORIAL.item (item_id, item_name, price) values (1, 'orange', 80);
insert into TUTORIAL.item (item_id, item_name, price) values (2, 'apple', 120);
