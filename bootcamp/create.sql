/*
  Bootcamp Definition
  % createdb testdb
  % psql -f create.sql testdb
*/

/* begin table creation */

create schema BOOTCAMP;

create table BOOTCAMP.item_mst_table (
  item_id integer not null,
  item_name varchar(255) not null,
  tag_price integer not null,
  primary key (item_id)
);

create table BOOTCAMP.point_management_table (
  member_id integer not null,
  managed_point integer not null,
  issue_date date not null,
  primary key (member_id)
);

create table BOOTCAMP.pos_table (
  pos_id integer not null,
  member_id integer not null,
  created_date date not null,
  primary key (pos_id)
);

create table BOOTCAMP.error_pos_table (
  pos_id integer not null,
  foreign key (pos_id) references BOOTCAMP.pos_table (pos_id)
);

create table BOOTCAMP.pos_detail_table (
  id integer not null,
  pos_id integer not null,
  item_id integer not null,
  amount integer not null,
  sales_price integer not null,
  offset_price integer not null,
  primary key (id),
  foreign key (pos_id) references BOOTCAMP.pos_table (pos_id),
  foreign key (item_id) references BOOTCAMP.item_mst_table (item_id)
);

create table BOOTCAMP.point_systems_table (
  id integer not null,
  member_id integer not null,
  point_system integer not null,
  primary key (id)
);

/* end table creation */
