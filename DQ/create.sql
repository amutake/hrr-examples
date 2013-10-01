/* create.sql */

create schema DQ;

create table DQ.family (
  family_id integer not null,
  family_name varchar(255) not null,
  primary key (family_id)
);

create table DQ.monster (
  monster_id integer not null,
  monster_name varchar(255) not null,
  level integer not null,
  family_id integer,
  primary key (monster_id),
  foreign key (family_id) references DQ.family (family_id)
);

insert into DQ.family (family_id, family_name) values (1, 'slime');
insert into DQ.family (family_id, family_name) values (2, 'dragon');

insert into DQ.monster (monster_id, monster_name, level, family_id) values (1, 'slime', 4, 1);
insert into DQ.monster (monster_id, monster_name, level, family_id) values (2, 'buchi-slime', 5, 1);
insert into DQ.monster (monster_id, monster_name, level, family_id) values (3, 'metal-king', 30, 1);
insert into DQ.monster (monster_id, monster_name, level, family_id) values (4, 'dragon', 10, 2);
insert into DQ.monster (monster_id, monster_name, level, family_id) values (5, 'great-dragon', 20, 2);
insert into DQ.monster (monster_id, monster_name, level, family_id) values (6, 'shinryu', 40, 2);
insert into DQ.monster (monster_id, monster_name, level) values (7, 'ryuoh', 50);
insert into DQ.monster (monster_id, monster_name, level) values (8, 'death-pisaro', 50);
