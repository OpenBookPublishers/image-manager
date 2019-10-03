
insert into copyright (licence_status, allowed) values ('To Be Determined', 'f');
insert into copyright (licence_status, allowed) values ('Public Domain', 't');
insert into copyright (licence_status, allowed) values ('Proprietary', 'f');
insert into copyright (licence_status, allowed) values ('CC-BY', 't');
insert into copyright (licence_status, allowed) values ('CC-BY-SA', 't');
insert into copyright (licence_status, allowed) values ('Fair use', 't');

insert into detail (detail_type) values ('Provenance');
insert into detail (detail_type) values ('URL');
insert into detail (detail_type) values ('Original artist');
insert into detail (detail_type) values ('Original year');
insert into detail (detail_type) values ('Original medium');
insert into detail (detail_type) values ('Original title');
insert into detail (detail_type) values ('Original size');

-- obviously for the multi-book version we can't have the same uuid,
-- so this will have to go
insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('cd0f89f1-bbcb-4b46-a104-6083b0556dd4', 'Chapter 1', '1', 10);

insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('0a9c647a-a2a3-4ee0-ade2-abfc1a029642', 'Chapter 2', '2', 20);

insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('32d33dc5-5aa2-4be3-ba0b-cf24edc354bc', 'Chapter 3', '3', 30);

insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('9c4c9fdf-018a-4f05-8a04-b374cfd2d3d2', 'Introduction', '0', 800);

insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('c3a9d135-67cf-46cf-8314-f266b67abfdb', 'Cover image', '', 900);

insert into chapter (chapter_uuid, chapter_name, figure_id, ordinal)
  values ('eeff18e9-8b15-41c5-88ff-d67bee98b7da', 'Unassigned images', '', 1000);

