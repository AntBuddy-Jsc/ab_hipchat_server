/* remove an old primary key (userjid, resource, object_jid)*/
ALTER TABLE user_active_room DROP CONSTRAINT us_object;

/* add a new primary key (id)*/
ALTER TABLE user_active_room ADD COLUMN id BIGSERIAL PRIMARY KEY;

/* remove all indexs related to column 'resource'*/
DROP INDEX user_resource_status;

/* remove a column 'resource'*/
/*ALTER TABLE user_active_room DROP COLUMN resource;*/

/* remove a NOT NULL in column 'resource'*/
ALTER TABLE user_active_room ALTER COLUMN resource DROP NOT NULL;

/* add new column 'server_name'*/
ALTER TABLE user_active_room ADD server_name varying(100);

/* need to update the data first before adding the not-null constraint*/
UPDATE user_active_room SET server_name = 'antbuddy.com' where object_jid like '%conference.antbuddy.com';
UPDATE user_active_room SET server_name = 'antbuddy.com' where object_jid like '%@antbuddy.com';
UPDATE user_active_room SET server_name = 'instant.antbuddy.com' where object_jid like '%conference.instant.antbuddy.com';
UPDATE user_active_room SET server_name = 'instant.antbuddy.com' where object_jid like '%@instant.antbuddy.com';
UPDATE user_active_room SET server_name = 'kite.antbuddy.com' where object_jid like '%conference.kite.antbuddy.com';
UPDATE user_active_room SET server_name = 'kite.antbuddy.com' where object_jid like '%@kite.antbuddy.com';
UPDATE user_active_room SET server_name = 'antbuddy.com' where coalesce(server_name, '') = '';

/* add NOT NULL constraint to column 'server_name'*/
ALTER TABLE user_active_room ALTER COLUMN server_name SET NOT NULL;
ALTER TABLE user_active_room ADD PRIMARY KEY (id);
