#!/bin/sh

dbname="antbuddy"
username="antbuddy"
psql $dbname $username <<EOF
ALTER TABLE user_active_room DROP CONSTRAINT us_object;
ALTER TABLE user_active_room ADD COLUMN id BIGSERIAL PRIMARY KEY;
DROP INDEX user_resource_status;
DROP INDEX user_object;
ALTER TABLE user_active_room ALTER COLUMN resource DROP NOT NULL;
ALTER TABLE user_active_room ADD server_name character varying(100);
UPDATE user_active_room SET server_name = 'antbuddy.com' where object_jid like '%conference.antbuddy.com';
UPDATE user_active_room SET server_name = 'antbuddy.com' where object_jid like '%@antbuddy.com';
UPDATE user_active_room SET server_name = 'anonymous.com' where object_jid like '%conference.anonymous.com';
UPDATE user_active_room SET server_name = 'anonymous.com' where object_jid like '%@anonymous.com';
UPDATE user_active_room SET server_name = 'kite.antbuddy.com' where object_jid like '%conference.kite.antbuddy.com';
UPDATE user_active_room SET server_name = 'kite.antbuddy.com' where object_jid like '%@kite.antbuddy.com';
UPDATE user_active_room SET server_name = 'antbuddy.com' where coalesce(server_name, '') = '';
ALTER TABLE user_active_room ALTER COLUMN server_name SET NOT NULL;
CREATE INDEX idx_us_object ON user_active_room (userjid, server_name, object_jid);
EOF

