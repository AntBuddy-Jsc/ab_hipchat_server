-include("ejabberd.hrl").
% -record(pool, {id
%             , host
%             , port
%             , database
%             , size=1
%             , conn_pids=queue:new()
%             , req_id=1}).
-include_lib("emongo/include/emongo.hrl").

-define(MONGO_DB, <<"mongodb">>).
-define(MONGO_COLL, <<"chatstore1">>).
-define(MONGO_CALL_COLL, <<"callhistory">>).
-define(MONGO_BOT_COLL, <<"bothistory">>).
-define(MONGO_DB_READ_MSG, <<"messages">>).
-define(MONGO_DB_ORGANIZATIONS, <<"organizations">>).
-define(MONGO_DB_SOCIAL_BOT, <<"socialbot">>).
-define(EMONGO_CONNECTION_TABLE, emongo_pool_connections).


