-define(NS_SHOWDME_GROUP_SUBSCRIBE, <<"showdme:group:subscribe">>).

%% Rooms per user
-record(user_room, {us, room, affiliation}).