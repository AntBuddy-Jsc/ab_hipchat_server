-module(mod_migrate).
-compile(export_all).

-record(user_room, {us, room, affiliation}).

migrate_data() ->
    %% get all data of muc_room
    Keys = mnesia:dirty_all_keys(muc_room),
    Data = lists:map(fun(Key) ->
                            mnesia:dirty_read({muc_room, Key})
                      end, Keys),

    %% get info of us (user, server), room, affiliation
    lists:foreach(fun(RoomInfo) ->
        [{_, RoomName, Opts}] = RoomInfo,
            case lists:keyfind(affiliations, 1, Opts) of
                false ->
                    ok;
                {_, Affiliations} ->
                    lists:foreach(fun({{U, S, _R}, Affiliation}) ->
                                %% Store data
                                case is_atom(Affiliation) of
                                    true ->
                                        Rec = #user_room{us = {U, S}, room = RoomName, affiliation = Affiliation},
                                        F = fun() -> mnesia:write(Rec) end,
                                        mnesia:transaction(F);
                                    false ->
                                        case is_tuple(Affiliation) of
                                            true ->
                                                {Aff, _} = Affiliation,
                                                Rec = #user_room{us = {U, S}, room = RoomName, affiliation = Aff},
                                                F = fun() -> mnesia:write(Rec) end,
                                                 mnesia:transaction(F);
                                            false -> ok
                                        end
                                end
                         end, Affiliations)
            end
        end, Data).

