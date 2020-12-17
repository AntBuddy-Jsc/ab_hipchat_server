%% @author bokner
%% @doc @todo Add description to showdme.


-module(showdme).

%% ====================================================================
%% API functions
%% ====================================================================
-export([auth/3]).

-compile(export_all).

-define(SECRET_ENV_VAR, "SECRET").
-define(DEFAULT_EXPIRATION_THRESHOLD, 60000).

%% TODO :  check if the unix time exceeded expiration threshold
auth(User, Server, Password) ->
  auth(User, Server, Password, os:getenv(?SECRET_ENV_VAR)).

auth(User, Server, Password, Secret) ->  
  %% Get the shared secret (the value of SECRET env variable).
  %% Throw away first 8 bytes (by agreement with node.js, they prepend the token with 8 random symbols)
  <<_:8/binary, DecryptedToken/binary>> = list_to_binary(decrypt(crypto:md5(Secret), hexbin_to_bin(Password))),
  [UnixTimeBin, DecryptedUser] = 
	binary:split(
				   DecryptedToken,
				   <<"-">>
				),
  DecryptedUser == User 
	andalso not is_token_expired(binary_to_integer(UnixTimeBin),
								 ejabberd_config:get_local_option({auth_expiration, Server})).
  
%% ====================================================================
%% Internal functions
%% ====================================================================
bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

hexbin_to_bin(S) ->
  hexbin_to_bin(S, <<>>).
hexbin_to_bin(<<>>, Acc) ->
  Acc;
hexbin_to_bin(<<X:8,Y:8, T/binary>>, Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexbin_to_bin(T, <<Acc/binary, V>>).



%% CBC decrypt
decrypt(Key, Encoded) ->
    <<Iv:8/binary, Rest/binary>> = Encoded,
    Padded = pad_to(8, Encoded),
    Decrypted = crypto:block_decrypt(blowfish_cbc, Key, crypto:rand_bytes(8), Padded),
	  %%crypto:blowfish_cbc_decrypt(Key, Iv, Padded),
    strip(Decrypted).


%% ECB decrypt
bf_decrypt(BlowfishKey, Text) ->
    Decrypted = bf_decrypt1(BlowfishKey, Text, []),
    %% This is a reverse list of binaries
    %% First element may contain trailing zeros
    [F | Rest] = Decrypted,
    lists:flatten(lists:foldl(fun(B, Acc) ->
                                      [binary_to_list(B) | Acc]
                              end, binary_to_list(cut_zeros(F)), Rest)).

bf_decrypt1(_BlowfishKey, <<>>, Acc) ->
    Acc;
bf_decrypt1(BlowfishKey, <<P:8/binary, Rest/binary>>, Acc) ->
    T = crypto:blowfish_ecb_decrypt(BlowfishKey, P),
    bf_decrypt1(BlowfishKey, Rest, [T | Acc]).

%%pad(Data) when length(Data) rem 8 =:= 0 ->
%%    Data;
%%pad(Data) when length(Data) rem 8 =/= 0 ->
%%    pad(lists:append(Data, [0])).

pad_to(Width, Binary) ->
     case (Width - size(Binary) rem Width) rem Width
       of 0 -> Binary
        ; N -> <<Binary/binary, 0:(N*8)>>
     end.

strip(Data) ->
    List = binary_to_list(Data),
    [X || X <- List, X > 7].

%% Cut trailing zeros in binary
cut_zeros(B) ->
    cut_zeros1(B, <<>>).

cut_zeros1(<<>>, Acc) ->
    Acc;
cut_zeros1(<<0, _Rest/binary>>, Acc) ->
    Acc;
cut_zeros1(<<H:1/binary, Rest/binary>>, Acc) ->
    cut_zeros1(Rest, <<Acc/binary, H:1/binary>>).


bf_encrypt(Key, Data) when is_binary(Data) >= 8 ->
	bf_encrypt(Key, Data, <<>>, size(Data)).

bf_encrypt(Key, <<Data:8/binary, Rest/binary>>, Acc, Size) ->
	Cipher = crypto:blowfish_ecb_encrypt(Key, Data),
	bf_encrypt(Key, Rest, <<Acc/binary, Cipher/binary>>, Size);
bf_encrypt(_Key, <<>>, Acc, Size) -> {cipher, Size, Acc};
bf_encrypt(Key, <<Data/binary>>, Acc, Size)->
	BitSize = ( 8 - (Size rem 8) ) * 8,
	Data1 = <<Data/binary, 0:BitSize/integer>>,
	Cipher = crypto:blowfish_ecb_encrypt(Key, Data1),
	bf_encrypt(Key, <<>>, <<Acc/binary, Cipher/binary>>, Size).


is_token_expired(TokenUnixTime, undefined) ->
  is_token_expired(TokenUnixTime, ?DEFAULT_EXPIRATION_THRESHOLD);

is_token_expired(_TokenUnixTime, infinity) ->
  false;

is_token_expired(TokenUnixTime, Threshold) when is_integer(Threshold) ->
  {Mega, Secs, _} = os:timestamp(),
  TimestampNow = Mega*1000000 + Secs,
  TimestampNow - TokenUnixTime > Threshold/1000.