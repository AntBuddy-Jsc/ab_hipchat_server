-include("jlib.hrl").
-include("ejabberd.hrl").

-define(CONTENT_TYPE_JSON, "application/json").
-define(CONTENT_TYPE_FORM_URLENCODED, "application/x-www-form-urlencoded").
-define(MAX_LENGTH_CONTENT, 128).
-define(TIMEOUT, 30000).
-define(RETRY, 3).

-define(HTTP_GET, <<"GET">>).
-define(HTTP_PUT, <<"PUT">>).
-define(HTTP_POST, <<"POST">>).
-define(HTTP_DELETE, <<"DELETE">>).

-define(ALLOWED_METHODS, [?HTTP_GET
                        , ?HTTP_POST
                        , ?HTTP_PUT
                        , ?HTTP_DELETE]).

-define(TOKEN_URL, "https://login.microsoftonline.com/common/oauth2/v2.0/token").
-define(SCOPE, "https://api.botframework.com/.default").
-define(GRANT_TYPE, "client_credentials").
-define(CONVERSION_URI, "/v3/conversations/").
-define(OPEN_ID_URL, "https://api.aps.skype.com/v1/.well-known/openidconfiguration").
-define(FACEBOOK_API, "https://graph.facebook.com/v2.8/").
-define(FACEBOOK_FIELDS, "?fields=first_name,last_name,locale,timezone,gender,profile_pic&access_token=").
-define(JOIN_SKYPE, "https://join.skype.com/bot/").
-define(API_FILE, "https://kite.ant.chat/api/skype-attachment/upload").
-define(SEND_VIBER_MSG, "https://chatapi.viber.com/pa/send_message").
-define(GET_USER_DETAIL, "https://chatapi.viber.com/pa/get_user_details").

-record(attachment, {   type = <<>>,
                        url  = <<>>
                    }).

-record(message, {  id   = <<>>,
                    text = <<>>,
                    attachments = [],
                    authorization = <<>> 
                  }).

-record(reply, {   text = <<>>,
                   attachments = [] 
                  }).
-type reply() :: #reply{}.

-record(bot_access_token, {
                token_type = <<>>,
                access_token = <<>>,
                expires_in,
                create_token_at = 0
    }).
-type bot_access_token() :: #bot_access_token{}.

-record(state, {id = <<>>,                      % conversion id
                user_account        :: list(),  % [{<<"id">>,<<"125233317884709">>},{<<"name">>,<<"Nguyen Lam">>}]
                bot_account         :: list(),  % [{<<"id">>,<<"1616280948699298">>},{<<"name">>,<<"Antbuddy">>}]
                conversation        :: list(),  % [{<<"isGroup">>,false},{<<"id">>,<<"125233317884709-1616280948699298">>}]
                channel_id = <<>>,              % <<"facebook">>, <<"skype">>
                room_jid            :: ejabberd:jid(),
                room_pid            :: pid(),
                room_pin,
                jid                :: ejabberd:jid(),
                token = <<>>,                   % token gets from web api
                joined_room = false   :: boolean(),
                message_queue = queue:new(),
                app_id = <<>>,                  %% Bot App ID (Bot handle)
                ms_app_id = <<>>,               %% MicrosoftAppID
                ms_app_secret = <<>>,           %% MicrosoftAppSecret
                fb_token = <<>>,                %% Facebook token
                service_url = <<>>,             %% <<"https://facebook.botframework.com">>
                web_base = <<>>,
                web_api = <<>>, 
                sid,
                jwks_uri = <<>>,
                access_token :: bot_access_token()        %% access_token to request rest API of MS Bot Framework 
               }).

%% The conversation info
-record(social_session, { id = <<>>,  
                        user_account :: list() ,
                        bot_account :: list(),
                        conversation :: list(),
                        channel_id = <<>>,
                        service_url = <<>>
                    }).

-record(muc_online_room, {name_host,
                          pid       :: pid()
                         }).

-record(bot_admin, {email, password}).

-record(process_social_c2s, {proc_name, pid}).
