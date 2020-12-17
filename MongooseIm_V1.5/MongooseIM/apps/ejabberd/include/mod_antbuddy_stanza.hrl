%% message with file
-define(FILE_THUMBNAIL_URL,	<<"thumbnailUrl">>).
-define(FILE_THUMBNAIL_WIDTH, 	<<"thumbnailWidth">>).
-define(FILE_THUMBNAIL_HEIGHT,	<<"thumbnailHeight">>).
-define(MIMETYPE,	<<"mimeType">>).
-define(FILE_URL,	<<"fileUrl">>).
-define(FILE_SIZE,	<<"size">>).
-define(FILE_NAME,	<<"name">>).

%% message with oembed
-define(PROVIDER_URL,	<<"provider_url">>).
-define(PROVIDER_NAME,	<<"provider_name">>).
-define(TITLE,	<<"title">>).
-define(AUTHOR_NAME,	<<"author_name">>).
-define(AUTHOR_URL,	<<"author_url">>).
-define(OEMBED_THUMBNAIL_URL,	<<"thumbnail_url">>).
-define(OEMBED_THUMBNAIL_WIDTH,	<<"thumbnail_width">>).
-define(OEMBED_THUMBNAIL_HEIGHT,	<<"thumbnail_height">>).
-define(OEMBED_VERSION,	<<"version">>).
-define(OEMBED_URL,	<<"url">>).
-define(DESCRIPTION,	<<"description">>).
-define(OEMBED_TYPE,	<<"type">>).
-define(OEMBED_MEAN_ALPHA,	<<"mean_alpha">>).
-define(OEMBED_HTML,	<<"html">>).

-define(FROM_KEY, fromKey).
-define(RECEIVER_KEY, receiverKey).
-define(SENDER_KEY, senderKey).

-record(user_active, {usr, object, us_object}).

%% message event
-define(EVENT, <<"event">>).

-define(KITE_EVENTS, [<<"kite-start">>, <<"kite-close">>]).