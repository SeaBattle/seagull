%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("tihon").

%% Database conf vars
-define(DATABASE_HOSTS, <<"database/hosts">>).
-define(DATABASE_DB, <<"database/db">>).
-define(DATABASE_LOGIN, <<"database/login">>).
-define(DATABASE_PASS, <<"database/pass">>).
-define(DATABASE_SIZE, <<"database/pool/size">>).
-define(DATABASE_OVERFLOW, <<"database/pool/overflow">>).
-define(DATABASE_OVERFLOW_TTL, <<"database/pool/overflow_ttl">>).
-define(DATABASE_OVERFLOW_CHECK, <<"database/pool/overflow_check">>).

%% Email conf vars
-define(MAILGUN_DOMAIN, <<"email/domain">>).
-define(MAILGUN_API_URL, <<"email/api_url">>).
-define(MAILGUN_API_KEY, <<"email/api_key">>).

%% UserService conf
-define(USER_SERVICE_HTTP_PORT, <<"user_service/http/port">>).
-define(USER_SERVICE_HTTP_ACCEPTORS, <<"user_service/http/acceptors">>).