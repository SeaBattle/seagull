%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("tihon").

%% Database conf vars
-define(DATABASE_HOSTS_CONF, <<"database/hosts">>).
-define(DATABASE_DB_CONF, <<"database/db">>).
-define(DATABASE_LOGIN_CONF, <<"database/login">>).
-define(DATABASE_PASS_CONF, <<"database/pass">>).
-define(DATABASE_SIZE_CONF, <<"database/pool/size">>).
-define(DATABASE_OVERFLOW_CONF, <<"database/pool/overflow">>).
-define(DATABASE_OVERFLOW_TTL_CONF, <<"database/pool/overflow_ttl">>).
-define(DATABASE_OVERFLOW_CHECK_CONF, <<"database/pool/overflow_check">>).

%% Email conf vars
-define(MAILGUN_DOMAIN_CONF, <<"email/domain">>).
-define(MAILGUN_API_URL_CONF, <<"email/api_url">>).
-define(MAILGUN_API_KEY_CONF, <<"email/api_key">>).

%% Auth conf vars
-define(SALT_LEN_CONF, <<"auth/salt_len">>).
-define(SECRET_LEN_CONF, <<"auth/secret_len">>).
-define(SECRET_ITER_CONF, <<"auth/secret_iter">>).

%% UserService conf
-define(USER_SERVICE_HTTP_PORT_CONF, <<"user_service/http/port">>).
-define(USER_SERVICE_HTTP_ACCEPTORS_CONF, <<"user_service/http/acceptors">>).