-module(rs_credentials, [Username, Password]).

-export([username/0, password/0, all/0]).

username() ->
    Username.

password() ->
    Password.

all() ->
    [{"username", Username}, {"password", Password}].
