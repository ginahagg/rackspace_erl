%% Copyright 2010 Gina Hagg 
%% Licensed under the Apache License, Version 2.0 (the "License"); 
%% you may not use this file except in compliance with the License. 
%% You may obtain a copy of the License at 
%%
%% http://www.apache.org/licenses/LICENSE-2.0 
%%
%% Unless required by applicable law or agreed to in writing, software 
%% distributed under the License is distributed on an "AS IS" BASIS, 
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
%% See the License for the specific language governing permissions and 
%% limitations under the License.

-module(rs_keys, [Identifier, SecretKey]).

-export([sign_request/1, identifier/0]).

identifier() ->
    Identifier.

sign_request(Req) ->
    crypto:start(),
    base64:encode_to_string((crypto:sha_mac(SecretKey, Req))).
