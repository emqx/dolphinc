%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dolphinc_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

%%--------------------------------------------------------------------
%% Setups
%%--------------------------------------------------------------------

all() ->
    [t_sample, t_login].

init_per_suite(Cfg) ->
    _ = application:ensure_all_started(dolphinc),
    Cfg.

end_per_suite(_Cfg) ->
    application:stop(dolphinc),
    ok.

%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

t_sample(_) ->
    {ok, C} = dolphinc:start_link([{host, "127.0.0.1"}]),
    {ok, [], []} = dolphinc:run(C, ""),
    %% syntax error
    {error, _} = dolphinc:run(C, "1+a").

t_login(_) ->
    %% automatic login
    {ok, _} = dolphinc:start_link([{host, "127.0.0.1"},
                                   {username, "admin"},
                                   {password, "123456"}]),

    {ok, C} = dolphinc:start_link([{host, "127.0.0.1"}]),
    ok = dolphinc:login(C, "admin", "123456").
