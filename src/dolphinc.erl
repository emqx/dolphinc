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

-module(dolphinc).

%% API
-export([ start_link/0
        , start_link/1
        , login/3
        , run/2
        , run/3
        ]).

%% Callback
-export([init/2]).

%% Sys callbacks
-export([ system_continue/3
        , system_terminate/4
        , system_code_change/4
        , system_get_state/1
        ]).

%% Internal callback
-export([wakeup_from_hib/2]).

-record(st,
        { host :: inet:socket_address() | inet:hostname()
        , port :: inet:port_number()
        , sock :: port()
        , sid  :: binary()  %% Session ID
        , username :: undefined | binary()
        , password :: undefined | function()
        , parser :: term()
        , requests = queue:new()
        , cnt_sent = 0
        , cnt_recv = 0
        }).

-type option()
        :: {host, inet:socket_address() | inet:hostname()} %% default to 127.0.0.1
         | {port, inet:port_number()} %% default to 8848
         | {username, iolist()}
         | {password, iolist()}
         .

%% TODO:
-define(MAX_SEG_SIZE, 1048576). %% 1MB

%%--------------------------------------------------------------------
%% APIs
%%--------------------------------------------------------------------

start_link() ->
    start_link([]).

-spec start_link([option()]) -> {ok, pid()}.
start_link(Options) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Options]),
    {ok, Pid}.

-spec login(pid(), binary(), binary()) -> ok | {error, term()}.
login(C, Username, Password) ->
    Script = [<<"login(\"">>, Username, "\",\"", Password, "\")"],
    run(C, Script).

run(C, Script) ->
    run(C, Script, infinity).

-spec run(pid(), iodata(), timeout())
  -> {ok, Data :: term(), Msgs :: [term()]}
   | {error, term()}.
run(C, Script, Timeout) ->
    WaitTs = case Timeout of
                 infinity -> infinity;
                 _ -> Timeout + 1000
             end,
    gen_server:call(C, {script, Script, Timeout}, WaitTs).

%%--------------------------------------------------------------------
%% callbacks
%%--------------------------------------------------------------------

init(Parent, Options) ->
    Host = proplists:get_value(host, Options, "127.0.0.1"),
    Port = proplists:get_value(port, Options, 8848),
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {ok, Sock} ->
            _ = gen_tcp:send(Sock, fe_connect()),
            case gen_tcp:recv(Sock, 0, 5000) of
                {ok, Bytes} ->
                    SId = fd_connect(Bytes),
                    St = #st{host = Host,
                             port = Port,
                             sock = Sock,
                             sid = SId,
                             parser = init_parse_state()
                            },
                    {ok, NSt} = may_do_login(St, Options),
                    inet:setopts(Sock, [{active, true}]),
                    proc_lib:init_ack(Parent, {ok, self()}),
                    run_loop(Parent, NSt);
                {error, R} ->
                    exit(R)
            end;
        {error, R} ->
             exit(R)
    end.

%%--------------------------------------------------------------------
%% Recv Loop

run_loop(Parent, St) ->
    hibernate(Parent, St).

recvloop(Parent, St) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, [], St);
        {'EXIT', Parent, Reason} ->
            terminate(Reason, St);
        Msg ->
            process_msg([Msg], Parent, St)
    after
        5000 ->
            io:format("HHHHHHHHHHHHHIBER\n"),
            hibernate(Parent, St)
    end.

hibernate(Parent, St) ->
    proc_lib:hibernate(?MODULE, wakeup_from_hib, [Parent, St]).

%% Maybe do something here later.
wakeup_from_hib(Parent, St) -> recvloop(Parent, St).

process_msg([], Parent, St) -> recvloop(Parent, St);

process_msg([Msg|More], Parent, St) ->
    case catch handle_msg(Msg, St) of
        ok ->
            process_msg(More, Parent, St);
        {ok, NSt} ->
            process_msg(More, Parent, NSt);
        {ok, Msgs, NSt} ->
            process_msg(append_msg(More, Msgs), Parent, NSt);
        {stop, Reason} ->
            terminate(Reason, St);
        {stop, Reason, NSt} ->
            terminate(Reason, NSt);
        {'EXIT', Reason} ->
            terminate(Reason, St)
    end.

-compile({inline, [append_msg/2]}).
append_msg([], Msgs) when is_list(Msgs) ->
    Msgs;
append_msg([], Msg) -> [Msg];
append_msg(Q, Msgs) when is_list(Msgs) ->
    lists:append(Q, Msgs);
append_msg(Q, Msg) ->
    lists:append(Q, [Msg]).

handle_msg({'$gen_call', From, Req}, St) ->
    case handle_call(From, Req, St) of
        {noreply, NSt} ->
            {ok, NSt};
        {reply, Reply, NSt} ->
            gen_server:reply(From, Reply),
            {ok, NSt};
        {stop, Reason, NSt} ->
            gen_server:reply(From, {error, Reason}),
            stop(Reason, NSt)
    end;

handle_msg(Shutdown = {shutdown, _Reason}, St) ->
   stop(Shutdown, St);

handle_msg(Msg, St) ->
    handle_info(Msg, St).

-compile({inline, [shutdown/2]}).
shutdown(Reason, St) ->
    stop({shutdown, Reason}, St).

-compile({inline, [stop/2]}).
stop(Reason, St) ->
    {stop, Reason, St}.

%%--------------------------------------------------------------------
%% Terminate

terminate(Reason, _St) ->
    logger:error("Terminated due to ~p", [Reason]),
    exit(Reason).

%%--------------------------------------------------------------------
%% Sys callbacks

system_continue(Parent, _Debug, St) ->
    recvloop(Parent, St).

system_terminate(Reason, _Parent, _Debug, St) ->
    terminate(Reason, St).

system_code_change(St, _Mod, _OldVsn, _Extra) ->
    {ok, St}.

system_get_state(St) -> {ok, St}.

handle_call(From, Req = {script, _, _}, St = #st{sock = Sock, sid = SId, requests = Reqs}) ->
    Msgs = drian_script_call([{From, Req}]),
    NowTs = erlang:system_time(millisecond),

    {Bytes0, NReqs} =
        lists:foldl(fun({F, {script, S, Ts}}, {Bs, Rs}) ->
            EndTs = case Ts of
                        infinity -> infinity;
                        _ -> NowTs + Ts
                    end,
            {[fe_script(SId, S) | Bs], queue:in({F, EndTs}, Rs)}
        end, {[], Reqs}, Msgs),

    NSt = St#st{requests = NReqs},
    Bytes = lists:reverse(Bytes0),
    case gen_tcp:send(Sock, Bytes) of
        {error, Reason} ->
            shutdown({sock_err, Reason}, NSt);
        ok ->
            io:format("SEND: ~p~n", [Bytes]),
            {noreply, incr(sent, length(Msgs), NSt)}
    end;

handle_call(_From, _Req, St) ->
    {reply, {error, unknown_call}, St}.

handle_info({tcp, _Sock, Bytes}, St) ->
    io:format("RECV: ~p~n", [Bytes]),
    handle_incoming(Bytes, St);

handle_info({tcp_closed, _Sock}, St) ->
    shutdown({sock_err, tcp_closed}, St);

handle_info({tcp_error, _Sock, Reason}, St) ->
    shutdown({sock_err, Reason}, St);

handle_info(_Info, St) ->
    {ok, St}.

%%--------------------------------------------------------------------
%% Internal funcs
%%--------------------------------------------------------------------

incr(sent, N1, St = #st{cnt_sent = N0}) ->
    St#st{cnt_sent = N1+N0};
incr(recv, N1, St = #st{cnt_recv = N0}) ->
    St#st{cnt_recv = N1+N0}.

drian_script_call(Acc) ->
    receive
        {'$gen_call', From, Req} ->
            drian_script_call([{From, Req}|Acc])
    after
        0 ->
            lists:reverse(Acc)
    end.

handle_incoming(Bytes, St = #st{parser = PSt}) ->
    NowTs = erlang:system_time(millisecond),
    {ok, Pkts, NPSt} = parse(Bytes, PSt),
    reply_requests(Pkts, NowTs, St#st{parser = NPSt}).

reply_requests([], _, St) ->
    {ok, St};
reply_requests([Pkt|More], NowTs, St0 = #st{requests = Reqs}) ->
    St = incr(recv, 1, St0),
    case queue:out(Reqs) of
        {empty, NReqs} ->
            shutdown({fatal_error, not_found_request}, St#st{requests = NReqs});
        {{value, {From, EndTs}}, NReqs} ->
            EndTs >= NowTs andalso begin
                Reply = case Pkt of
                            #{error := Error} -> {error, Error};
                            #{data := Data} ->
                                {ok, Data, maps:get(msgs, Pkt, [])}
                        end,
               gen_server:reply(From, Reply)
            end,
            reply_requests(More, NowTs, St#st{requests = NReqs})
    end.

%% @private
may_do_login(St, Options) ->
    case {proplists:get_value(username, Options),
          proplists:get_value(password, Options)} of
        {undefined, undefined} ->
            {ok, St};
        {undefined, _} ->
            {error, miss_password_option};
        {Username, Password} ->
            do_login(Username, Password, St)
    end.

%% @private
do_login(Username, Password, St = #st{sock = Sock, sid = SId}) ->
    Script = [<<"login(\"">>, Username, "\",\"", Password, "\")"],
    _ = gen_tcp:send(Sock, fe_script(SId, Script)),
    case recv_a_packet(St) of
        {error, Reason} -> {error, Reason};
        {ok, #{error := Err}, _} ->
            {error, Err};
        {ok, _, NSt} ->
            {ok, NSt#st{username = Username, password = fun() -> Password end}}
    end.

recv_a_packet(St = #st{sock = Sock, parser = PSt}) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Bytes} ->
            {ok, [Pkt], NPSt} = parse(Bytes, PSt),
            {ok, Pkt, St#st{parser = NPSt}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Utils for Frame encoding

fe_connect() ->
    <<"API 0 8\nconnect\n">>.

fe_script(SId, Script) ->
    Len = integer_to_binary(iolist_size(Script) + 7),
    ["API ", SId, " ", Len, " / 0_1_4_2\n",
     "script\n",
     Script].

%%--------------------------------------------------------------------
%% Parser

init_parse_state() ->
    #{rest => <<>>}.

parse(In, PSt = #{rest := Rest}) ->
    Bytes = <<Rest/binary, In/binary>>,
    parse(Bytes, [], PSt#{rest => <<>>}).

parse(<<>>, Acc, PSt) ->
    {ok, lists:reverse(Acc), PSt};

parse(Bytes, Acc, PSt) ->
    case binary:split(Bytes, <<"\n">>) of
        [Hdr, Rest1] ->
            [SId, Cnt0, Endian] = binary:split(Hdr, <<" ">>, [global]),
            Cnt = binary_to_integer(Cnt0),
            case binary:split(Rest1, <<"\n">>) of
                [<<"OK">>, Rest] ->
                    Pkt = #{header => #{sid => SId, cnt => Cnt, ed => Endian},
                            data => []
                           },
                    case Cnt == 0 of
                        true ->
                            parse(Rest, [Pkt|Acc], PSt);
                        _ ->
                            error({not_supported_data_return, Rest})
                    end;
                [Err, Rest] ->
                    Pkt = #{header => #{sid => SId, cnt => Cnt, ed => Endian},
                            error => Err
                           },
                    parse(Rest, [Pkt|Acc], PSt);
                _ ->
                    parse(<<>>, Acc, PSt#{rest => Bytes})
            end;
        _ ->
            parse(<<>>, Acc, PSt#{rest => Bytes})
    end.

fd_connect(Bytes) ->
    case re:split(Bytes, "\n", [{parts, 3}]) of
        [Hdr, <<"OK">>, _] ->
            [SId|_] = re:split(Hdr, " "),
            SId;
        _ ->
            error({failed_connect_to_server, Bytes})
    end.
