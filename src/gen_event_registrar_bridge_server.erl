%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson [http://hyperthunk.wordpress.com]
%% @copyright (c) Tim Watson, 2011
%%
%% @doc
%%
%% -----------------------------------------------------------------------------
-module(gen_event_registrar_bridge_server).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/3]).
-record(state, { event_manager, event_handler, log }).

%%
%% Public API
%%

start_link(EventManager, Handler, Options) ->
    gen_server:start_link(?MODULE, [EventManager, Handler, Options], []).

%%
%% gen_server callbacks
%%

%% @hidden
init([EventManager, Handler, Args]) ->
    Result = gen_event:add_sup_handler(EventManager, Handler, Args),
    case Result of
        ok ->
            process_flag(trap_exit, true),
            gproc:add_local_name({EventManager, Handler}),
            Log = case code:which(fastlog) of
                non_existing -> fun error_logger:info_msg/2;
                _Other -> fun fastlog:info/2
            end,
            {ok, #state{ event_manager=EventManager, 
                         event_handler=Handler,
                         log=Log }};
        Other ->
            {stop, {subscription_failed, Other}}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, Handler, normal}, State=#state{ log=Log }) ->
    Log("Ignoring gen_event normal exit signal for ~p~n", [Handler]),
    {noreply, State};
handle_info({gen_event_EXIT, Handler, shutdown}, State=#state{ log=Log }) ->
    Log("Ignoring gen_event shutdown for ~p~n", [Handler]),
    {noreply, State};
handle_info({gen_event_EXIT, Handler, {swapped, NewHandler, _}}, 
                                                State=#state{ log=Log }) ->
    Log("Ignoring gen_event handler swap (~p for ~p)~n", [Handler, NewHandler]),
    {noreply, State};
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    {stop, {exit, Handler, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
