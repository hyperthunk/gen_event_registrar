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
%% @doc This process provides a simple_one_for_one supervision tree for instances
%% of the gen_event_registrar_bridge_server gen_server process.
%%
%% -----------------------------------------------------------------------------
-module(gen_event_registrar_bridge_sup).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-export([add_handler/3]).

%%
%% Public API
%%

start_link(EventManager) ->
    supervisor:start_link(?MODULE, [EventManager]).

add_handler(EventManager, Handler, Args) ->
    Pid = gproc:lookup_local_name({EventManager, ?MODULE}),
    supervisor:start_child(Pid, [EventManager, Handler, Args]).

%%
%% Supervisor callbacks
%%

init([EventManager]) ->
    gproc:add_local_name({EventManager, ?MODULE}),
    ChildTemplate = [{'_',
        {gen_event_registrar_bridge_server, start_link, []},
         transient, 5000, worker, [gen_server]}],
    {ok, {{simple_one_for_one, 10, 10}, ChildTemplate}}.
