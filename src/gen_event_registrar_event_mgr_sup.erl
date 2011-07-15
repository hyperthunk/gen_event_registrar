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
%% @doc This process supervises an event manager, and a child supervision tree
%% by menas of a gen_event_registrar_bridge_sup instance.
%%
%% -----------------------------------------------------------------------------
-module(gen_event_registrar_event_mgr_sup).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(EvMgrHandle) ->
    supervisor:start_link(?MODULE, [EvMgrHandle]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([EvMgrHandle]) ->
    gproc:add_local_name({EvMgrHandle, ?MODULE}),
    Children = [
        {gen_event_registrar_handler, 
            {gen_event, start_link, [EvMgrHandle]},
             permanent, 5000, worker, dynamic},
        {gen_event_registrar_bridge_sup,
            {gen_event_registrar_bridge_sup, start_link, [EvMgrHandle]},
             permanent, 5000, supervisor, dynamic}
    ],
    {ok, {{rest_for_one, 5, 5}, Children}}.
