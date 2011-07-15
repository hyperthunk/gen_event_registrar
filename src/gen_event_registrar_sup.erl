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
%% @doc This is the top level supervisor for the gen_event_registrar application.
%%
%% -----------------------------------------------------------------------------
-module(gen_event_registrar_sup).
-author('Tim Watson <watson.timothy@gmail.com>').
-behaviour(supervisor).

-export([start_link/0]).
-export([start_event_manager/1]).
-export([init/1]).

-define(SUPERVISOR_NAME, gen_event_registrar_event_mgr_sup).

start_link() ->
  supervisor:start_link(?MODULE, []).

start_event_manager(EvMgrHandle) ->
    Id = make_child_id(EvMgrHandle),
    ChildSpec = 
        {Id, {?SUPERVISOR_NAME, start_link, [EvMgrHandle]},
              permanent, 5000, supervisor, dynamic},
    supervisor:start_child(?MODULE, ChildSpec).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.

make_child_id(EvMgrHandle) ->
    list_to_atom(
        atom_to_list(EvMgrHandle) ++ "_" ++ 
        atom_to_list(?SUPERVISOR_NAME)).
