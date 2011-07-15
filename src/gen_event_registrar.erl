%% -----------------------------------------------------------------------------
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
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
%%
%% -----------------------------------------------------------------------------
%%
%% @doc This module provides the high level gen_event_registrar API.
%%
%% -----------------------------------------------------------------------------

-module(gen_event_registrar).
-author('Tim Watson <watson.timothy@gmail.com>').
-compile(export_all).

%%
%% @doc Hook to start the gen_event_registrar application outside of a release,
%% for example in a development or test environment prior to when an OTP release
%% is being used.
%% 
start() -> appstart:start(gen_event_registrar).

-spec(start_supervised_event_manager/1 :: 
                    (Name::atom()) -> supervisor:startchild_ret()).
start_supervised_event_manager(Name) when is_atom(Name) ->
    gen_event_registrar_sup:start_event_manager(Name).


