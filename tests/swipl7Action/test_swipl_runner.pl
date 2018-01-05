%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2017 FUJITSU LIMITED
%%
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
%%

:- use_module(library(plunit)).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- include('src/swipl7Action/swipl_runner.pl').

read_code(File, Code) :-
    open(File, read, S),
    call_cleanup(
            read_string(S, _N, Code),
            close(S)).

%%
%% 
%%
:- begin_tests(hello_world).

test(init, (Dict, StatusCode) = (_{file:"/logs/helloProlog"}, 200)) :-
    read_code('samples/actions/hello_world.pl', Code),
    Params = _{value:
               _{name: "helloProlog",
                 binary: "false",
                 main: "main",
                 code: Code
                }
              },
    term_json_dict(Json, Params),
    http_post("http://127.0.0.1:8080/init", json(Json), Data,
              [status_code(StatusCode)]),
    term_json_dict(Data, Dict).

test(init_error, (Dict, StatusCode) = (_{error:"illegal parameter"}, 404)) :-
    Params = 1,
    term_json_dict(Json, Params),
    http_post("http://127.0.0.1:8080/init", json(Json), Data,
              [status_code(StatusCode)]),
    term_json_dict(Data, Dict).

test(run, (Dict, StatusCode) = (_{payload:"Hello, runner!"}, 200)) :-
    Params = _{activation_id: "307bd9e1b82b4b62bbd9e1b82b1b62b0",
               action_name: "/guest/helloProlog",
               deadline: "1515113315714",
               api_key: "ID:PW",
               value: _{name: "runner"},
               namespace: "guest"},
    term_json_dict(Json, Params),
    http_post("http://127.0.0.1:8080/run", json(Json), Data,
              [status_code(StatusCode)]),
    term_json_dict(Data, Dict).

:- end_tests(hello_world).
