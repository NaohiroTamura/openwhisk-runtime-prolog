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

:- include('src/swipl7Action/swipl_runner.pl').

%%
:- use_module(library(plunit)).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

%%
read_code(File, Code) :-
    read_code(File, Code, []).

read_code(File, Code, Options) :-
    open(File, read, S, Options),
    call_cleanup(
            read_string(S, _N, Code),
            close(S)).

%%
%% 
%%
:- begin_tests(hello_world).

test(secnarios) :-
    %% 1. init_text
    read_code('samples/actions/hello_world.pl', Code1),
    Params1 = _{value:
                _{name: "helloProlog",
                  binary: "false",
                  main: "main",
                  code: Code1
                 }
               },
    term_json_dict(Json1, Params1),
    http_post("http://127.0.0.1:8080/init", json(Json1), Data1,
              [status_code(StatusCode1)]),
    term_json_dict(Data1, Dict1),
    assertion((Dict1, StatusCode1) = ("OK", 200)),

    %% 2. run_text
    Params2 = _{activation_id: "307bd9e1b82b4b62bbd9e1b82b1b62b0",
                action_name: "/guest/helloProlog",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "runner"},
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion((Dict2, StatusCode2) = (_{payload:"Hello, runner!"}, 200)),

    %% 2. init_zip
    open(pipe('cd samples/actions; zip - exec'), read, S3, [type(binary)]),
    read_string(S3, _N, Code3),
    base64(Code3, Base64),
    Params3 = _{value:
                _{name: "prologScript",
                  binary: "true",
                  main: "main",
                  code: Base64
                 }
               },
    term_json_dict(Json3, Params3),
    http_post("http://127.0.0.1:8080/init", json(Json3), Data3,
              [status_code(StatusCode3)]),
    term_json_dict(Data3, Dict3),
    assertion((Dict3, StatusCode3) = ("OK", 200)),

    %% 4. run_zip
    Params4 = _{activation_id: "307bd9e1b82b4b62bbd9e1b82b1b62b0",
                action_name: "/guest/prologScript",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "runner"},
                namespace: "guest"},
    term_json_dict(Json4, Params4),
    http_post("http://127.0.0.1:8080/run", json(Json4), Data4,
              [status_code(StatusCode4)]),
    term_json_dict(Data4, Dict4),
    assertion((Dict4, StatusCode4) = (_{payload:"Hello, runner!"}, 200)).

test(init_error, (Dict, StatusCode) = (_{error:"illegal parameter"}, 404)) :-
    Params = 1,
    term_json_dict(Json, Params),
    http_post("http://127.0.0.1:8080/init", json(Json), Data,
              [status_code(StatusCode)]),
    term_json_dict(Data, Dict).

test(run_error, (Dict, StatusCode) = (_{error:"illegal parameter"}, 404)) :-
    Params = 1,
    term_json_dict(Json, Params),
    http_post("http://127.0.0.1:8080/run", json(Json), Data,
              [status_code(StatusCode)]),
    term_json_dict(Data, Dict).

:- end_tests(hello_world).
