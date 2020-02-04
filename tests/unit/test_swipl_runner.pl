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

:- include('src/swipl8Action/swipl_runner.pl').

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
%% Unit Test
%%
:- begin_tests(hello_world).

test(scenario) :-
    %% 1. init_text
    read_code('samples/actions/hello_world.pl', Code1),
    Params1 = _{value:
                _{name: "hello_prolog",
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
                action_name: "/guest/hello_prolog",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "Prolog"},
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion((Dict2, StatusCode2) = (_{payload:"Hello, Prolog!"}, 200)),

    %% 3. init_zip
    open(pipe('cd samples/actions; zip - hello_world_script'), read, S3,
         [type(binary)]),
    call_cleanup(
            read_string(S3, _N, Code3),
            close(S3)),
    base64(Code3, Base64),
    Params3 = _{value:
                _{name: "prolog_script",
                  binary: "true",
                  main: "hello_world_script",
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
                action_name: "/guest/prolog_script",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "PrologScript"},
                namespace: "guest"},
    term_json_dict(Json4, Params4),
    http_post("http://127.0.0.1:8080/run", json(Json4), Data4,
              [status_code(StatusCode4)]),
    term_json_dict(Data4, Dict4),
    assertion((Dict4, StatusCode4) = (_{payload:"Hello, PrologScript!"}, 200)).

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

%%
:- begin_tests(hello_start).

test(hello_start) :-
    %% 1. init_text
    read_code('samples/actions/hello_world_start.pl', Code1),
    Params1 = _{value:
                _{name: "hello_start",
                  binary: "false",
                  main: "start",
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
                action_name: "/guest/hello_start",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "Entrypoint"},
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion((Dict2, StatusCode2) = (_{payload:"Hello, Entrypoint!"}, 200)).

:- end_tests(hello_start).

%%
:- begin_tests(hello_scala).

test(hello_scala) :-
    %% 1. init_text
    read_code('samples/actions/hello_world_scala.pl', Code1),
    Params1 = _{value:
                _{name: "hello_scala",
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
                action_name: "/guest/hello_scala",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: "Scala",
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    atomic(Data2),
    assertion((Data2, StatusCode2) = ('Hello, Scala!', 200)).

:- end_tests(hello_scala).

%%
:- begin_tests(hello_exception).

test(hello_exception) :-
    %% 1. init_text
    read_code('samples/actions/hello_exception.pl', Code1),
    Params1 = _{value:
                _{name: "hello_exception",
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
                action_name: "/guest/hello_exception",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: "",
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion(StatusCode2 = 200),
    assertion(Dict2 = _{errorMessage: "This is a custom error!",
                        errorType: "CustomError"}).

:- end_tests(hello_exception).

:- begin_tests(shell_script).

test(shell_script) :-
    %% 1. init_zip
    open(pipe('cd samples/actions; zip - shell_script'), read, S1,
         [type(binary)]),
    call_cleanup(
            read_string(S1, _N, Code1),
            close(S1)),
    base64(Code1, Base64),
    Params1 = _{value:
                _{name: "shell_script",
                  binary: "true",
                  main: "shell_script",
                  code: Base64
                 }
               },
    term_json_dict(Json1, Params1),
    http_post("http://127.0.0.1:8080/init", json(Json1), Data1,
              [status_code(StatusCode1)]),
    term_json_dict(Data1, Dict1),
    assertion((Dict1, StatusCode1) = ("OK", 200)),

    %% 2. run_zip
    Params2 = _{activation_id: "307bd9e1b82b4b62bbd9e1b82b1b62b0",
                action_name: "/guest/shell_script",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{name: "ShellScript"},
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion((Dict2, StatusCode2) = (_{result: "ok"}, 200)).

:- end_tests(shell_script).


:- begin_tests(graphql).

test(github) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),

    %% 1. init_text
    read_code('samples/actions/graphql.pl', Code1),
    Params1 = _{value:
                _{name: "graphql",
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
                action_name: "/guest/graphql",
                deadline: "1515113315714",
                api_key: "ID:PW",
                value: _{ target: "fujitsu.com",
                          github_token: GITHUB_TOKEN,
                          owner: '"naohirotamura"',
                          name: '"faasshell"',
                          since: '"2018-06-21T00:00:00+00:00"',
                          until: '"2018-07-20T00:00:00+00:00"'
                        },
                namespace: "guest"},
    term_json_dict(Json2, Params2),
    http_post("http://127.0.0.1:8080/run", json(Json2), Data2,
              [status_code(StatusCode2)]),
    term_json_dict(Data2, Dict2),
    assertion((Dict2, StatusCode2) = (_{payload: _}, 200)).

:- end_tests(graphql).
