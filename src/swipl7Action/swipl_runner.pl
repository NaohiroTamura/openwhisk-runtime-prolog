%% #!/usr/bin/swipl -q
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

%%
%%  
%%

%%:- module(swlpl_runner).

%% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_error)). % should be removed in puroduction

%% start
%% :- initialization(main).

%%
%% main
%%   $ swipl -q -l swipl_runner.pl -g main -t halt
%%
%% start:
%%   ?- asl_svc:main.
%% stop the server:
%%   ?- http_stop_server(8080,[]).
%%
main :-
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    getenv('SVC_PORT', Port) -> server(Port); server(8080).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%%
:- dynamic
       cached_code/4.

%% init action
:- http_handler('/init', init, [methods([post])]).

init(Request) :-
    http_log('~p~n', [request(Request)]),
    http_read_json_dict(Request, Dict, []),
    http_log('~p~n', [params(Dict)]),

    ( is_dict(Dict),
      _{value: _{name: Name, binary: Binary, main: Main, code: Code}} :< Dict
      -> term_string(Term, Code),
         save_term(Name, Code, File),
         Output = _{file: File},
         assertz(cached_code(Name, Binary, Main, Code)),
         Status = 200
      ;  Output = _{error: 'illegal parameter'},
         Status = 404
    ),
    reply_json_dict(Output, [status(Status)]).

%% run action
:- http_handler('/run', run, [methods([post])]).

run(Request) :-
    http_log('~p~n', [request(Request)]),
    http_read_json_dict(Request, Dict, []),
    http_log('~p~n', [params(Dict)]),

    ( is_dict(Dict),
      _{activation_id: ActivationID,
        action_name: ActionName,
        deadline: Deadline,
        api_key: API_KEY,
        value: Value,
        namespace:Namespace} :< Dict
      -> split_string(ActionName, "/", "", ["", NS, Name]),
         load_term(Name, File),
         cached_code(Name, Binary, Main, Code),
         retract(cached_code(Name, Binary, Main, Code)),
         atom_string(Func, Main),
         Q =.. [Func, Value, Output],
         Q,
         Status = 200
      ;  Output = _{error: 'illegal parameter'},
         Status = 404
    ),
    reply_json_dict(Output, [status(Status)]).

%%
term_json_dict(Term, Dict) :-
    ground(Term), !,
    atom_json_term(Atom, Term, []), atom_json_dict(Atom, Dict, []).
term_json_dict(Term, Dict) :-
    atom_json_dict(Atom, Dict, []), atom_json_term(Atom, Term, []).

%%
save_term(Name, Term, File) :-
    string_concat("/logs/", Name, File),
    open(File, write, S),
    call_cleanup(
            write_term(S, Term, []),
            %%format(S, '~p.~n', [Term]),
            close(S)).

load_term(Name, File) :-
    string_concat("/logs/", Name, File),
    load_files(File).
