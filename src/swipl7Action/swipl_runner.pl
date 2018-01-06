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

:- module(swlpl_runner, [main/0]).

%% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_error)). % should be removed in puroduction

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

%% Only one job is to be sent to the ContainerProxy at one time.
:- dynamic
       cached_job/4.

%% init action
:- http_handler('/init', init, [methods([post])]).

init(Request) :-
    http_log('~p~n', [request(Request)]),
    http_read_json_dict(Request, Dict, []),
    http_log('~p~n', [params(Dict)]),

    ( is_dict(Dict),
      _{value: _{name: Name, binary: Binary, main: Main, code: Code}} :< Dict
      -> ( Binary = "true"
           -> base64(PlainCode, Code),
              open('/action/exe.zip', write, S1, [type(binary)]),
              call_cleanup(
                      write(S1, PlainCode),
                      close(S1)),
              open(pipe('cd /action; unzip -o exe.zip'), read, S2),
              call_cleanup(
                      read_string(S2, _N, Unzip),
                      close(S2)),
              format(user_output, 'Unzip: ~w', [Unzip])
           ;  term_string(_Term, Code), %% make sure Code has no syntax error
              save_term(Name, Code, File),
              format(user_output, 'Saved: ~w', [File])
         ),
         assertz(cached_job(Name, Binary, Main, Code)),
         Output = "OK",
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
        namespace: Namespace} :< Dict
      -> ( split_string(ActionName, "/", "", ["", NS, Name]),
           format(user_output, 'Namespace: ~p, Name: ~p, ~p~n',
                  [NS, Name, params(ActivationID, ActionName, Deadline,
                                    API_KEY, Value, Namespace)]),
           cached_job(Name, Binary, Main, Code),
           (  Binary = "true"
              -> http_log('Binary: ~p~n', [job(Name, Binary, Main, Code)]),
                 atom_json_dict(Arg, Value, []),
                 format(string(Command), "cd /action; ./exe '~w'", [Arg]),
                 format(user_output, 'Command: ~w~n', [Command]),
                 open(pipe(Command), read, S2),
                 call_cleanup(
                         read_string(S2, _N, Json),
                         close(S2)),
                 atom_json_dict(Json, Output, []),
                 format(user_output, 'Result: ~p, ~p~n', [Json, Output]),
                 Status = 200
              ;  http_log('Text  : ~p~n', [job(Name, Binary, Main, Code)]),
                 load_term(Name, File),
                 atom_string(Func, Main),
                 Q =.. [Func, Value, Output],
                 format(user_output, 'File: ~w, Function: ~w~n', [File, Q]),
                 Q,
                 Status = 200
           ),
           retract(cached_job(Name, Binary, Main, Code))
         )
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
    string_concat("/action/", Name, File),
    open(File, write, S),
    call_cleanup(
            write_term(S, Term, []),
            close(S)).

load_term(Name, File) :-
    string_concat("/action/", Name, File),
    load_files(File).
