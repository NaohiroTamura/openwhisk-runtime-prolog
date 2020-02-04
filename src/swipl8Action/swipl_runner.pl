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
%%  OpenWhisk runtimes for prolog
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
%%   ?- main.
%% stop the server:
%%   ?- http_stop_server(8080,[]).
%%
main :-
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    getenv('SVC_PORT', Port) -> server(Port); server(8080).

%% single threaded server
server(Port) :-
    http_server(http_dispatch, [port(Port), workers(1)]),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%% cache should not be removed because Invoker calls /run more than once
%% after calling /init.
:- dynamic
       cached_job/4.

%%
%% init action
%%
:- http_handler('/init', init, [methods([post])]).

init(Request) :-
    http_log('~p~n', [request(Request)]),
    http_read_json_dict(Request, Dict, []),
    http_log('~p~n', [params(Dict)]),

    ( check_init_param(Dict, Name, Binary, Main, Code)
      -> save_action(Name, Binary, Main, Code),
         assertz(cached_job(Name, Binary, Main, Code)),
         Output = "OK",
         Status = 200
      ;  Output = _{error: 'illegal parameter'},
         Status = 404
    ),
    reply_json_dict(Output, [status(Status)]).

%%
check_init_param(Dict, Name, Binary, Main, Code) :-
    is_dict(Dict),
    _{value: _{name: Name, binary: BinaryStr, main: Main, code: Code}} :< Dict,
    atom_string(Binary, BinaryStr).

%% Binary Mode
save_action(_Name, true, _Main, Code) :-
    base64(PlainCode, Code),
    open('/action/exec.zip', write, S1, [type(binary)]),
    call_cleanup(
            write(S1, PlainCode),
            close(S1)),
    open(pipe('cd /action; unzip -o exec.zip'), read, S2),
    call_cleanup(
            read_string(S2, _N, Unzip),
            close(S2)),
    http_log('Unzip: ~w', [Unzip]).

%% Text Mode
save_action(Name, false, _Main, Code) :-
    term_string(_Term, Code), %% make sure Code has no syntax error
    save_term(Name, Code, File),
    http_log('Saved: ~w', [File]).

%%
%% run action
%%
:- http_handler('/run', run, [methods([post])]).

run(Request) :-
    http_log('~p~n', [request(Request)]),
    catch( run1(Request),
           Error,
           ( Error =.. [ErrorType, Message, Code |_],
             http_log('~w~n', [catch((ErrorType, Message, Code))]),
             reply_json_dict(_{errorMessage: Message,
                               errorType: ErrorType}, [status(Code)])
           )
         ).

run1(Request) :-
    http_read_json_dict(Request, Dict, []),
    http_log('~p~n', [params(Dict)]),

    ( check_run_param(Dict, ActivationID, ActionName, Value)
      -> split_string(ActionName, "/", "", ["", NS, Name]),
         http_log('Namespace: ~p, Name: ~p~n', [NS, Name]),
         ( cached_job(Name, Binary, Main, Code)
           -> format(user_output, 'cached_job OK: ~p~n',
                     [(Name, Binary, Main, Code)]),
              exec(Name, Binary, Main, Code, Value, ActivationID, Output, Status)
           ;  format(user_output, 'cached_job NG: ~p~n',
                     [(Name, Binary, Main, Code)]),
              Output = _{error: 'internal state error'},
              Status = 500
         )
      ;  Output = _{error: 'illegal parameter'},
         Status = 404
    ),
    ( atomic(Output)
      -> reply_json(Output, [status(Status)])
      ;  reply_json_dict(Output, [status(Status)])
    ).

%%
check_run_param(Dict, ActivationID, ActionName, Value) :-
    is_dict(Dict),
    _{activation_id: ActivationID,
      action_name: ActionName,
      deadline: _Deadline,
      api_key: _API_KEY,
      value: Value,
      namespace: _Namespace} :< Dict.

%% Binary Execution
exec(Name, true, Main, Code, Value, ActivationID, Output, Status) :-
    http_log('Binary: ~p~n', [(Name, Main, Code)]),
    atom_json_dict(Arg, Value, []),
    format(string(Command), "cd /action; ./~w '~w'", [Main, Arg]),
    format(user_output,
           '~nExecute Binary: ~w~nActivation ID: ~w~n',
           [Command, ActivationID]),
    open(pipe(Command), read, S2),
    call_cleanup(
            read_string(S2, _, All),
            close(S2)),
    %%format(user_output, '~n~p~n', [all(All)]),
    split_string(All, '\n' ,'', Lines),
    exclude(=(""), Lines, LinesNoBlank),
    last(LinesNoBlank, Json),
    %%format(user_output, '~n~p~n', [json(Json)]),
    catch( atom_json_dict(Json, Output, [as(string)]),
           error(Error,_),
           ( term_string(Error, ErrorStr),
             Output = _{output: ErrorStr} )
         ),
    format(user_output,
           '~nBinary Exection Result: ~p~nActivation ID: ~w~n',
           [Output, ActivationID]),
    Status = 200.

%% Text Execution
exec(Name, false, Main, Code, Value, ActivationID, Output, Status) :-
        http_log('Text  : ~p~n', [(Name, false, Main, Code)]),
        load_term(Name, File),
        atom_string(Func, Main),
        Q =.. [Func, Value, Output],
        format(user_output,
               '~nExecute Text: ~w, Function: ~w~nActivation ID: ~w~n',
               [File, Q, ActivationID]),
        Q,
        format(user_output,
               '~nText Exection Result: ~p~nActivation ID: ~w~n',
               [Output, ActivationID]),
        Status = 200.

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
