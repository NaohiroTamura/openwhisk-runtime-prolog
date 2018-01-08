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

:- use_module(library(http/json)).

%% $ swipl -O --goal=go --stand_alone=true -o myexec -c hello_world_bin.pl
%%
%% $ zip myexec.zip myexec
%%
%% $ wsk action create hello_binary myexec.zip --main myexec \
%%   --docker ${docker_image_prefix}swipl7action -i
%%
%% $ wsk action invoke hello_binary -p name PrologBinary -ir

%% $ ./myexec '{"name":"PrologBinary"}'
%% {"payload":"Hello, PrologBinary!"}
%%
go :-
    current_prolog_flag(argv, [_Command, Json |_]),
    open_string(Json, S),
    json_read_dict(S, Dict),
    ( _{name: Name} :< Dict; Name = 'World' ),
    atomics_to_string(['Hello, ', Name, '!'], Greetings),
    json_write_dict(user_output, _{payload: Greetings}),
    halt(0).


