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

:- use_module(library(http/json)).

:- begin_tests(text).

test(scenario) :-
    %% 0. settings
    getenv(docker_image_prefix, DOCKER_IMAGE_PREFIX),
    getenv(docker_image_tag, DOCKER_IMAGE_TAG),
    format(string(DOCKER_IMAGE), '~wswipl7action~w',
           [DOCKER_IMAGE_PREFIX, DOCKER_IMAGE_TAG]),

    %% 1. wsk action create
    atomics_to_string(['wsk action create hello_world',
                       ' samples/actions/hello_world.pl',
                       ' --main main --docker ', DOCKER_IMAGE, ' -i'],
                      CreateCmd),
    shell(CreateCmd, Status2),
    assertion(Status2 = 0),

    %% 2. wsk action invoke
    open(pipe("wsk action invoke hello_world -p name Prolog -ir"), read, S2),
    call_cleanup(
            read_string(S2, _, Result2),
            close(S2)),
    atom_json_dict(Result2, Dict2, []),
    assertion(_{payload: "Hello, Prolog!"} :< Dict2),

    %% 3. wsk action delete
    shell("wsk action delete hello_world -i", Status4),
    assertion(Status4 = 0).

:- end_tests(text).
