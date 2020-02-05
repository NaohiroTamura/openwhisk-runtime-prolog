
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
    format(string(DOCKER_IMAGE), '~w/swipl8action:~w',
           [DOCKER_IMAGE_PREFIX, DOCKER_IMAGE_TAG]),

    %% 1. wsk action create
    atomics_to_string(['ibmcloud wsk action create hello_world',
                       ' samples/actions/hello_world.pl',
                       ' --main main --docker ', DOCKER_IMAGE, ' -i'],
                      CreateCmd),
    shell(CreateCmd, Status2),
    assertion(Status2 = 0),

    %% 2. wsk action invoke
    open(pipe("ibmcloud wsk action invoke hello_world -p name Prolog -ir"), read, S2),
    call_cleanup(
            read_string(S2, _, Result2),
            close(S2)),
    atom_json_dict(Result2, Dict2, []),
    assertion(_{payload: "Hello, Prolog!"} :< Dict2),

    %% 3. wsk action delete
    shell("ibmcloud wsk action delete hello_world -i", Status4),
    assertion(Status4 = 0).

:- end_tests(text).

:- begin_tests(graphql).

test(scenario) :-
    %% 0. settings
    getenv(docker_image_prefix, DOCKER_IMAGE_PREFIX),
    getenv(docker_image_tag, DOCKER_IMAGE_TAG),
    format(string(DOCKER_IMAGE), '~w/swipl8action:~w',
           [DOCKER_IMAGE_PREFIX, DOCKER_IMAGE_TAG]),

    %% 1. wsk action create
    /*
    $ wsk action create graphql_test samples/actions/graphql.pl \
      --main main --docker nao16t/swipl7action:latest -i
    */
    atomics_to_string(['ibmcloud wsk action create graphql_test',
                       ' samples/actions/graphql.pl',
                       ' --main main --docker ', DOCKER_IMAGE, ' -i'],
                      CreateCmd),
    shell(CreateCmd, Status2),
    assertion(Status2 = 0),

    %% 2. wsk action invoke
    /*
    $ wsk action invoke graphql_test -ir \
      -p target fujitsu.com -p github_token $GITHUB_TOKEN \
      -p owner '"naohirotamura"' -p name '"faasshell"' \
      -p since '\"2018-06-21T00:00:00+00:00\"' \
      -p until '\"2018-07-20T00:00:00+00:00\"'
    */
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    format(string(PIPE_CMD),
           %% \c is continuous line
           "ibmcloud wsk action invoke graphql_test -ir \c
            -p target fujitsu.com -p github_token ~w \c
            -p owner \"naohirotamura\" -p name \"faasshell\" \c
            -p since \"2018-06-21T00:00:00+00:00\" \c
            -p until \"2018-07-20T00:00:00+00:00\"",
           [GITHUB_TOKEN]),

    open(pipe(PIPE_CMD), read, S2),
    call_cleanup(
            read_string(S2, _, Result2),
            close(S2)),
    atom_json_dict(Result2, Dict2, []),
    _{values: [History]} :< Dict2,
    assertion(length(History, 6)),

    %% 3. wsk action delete
    shell("ibmcloud wsk action delete graphql_test -i", Status4),
    assertion(Status4 = 0).

:- end_tests(graphql).
