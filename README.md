# OpenWhisk runtimes for prolog

This OpenWhisk runtimes provide SWI Prolog 7.5 environment for Prolog predicate, Prolog script, and Prolog compiled binary.

## Build

```sh
ubuntu@trusty:~/openwhisk-runtime-prolog[master]$ make build -e docker_image_prefix=myprefix/

ubuntu@trusty:~/openwhisk-runtime-prolog[master]$ make push -e docker_image_prefix=myprefix/

```

## Usage

### Text Mode

* execute "main" predicate of hello_world.pl

```sh
ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action create hello_prolog hello_world.pl --docker ${docker_image_prefix}swipl7action -i
ok: created action hello_prolog

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action invoke hello_prolog -p name Prolog -ir
{
    "payload": "Hello, Prolog!"
}
```

* execute "start" predicate of hello_world_start.pl

```sh
ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action create hello_start hello_world_start.pl --main start --docker ${docker_image_prefix}swipl7action -i
ok: created action hello_start

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action invoke hello_start -p name Entrypoint -ir
{
    "payload": "Hello, Entrypoint!"
}
```

### Binary Mode

* execute Prolog script, hello_script, which file name can be specified as '--main' parameter.

```sh
ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ ./hello_world '{"name":"Script"}'
{"payload":"Hello, Script!"}

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ zip hello_world.zip hello_world
  adding: hello_world (deflated 43%)

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action create hello_script hello_world.zip --main hello_world --docker ${docker_image_prefix}swipl7action -i
ok: created action hello_script

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action invoke hello_script -p name PrologScript -ir
{
    "payload": "Hello, PrologScript!"
}
```

* execute Prolog compiled binary, myexec, which file name can be specified as '--main' parameter.

```sh
ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ swipl -O --goal=go --stand_alone=true -o myexec -c hello_world_bin.pl
% autoloading prolog_codewalk:must_be/2 from /swipl/lib/swipl-7.5.15/library/error
% autoloading json:maplist/2 from /swipl/lib/swipl-7.5.15/library/apply
% autoloading json:select/3 from /swipl/lib/swipl-7.5.15/library/lists
% autoloading json:assertion/1 from /swipl/lib/swipl-7.5.15/library/debug
% autoloading prolog_debug:backtrace/1 from /swipl/lib/swipl-7.5.15/library/prolog_stack
% autoloading oset:reverse/2 from /swipl/lib/swipl-7.5.15/library/lists
% autoloading prolog_codewalk:clause_info/4 from /swipl/lib/swipl-7.5.15/library/prolog_clause
% autoloading prolog_codewalk:initialization_layout/4 from /swipl/lib/swipl-7.5.15/library/prolog_clause
% autoloading prolog_codewalk:portray_clause/1 from /swipl/lib/swipl-7.5.15/library/listing
% autoloading record:member/2 from /swipl/lib/swipl-7.5.15/library/lists
% autoloading error:assertion/1 from /swipl/lib/swipl-7.5.15/library/debug
% autoloading qsave:current_foreign_library/2 from /swipl/lib/swipl-7.5.15/library/shlib
% autoloading prolog_codewalk:clause_name/2 from /swipl/lib/swipl-7.5.15/library/prolog_clause
% Autoloader: iteration 1 resolved 9 predicates and loaded 13 files in 0.214 seconds.  Restarting ...
% Autoloader: loaded 9 files in 2 iterations in 0.295 seconds

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ ./myexec '{"name":"Binary"}'
{"payload":"Hello, Binary!"}

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ zip myexec.zip myexec
  adding: myexec (deflated 63%)

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ wsk action create hello_binary myexec.zip --main myexec --docker ${docker_image_prefix}swipl7action -i
ok: created action hello_binary

ubuntu@trusty:~/openwhisk-runtime-prolog/samples/actions[master]$ $ wsk action invoke hello_binary -p name PrologBinary -ir
{
    "payload": "Hello, PrologBinary!"
}

```
