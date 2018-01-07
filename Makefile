# Copyright 2017 FUJITSU LIMITED
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.
#

SHELL = /bin/bash

docker_image_prefix =
docker_image_tag = :latest

unit_test_files := $(wildcard tests/unit/test_*.pl)
functional_test_files := $(wildcard tests/functional/test_*.pl)

all: unit_test

unit_test:
	@echo "unit  test"
	swipl -q -l src/swipl7Action/swipl_runner.pl -g main -t halt &
	sleep 3
	for case in $(unit_test_files); do \
		echo $$case; \
		swipl -q -l $$case -g run_tests -t halt; \
	done
	pkill -HUP swipl

functional_test:
	@echo "functional  test"
	for case in $(functional_test_files); do \
		echo $$case; \
		swipl -q -l $$case -g run_tests -t halt; \
	done

# make build -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1
build:
	@echo "create runtime image"
	@pushd src/swipl7Action && \
	docker build -t $(docker_image_prefix)swipl7action$(docker_image_tag) . &&\
	popd

# make push -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1
push:
	@echo "push to Docker Hub"
	docker push  $(docker_image_prefix)swipl7action$(docker_image_tag)

# make run -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1
run:
	@echo "run prolog action runner in docker for debugging"
	docker run -d \
	           -p 8080:8080 -v /tmp:/logs -v /tmp:/action \
	           $(docker_image_prefix)swipl7action$(docker_image_tag)

debug:
	@echo "run prolog action runner in local for debugging"
	swipl -q -l src/swipl7Action/swipl_runner.pl -g main -t halt
