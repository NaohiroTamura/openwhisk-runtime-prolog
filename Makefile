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

docker_image_prefix =
docker_image_tag = :latest

unit_test_files := $(wildcard tests/swipl7Action/test_*.pl)

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

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 build
build:
	@echo "create runtime image"
	docker build -t $(docker_image_prefix)swipl7Action$(docker_image_tag)

# make -e docker_image_prefix=myprefix/ -e docker_image_tag=:0.1 run
run:
	@echo "run prolog action runner in docker"
	docker run -d \
	           -p 8080:8080 -v /tmp:/logs \
	           $(docker_image_prefix)swipl7Action$(docker_image_tag)

debug:
	@echo "run prolog action runner in local for debugging"
	swipl -q -l src/swipl7Action/swipl_runner.pl -g main -t halt
