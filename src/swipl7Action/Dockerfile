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

FROM swipl:7.5.15

RUN apt-get update -y && \ 
    apt-get install -y zip unzip \
                       procps curl iputils-ping iproute2 traceroute dnsutils \
                       less vim-tiny

EXPOSE 8080

RUN mkdir /prologAction /action /logs
ADD swipl_runner.pl /prologAction/

CMD ["/bin/bash", "-c", "cd prologAction && swipl -q -l swipl_runner.pl -g main -t halt"]
