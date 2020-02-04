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
    apt-get install -y zip unzip ca-certificates git make graphviz \
                       procps curl iputils-ping iproute2 traceroute dnsutils less \
                       vim-tiny tcpdump kafkacat socat net-tools gettext-base expect

RUN curl -L https://download.docker.com/linux/static/stable/x86_64/docker-17.12.1-ce.tgz | tar zxvf - --strip 1 -C /usr/local/bin docker/docker
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl && chmod 0755 kubectl && mv kubectl /usr/local/bin
RUN curl -L https://mirror.openshift.com/pub/openshift-v3/clients/3.9.14/linux/oc.tar.gz | tar zxvf - -C /usr/local/bin
RUN curl -L https://github.com/openshift/source-to-image/releases/download/v1.1.9a/source-to-image-v1.1.9a-40ad911d-linux-amd64.tar.gz | tar zxvf - -C /usr/local/bin ./s2i
RUN curl -L https://github.com/apache/incubator-openwhisk-cli/releases/download/latest/OpenWhisk_CLI-latest-linux-amd64.tgz | tar zxvf - -C /usr/local/bin

EXPOSE 8080

RUN mkdir /prologAction /action /logs
ADD swipl_runner.pl /prologAction/

CMD ["/bin/bash", "-c", "cd prologAction && swipl -q -l swipl_runner.pl -g main -t halt"]
