FROM erlang:21 as builder

WORKDIR /hubot-cloud-server
COPY . .

RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /hubot-cloud-server/_build/prod/rel/*/*.tar.gz -C /opt/rel

RUN ls -lh /opt/rel

ENTRYPOINT ["/opt/rel/bin/server", "foreground"]
