FROM erlang:21 as builder

WORKDIR /hubot-cloud-server
COPY . .

RUN rebar3 as prod tar

RUN mkdir -p /opt/rel
RUN tar -zxvf /hubot-cloud-server/_build/prod/rel/*/*.tar.gz -C /opt/rel

RUN ls -lh /opt/rel

# FROM ubuntu:16.04

# WORKDIR /opt/hubot-cloud-server

# ENV RELX_REPLACE_OS_VARS true

# COPY --from=builder /opt/rel /opt/hubot-cloud-server

# RUN ls -lh /opt/hubot-cloud-server/bin/

ENTRYPOINT ["/opt/rel/bin/server", "foreground"]
