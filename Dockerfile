FROM erlang:26
WORKDIR /app
COPY . .
RUN rebar3 release
EXPOSE 8080
CMD ["_build/default/rel/simple_server/bin/simple_server", "foreground"]