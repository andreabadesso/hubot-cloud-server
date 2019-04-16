-module(http_handler).

-export([init/2, info/3]).

init(Req, State) ->
  #{headers := Headers,
    path := Path,
    method := Method} = Req,
  Data = #{
    headers => Headers,
    path => Path,
    method => Method,
    body => maps:get(body, Req, #{})
   },
  CentralId = maps:get(<<"central-id">>, Headers),
  %% HTTP Requests don't need an active WS Connection.
  %% We just need to auth the user JWT Token and check if
  %% the user has access to the central on the header central-id
  lager:info("Data ~p", [Data]),
  lager:info("ClientId: ~p", [CentralId]),
  case central_controller:http_send(CentralId, Data) of
    ok ->
      {cowboy_loop, Req, State, hibernate};
    central_not_found ->
      cowboy_req:reply(404, Req),
      {ok, Req, State}
  end.

info({http_ack, Body}, Req, State) ->
  #{<<"data">> := Data} = Body,
  #{<<"headers">> := Headers} = Data,
  Payload = jiffy:encode(Data),
  cowboy_req:reply(200, Headers, Payload, Req),
  {stop, Req, State};
info(_Msg, Req, State) ->
  {ok, Req, State, hibernate}.

