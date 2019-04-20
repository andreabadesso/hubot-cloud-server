-module(http_handler).

-export([init/2, info/3]).

init(Req, State) ->
  #{headers := Headers,
    path := Path,
    method := Method} = Req,

  lager:info("Req ~p", [Req]),

  Body = case cowboy_req:has_body(Req) of
           true ->
             case cowboy_req:read_body(Req) of
               {ok, Data, _} ->
                 Data;
               _ ->
                 #{}
             end;
           false -> #{}
         end,
  lager:info("Got body ~p", [Body]),
  Payload = #{
    headers => Headers,
    path => Path,
    method => Method,
    body => Body
   },
  CentralId = maps:get(<<"central-id">>, Headers),
  %% HTTP Requests don't need an active WS Connection.
  %% We just need to auth the user JWT Token and check if
  %% the user has access to the central on the header central-id
  case central_controller:http_send(CentralId, Payload) of
    ok ->
      {cowboy_loop, Req, State, hibernate};
    central_not_found ->
      cowboy_req:reply(404, Req),
      {ok, Req, State}
  end.

info({http_ack, Payload}, Req, State) ->
  #{<<"data">> := Data, <<"uuid">> := Uuid} = Payload,
  lager:info("Received uuid: ~p", [Uuid]),
  Headers = maps:get(<<"headers">>, Data, #{}),
  Body = maps:get(<<"body">>, Data, <<"">>),
  Status = maps:get(<<"status">>, Data, 500),
  cowboy_req:reply(Status, Headers, Body, Req),
  {stop, Req, State};
info(_Msg, Req, State) ->
  {ok, Req, State, hibernate}.

