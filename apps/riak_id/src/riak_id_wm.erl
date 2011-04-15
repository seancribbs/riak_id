-module(riak_id_wm).

-export([init/1,
         service_available/2,
         content_types_provided/2,
         to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
     {ok, riak_id:next_id()}.

service_available(RD, {fail, backwards_clock}=Ctx) ->
    {false,
     wrq:set_resp_header("Content-Type", "text/plain",
                         wrq:append_to_response_body("Clock moved backwards, please retry.",
                                                     RD)),
     Ctx};
service_available(RD, Ctx) when is_integer(Ctx) ->
    {true, RD, Ctx}. 

content_types_provided(RD, Ctx) ->
    {[{"text/plain", to_text}], RD, Ctx}.

to_text(RD, Ctx) ->
    {integer_to_list(Ctx), RD, Ctx}.
