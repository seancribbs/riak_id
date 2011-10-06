-module(riak_id).
-include("riak_id.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         next_id/0,
         ping/0
        ]).

%% Public API

next_id() ->
    command(next_id).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    command(ping).

command(Cmd) ->
    CmdBin = list_to_binary(atom_to_list(Cmd)),
    DocIdx = riak_core_util:chash_key({CmdBin, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_id),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, riak_id_vnode_master).
