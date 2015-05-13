-module(riak_id).

-include("riak_id.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([next_id/0]).

%%====================================================================
%% API
%%====================================================================

next_id() ->
    command(next_id).

%%====================================================================
%% Internal functions
%%====================================================================

command(Cmd) ->
    CmdBin = atom_to_binary(Cmd, utf8),
    DocIdx = riak_core_util:chash_key({CmdBin, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_id),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, riak_id_vnode_master).
