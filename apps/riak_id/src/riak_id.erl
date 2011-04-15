-module(riak_id).
-include("riak_id.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         next_id/0
        ]).

%% Public API

% @doc Retrieves a k-sorted unique integer.
next_id() ->
    DocIdx = riak_core_util:chash_key({<<"next_id">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_id),
    [{IndexNode, _}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, next_id, riak_id_vnode_master).
