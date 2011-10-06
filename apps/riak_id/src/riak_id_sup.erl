-module(riak_id_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    webmachine_router:add_route({["id"], riak_id_wm, []}),
    VMaster = { riak_id_vnode_master,
                  {riak_core_vnode_master, start_link, [riak_id_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster]}}.
