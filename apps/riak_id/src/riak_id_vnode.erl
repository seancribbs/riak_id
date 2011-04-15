-module(riak_id_vnode).
-behaviour(riak_core_vnode).
-include("riak_id.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_exit/3]).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).


%% vnode callbacks
init([Index]) ->
    TS = erlang:now(),
    % This could get ugly if you expect them to be unique across data
    % centers, or if you have more than 1024 partitions
    <<MachineID:10/bits, _Rest/bits>> = <<Index:160/integer>>,
    {ok, #state{idx = Index, machine_id = MachineID, last_timestamp = TS}}.

handle_command(next_id, Sender, #state{last_timestamp = TS, sequence = Seq, machine_id = Machine} = State) ->
    case get_next_seq(TS, Seq) of        
        backwards_clock ->
            {reply, {fail, backwards_clock}, State};
        exhausted ->
            % Retry after a millisecond
            erlang:sleep(1),
            handle_command(next_id, Sender, State);              
        {ok, Time, NewSeq} ->            
            {reply, construct_id(Time, Machine, NewSeq), State#state{last_timestamp = Time, sequence = NewSeq}}
    end;
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_exit(_Pid,Reason,State) ->
    {stop, Reason, State}.

handle_handoff_command(_Message, _Sender, State) ->
    % Delay a little to naively avoid ID collisions
    erlang:sleep(1),
    {forward, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% Private functions
get_next_seq({Megas, Secs, Micros} = Time, Seq) ->
    Now = erlang:now(),
    {NowMegas, NowSecs, NowMicros} = Now,
    if
        % Time is essentially equal at the millisecond
        Megas =:= NowMegas,
        Secs =:= NowSecs,
        NowMicros div 1000 =:= Micros div 1000 ->            
            case (Seq + 1) rem 4096 of
                0 -> exhausted;
                NewSeq -> {ok, Now, NewSeq}
            end;
        % Woops, clock was moved backwards by NTP
        Now < Time ->
            backwards_clock;
        % New millisecond
        true ->
            {ok, Now, 0}
    end.

construct_id({Megas, Secs, Micros}, MachineID, Seq) ->
    Millis = Micros div 1000,
    Combined = (Megas * 1000000 + Secs) * 1000 + Millis,
    <<Integer:64/integer>> = <<0:1, Combined:41/integer-unsigned, MachineID:10/bits,
                               Seq:12/integer-unsigned>>,
    Integer.
    

