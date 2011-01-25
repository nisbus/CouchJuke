%%%-------------------------------------------------------------------
%%% File    : couchjuke_queue.erl
%%% @author  : nisbus
%%% @doc 
%%% The couchjuke_queue is responsible for queueing up files that need
%%% to be processed so that no more than the specified concurrent processes
%%% are running at any given time. 
%%%
%%% Created : 23 Jan 2011 by nisbus
%%%-------------------------------------------------------------------
-module(couchjuke_queue).

-behaviour(gen_server).

%% API
-export([start_link/1, add/2, done/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, 
	{
	  pending = [],
	  running = [],
	  max = 0,
	  db
	}).

%%====================================================================
%% API
%%====================================================================
add(File, Db) ->
    gen_server:cast(?SERVER, {add, File,Db}).
done(File) ->
    gen_server:cast(?SERVER, {done, File}).
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(MaxConcurrent) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxConcurrent], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([MaxConcurrent]) ->
    {ok, #state{max = MaxConcurrent}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast([ok],State) ->
    io:format("ignored ok event~n"),
    {noreply, State};

handle_cast({done,File}, State) ->
    NewList = lists:delete(File,State#state.running),
    NewState = State#state{running = NewList},
    case count(NewState#state.pending) > 0 of
	true ->
	     F = hd(NewState#state.pending),
	    
	    _P = spawn_link(couch_juke_crawler, mp3, [{mp3,F},State#state.db]),
	    {noreply, NewState#state{running = [F|NewState#state.running], pending = lists:delete(F,NewState#state.pending)}};
	false ->
	    io:format("Done!!!~n"),
	    {noreply, NewState}
    end;

handle_cast({add, File, Db}, State) ->
    case count(State#state.running) of
	0 ->
	    _P = spawn_link(couch_juke_crawler, mp3, [{mp3, File},Db]),
	    NewState = State#state{running = [File|State#state.running], db = Db, pending = lists:delete(File, State#state.pending)},
	    {noreply, NewState};
	Count ->
	    case State#state.max == Count of
		true ->
		    NewState = State#state{pending = [File|State#state.pending], db = Db},
		   {noreply, NewState};
		false ->
		    spawn_link(couch_juke_crawler, mp3, [{mp3, File},Db]),
		    NewState = State#state{running = [File|State#state.running], db = Db},
		    {noreply, NewState}
	    end
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info('DOWN', State) ->
    io:format("Received Downd event ~n"),
    case count(State#state.pending) of
	0 ->
	    void;
	_Count ->
	    void
	    %% {File, _Db} = hd(State#state.pending),
	    %% P = spawn_link(couch_juke_crawler, mp3, [{mp3, File}]),
	    %% {noreply, State#state{running = lists:delete()}}
    end,
    {noreply, State};

handle_info(_Info, State) ->
    io:format("Unhandled event ~p~n",[_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("terminating~n"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
count([]) -> 0;
count([_|T]) -> 1 + count(T).

    
