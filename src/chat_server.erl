%% @author attack
%% @doc @todo Add description to chat_server.

-module(chat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, get_user/0, send_msg/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, {}, []).

init({}) ->
	State = [],
    {ok, State}.

get_user() ->
	gen_server:cast({global, ?SERVER}, {get_user, self()}).

send_msg(From, ToNode, ToClt, Msg) ->
	io:format("Message from ~p to ~p~n", [From, ToClt]),
	gen_server:call({ToClt, ToNode}, {message, From, Msg}).

handle_call({connect, DestNode, DestClt}, _From, State) ->
	io:format("____~p from ~p connecting...~n", [DestClt, DestNode]),
	case lists:member(DestClt, State) of
		true ->	io:format("~p already exists~n", [DestClt]),
				{reply, {connected, DestClt}, State};
		false -> NewState = lists:merge(State, [DestClt]),
				io:format("New connect: ~p~n", [DestClt]),
				{reply, {connected, DestClt}, NewState}
	end;

handle_call({send_msg, From, ToNode, ToClt, Msg}, _From, State) ->
    case lists:member(ToClt, State) of
		true ->	send_msg(From, ToNode, ToClt, Msg),
				{reply, ok, State};
		false -> io:format("~p Not found~n", [ToClt]),
				{reply, {send_msg, From}, State}
	end;

handle_call({exit, Pid}, _From, State) ->
    {reply, {exit, Pid}, State};

handle_call(Request, _From, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast({get_user, _From}, State) ->
	io:format("List users: ~p~n", [State]),
	{noreply, State};

handle_cast(Msg, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Msg]),
    {noreply, State}.

%% Other gen_server callbacks
handle_info(Info, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
