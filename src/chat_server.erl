%% @author attack
%% @doc @todo Add description to chat_server.

-module(chat_server).
-behaviour(gen_server).

-export([start_link/0, get_users/0, send_msg/4, user/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(USER_TABLE, users).

-record(users, {node, name}).


start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, {}, []).

init({}) ->
	State = [],
	ets:new(?USER_TABLE, [ordered_set, {keypos,#?USER_TABLE.name}, named_table]),
    {ok, State}.

%% Public
get_users() ->
	gen_server:cast({global, ?SERVER}, {get_users, self()}).

user(_DestNode, DestClt) ->
	ets:select(?USER_TABLE,[{{'_', '$2', DestClt}, [], ['$_']}]).


send_msg(From, ToNode, ToClt, Msg) ->
	io:format("Message from ~p to ~p~n", [From, ToClt]),
	gen_server:call({ToClt, ToNode}, {message, From, Msg}).

%% Private
handle_call({connect, #users{node = DestNode, name = DestClt} = User}, _From, State) ->
	io:format("____~p from ~p connecting...~n", [DestClt, DestNode]),

	case ets:select(?USER_TABLE,[{{'_', '$2', DestClt}, [], ['$_']}]) of

		[{users, DestNode, DestClt}] ->	io:format("~p already exists~n", [DestClt]),
										{reply, {connected, DestClt}, State};

		_ -> 	ets:insert(?USER_TABLE, [User]),
				io:format("New connect: ~p~n", [DestClt]),
				{reply, {connected, DestClt}, State}
	end;

handle_call({send_msg, From, ToNode, ToClt, Msg}, _From, State) ->
    case ets:select(?USER_TABLE,[{{'_', '$2', ToClt}, [], ['$_']}]) of
		[{users, ToNode, ToClt}] ->	send_msg(From, ToNode, ToClt, Msg),
				{reply, ok, State};
		_ -> io:format("~p Not found~n", [ToClt]),
				{reply, {send_msg, From}, State}
	end;

handle_call({exit, Pid}, _From, State) ->
    {reply, {exit, Pid}, State};

handle_call(Request, _From, State) ->
    error_logger:warning_msg("Bad message: ~p~n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast({get_users, _From}, State) ->
	User = ets:tab2list(?USER_TABLE),
	io:format("List users: ~p~n", [User]),
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
