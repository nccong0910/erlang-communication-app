%% @author attack
%% @doc @todo Add description to chat_client2.

-module(chat_client2).
-behaviour(gen_server).
-export([start_link/0, connect/1, send_msg/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []),
    io:format("start_link: ~p~n", [Return]),
    Return.

init({}) ->
    State = [],
    Return = {ok, State},
    io:format("init: ~p~n", [State]),
    Return.

% ===================================================================
connect(Node) ->
    Status = net_adm:ping(Node),
    io:format("Status: ~p~n", [Status]),
    case Status of
        pong -> io:format("Connecting...~n"),
            io:format("node() ~p, self() ~p~n", [node(), self()]),
            try
                net_adm:ping(Node),
                gen_server:call({global, chat_server}, {connect, node(), ?MODULE})
            catch _:_ ->
                gen_server:cast(?MODULE, {noconnect, Node})
            end;
        pang -> gen_server:cast(?MODULE, {noconnect, Node})
    end.

send_msg(ToNode, ToClient, Msg) ->
    gen_server:call({global, chat_server}, {send_msg, ?MODULE, ToNode, ToClient, Msg}).

% ===================================================================
handle_call({message, From, Msg}, _From, State) ->
    io:format("~p: ~p~n", [From, Msg]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    Return = {reply, Reply, State},
    io:format("handle_call: ~p~n", [Return]),
    Return.

handle_cast({noconnect, Node}, State) ->
    Return = {noreply, State},
    io:format("handle_cast: Cannot connect to ~p~n", [Node]),
    Return;

handle_cast({send_msg, Msg}, State) ->
    Return = {noreply, State},
    io:format("Message: ~p~n", [Msg]),
    Return;

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(_Info, State) ->
    Return = {noreply, State},
    io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    Return = ok,
    io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.

