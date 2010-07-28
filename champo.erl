-module(champo).
-author('olivier@biniou.info').

-include("champo.hrl").

%% Module API
-export([start/0, start/1, stop/0]).

%% Internal exports
-export([mate/4]).

%% Profiling
-export([pid/0]).

%% Tests
-export([test_xover1/0, test_xover2/0]).

%% Nombre de solutions a ce pb: 26^14
%%
%% > math:pow(26,14).
%% 6.450997470329715e19
%%
%% ce qui fait... beaucoup :)
%%
%% => 141167095653376 si on prend l'indice "amon" (26^10).

%%
%% save/load d'une population (liste de chroms dans un binary term)
%%
%% module "config"
%%

%% GA parameters
-define(POP_SIZE, 1000). %%1000).

%% Mutations
-define(P_MUTATION, 10).

%% CPU cooling pauses
-define(TOS, 3). %% seconds
-define(TOM, ?TOS*1000).

-define(H_POP_SIZE, (?POP_SIZE bsr 1)).

-define(SERVER, ?MODULE).

-define(TOP, 20). %% Top N display


pid() ->
    whereis(?SERVER).

start() ->
    start(infinity).

start(NLoops) ->
    %% Start crypto application
    io:format("[+] Starting crypto application: ~p~n", [crypto:start()]),

    %% Start dictionnary module
    capello:start(),

    %% Create initial population
    Pids = [chrom:new() || _Counter <- lists:seq(1, ?POP_SIZE)],
    io:format("[+] ~p chromosomes created~n", [length(Pids)]),

    register(?SERVER, self()),
    io:format("[i] Main loop pid: ~p~n", [self()]),

    %% Start GA
    loop(Pids, 1, 0, NLoops).


stop() ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, stop},
    receive
	{Ref, stopped} ->
	    capello:stop()
    end.


ts() ->
    {_Date, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("~2B:~2.10.0B:~2.10.0B", [Hour, Min, Sec]).

receive_result(Ref) ->
    receive
	{Ref, R} ->
	    R
    end.

loop(Pids, Gen, RunTime, NLoops) ->
    Start = now(),

    %% Ask all chroms to evaluate
    Ref = make_ref(),
    Self = self(),
    [Pid ! {Self, Ref, evaluate} || Pid <- Pids],

    %% Receive evaluations
    Evaluations = [receive_result(Ref) || _Pid <- Pids],

    %% Inverse scores
    %% io:format("Evals: ~p~n", [Evaluations]),
    NegEvals = [neg_score(E) || E <- Evaluations],

    %% Sort by best score descending
    Results = lists:reverse(lists:keysort(2, NegEvals)),

    %% Display Top N
    Top = top(Results),
    io:format("~n[*] Generation: ~p, ~p individuals evaluated~n", [Gen, Gen*?POP_SIZE]),
    io:format("[i] ~p processes~n~n", [length(processes())]),
    io:format("[*] Top ~p:~n", [?TOP]),
    [chrom:display(T) || T <- Top],
    io:format("~n", []),

    case do_stop() of
	true ->
	    ok;
	false ->
	    %% Start again
	    restart(Gen, RunTime, NLoops, Results, Start)
    end.

receive_refs(Refs) ->
    %% io:format("Waiting for ~p refs (~p)~n", [length(Refs), Refs]),
    receive_refs(Refs, []).
receive_refs([], Pids) ->
    Pids;
receive_refs([Ref|Refs], Acc) ->
    receive
	{Ref, Pids} ->
	    receive_refs(Refs, [Pids | Acc])
    end.

restart(Gen, RunTime, NLoops, Results, Start) ->
    %% Divide poulation in two
    %% TODO ? garder 25% et regénérer 75% ?
    {Winners, Losers} = lists:split(?H_POP_SIZE, Results),

    %% Kill losers
    [chrom:delete(LoserPid) || {LoserPid, _Score} <- Losers],

    %% Compute score of all the winners
    SumScores = sum_scores(Winners),

    %% Create new population
    Refs = new_population(?H_POP_SIZE, Winners, SumScores),
    ChildrenPids = receive_refs(Refs),
    %% io:format("[i] ChildrenPids: ~p~n", [ChildrenPids]),
    WinnersPids = [Pid || {Pid, _Score} <- Winners],
    NewPids = lists:flatten([WinnersPids|ChildrenPids]),
    %% io:format("[i] NewPids: ~p~n", [NewPids]),

    %% Stats
    Now = now(),
    ElapsedR = (timer:now_diff(Now, Start)) / 1000000,
    Elapsed = RunTime + ElapsedR,
    io:format("~n[i] ~s, done in ~ps (~ps mean)~n", [ts(), ElapsedR, (Elapsed/Gen)]),

    %% Sleep for a while to cool the CPU
    timer:sleep(?TOM),

    NewNLoops = case NLoops of
		    infinity ->
			infinity;
		    N ->
			N -1
		end,
    if
	NewNLoops > 0 -> %% heureusement (infinity > 0) =:= true :)
	    loop(NewPids, Gen+1, Elapsed, NewNLoops);
	true ->
	    io:format("[i] Exiting at generation ~p~n", [Gen]),
	    [Pid ! die || Pid <- NewPids],
	    ok
    end.


do_stop() ->
    receive
	{Pid, Ref, stop} ->
	    Pid ! {Ref, stopped},
	    true
    after 0 ->
	    false
    end.


top(List) ->
    {L1, _} = lists:split(?TOP, List),
    L1.


neg_score({Pid, Score}) ->
    {Pid, ?WORST_GUESS_EVER-Score}.


%%
%% Create a new population
new_population(N, Population, MaxScore) ->
    new_population(N, Population, MaxScore, []).
new_population(0, _Population, _MaxScore, Acc) ->
    Acc;
new_population(N, Population, MaxScore, Acc) ->
    {Parent1, _S1} = roulette(Population, MaxScore, undefined),
    {Parent2, _S2} = roulette(Population, MaxScore, Parent1),
    Ref = make_ref(),
    V = self(),
    spawn(fun() -> (?MODULE):mate(V, Ref, Parent1, Parent2) end),
    new_population(N-2, Population, MaxScore, [Ref | Acc]).

mate(Pid, Ref, Parent1, Parent2) ->
    {_, _, MS} = now(),
    Chrom1 = chrom:get(Parent1),
    Chrom2 = chrom:get(Parent2),
    Parents = {Chrom1, Chrom2},
    {Child1, Child2} = case MS rem 2 of
			   0 ->
			       xover1(Parents);
			   1 ->
			       xover2(Parents)
		       end,
    Pid1 = chrom:new(Child1),
    Pid2 = chrom:new(Child2),
    maybe_mutate(Pid1),
    maybe_mutate(Pid2),
    Pid ! {Ref, [Pid1, Pid2]}.


roulette(Population, MaxScore, NotThisPid) ->
    Score = crypto:rand_uniform(0, MaxScore),
    {Pid, _S} = This = extract(Population, Score),
    if
	Pid =:= NotThisPid ->
	    roulette(Population, MaxScore, NotThisPid);
	true ->
	    This
    end.


%% f({Pid, _Score}) ->
%%     S = self(),
%%     Ref = make_ref(),
%%     Pid ! {S, Ref, get},
%%     Alphabet = receive
%% 		   {Ref, A} ->
%% 		       A
%% 	       end,
%%     pp(Alphabet).


extract(Population, Score) ->
    extract(Population, Score, 0).
extract([{_Pid, S} = Element | Population], Score, CurScore) ->
    NewScore = CurScore + S,
    if
	NewScore >= Score ->
	    %% io:format("[d] Found element ~p, score ~p -> ~p >= ~p~n~n", [f(Element), S, NewScore, Score]),
	    Element;
	true ->
	    %% io:format("[d] Skipp element ~p, score ~p -> ~p  < ~p~n",   [f(Element), S, NewScore, Score]),
	    extract(Population, Score, NewScore)
    end.


sum_scores(Results) ->
    %% io:format("sum_scores ~p~n", [Results]),
    lists:sum([Score || {_Pid, Score} <- Results]).


maybe_mutate(Pid) ->
    case crypto:rand_uniform(0, ?P_MUTATION) of
	0 ->
	    mutate(Pid);
	_Other ->
	    ok
    end.


mutate(Pid) ->
    Mutation = crypto:rand_uniform(0, ?NB_MUTATIONS),
    Pid ! {mutate, Mutation}.





t2b(X) ->
    list_to_binary(tuple_to_list(X)).

b2t(X) ->
    list_to_tuple(binary_to_list(X)).


%%
%% mix 2 chromosomes
%%
%% one-point cross-over
xover1({C1, C2}) ->
    Bin1 = t2b(C1),
    Bin2 = t2b(C2),
    Rnd = crypto:rand_uniform(1, ?ALPHABET_SIZE),
    %% io:format("xover1: pos= ~p~n", [Rnd]),
    %% h1(Rnd), h2(Rnd),
    %% io:format("~s~n", [split(pp(C1), Rnd, $|)]),
    %% io:format("~s~n", [split(pp(C2), Rnd, $|)]),
    {L1, R1} = erlang:split_binary(Bin1, Rnd),
    {L2, R2} = erlang:split_binary(Bin2, Rnd),
    NC1 = erlang:list_to_binary([L1, R2]),
    NC2 = erlang:list_to_binary([L2, R1]),
    Child1 = b2t(NC1),
    Child2 = b2t(NC2),
    {Child1, Child2}.

%% two-point cross-over
xover2({C1, C2}) ->
    LC1 = tuple_to_list(C1),
    LC2 = tuple_to_list(C2),

    First  = crypto:rand_uniform(2, ?ALPHABET_SIZE-2),
    Second = crypto:rand_uniform(First+1, ?ALPHABET_SIZE-1),

    %% io:format("xover2: ~p / ~p~n", [First, Second]),

    {Left1, Rest1} = lists:split(First, LC1),
    {Left2, Rest2} = lists:split(First, LC2),

    Delta = Second - First,

    {Middle1, Right1} = lists:split(Delta, Rest1),
    {Middle2, Right2} = lists:split(Delta, Rest2),

    Child1 = list_to_tuple(Left1 ++ Middle2 ++ Right1),
    Child2 = list_to_tuple(Left2 ++ Middle1 ++ Right2),

    {Child1, Child2}.


test_xover1() ->
    {P1, P2} = T1 = {chrom:new(), chrom:new()},
    io:format("Parent1: ~p~n", [?PP(P1)]),
    io:format("Parent2: ~p~n", [?PP(P2)]),
    {C1, C2} = xover1(T1),
    io:format("Child1:  ~p~n", [?PP(C1)]),
    io:format("Child2:  ~p~n", [?PP(C2)]).


test_xover2() ->
    {P1, P2} = T1 = {chrom:new(), chrom:new()},
    io:format("Parent1: ~p~n", [?PP(P1)]),
    io:format("Parent2: ~p~n", [?PP(P2)]),
    {C1, C2} = xover2(T1),
    io:format("Child1:  ~p~n", [?PP(C1)]),
    io:format("Child2:  ~p~n", [?PP(C2)]).


%% split(String, Pos, Delim) ->
%%     {L, R} = lists:split(Pos, String),
%%     S = io_lib:format("~s~c~s", [L, Delim, R]),
%%     %% lists:flatten(S).
%%     S.


%% h1(Pos) ->
%%     io:format("~s~n", [split(?H1, Pos, $ )]).
%% h2(Pos) ->
%%     io:format("~s~n", [split(?H2, Pos, $|)]).
%% hl(Pos) ->
%%     io:format("~s~n", [split(?HR, Pos, $-)]).
