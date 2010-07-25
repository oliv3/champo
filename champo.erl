-module(champo).
-author('olivier@biniou.info').

-include("champo.hrl").

%% -define(USE_HINT, true). %% Use the "amon" word hint

%% XXX FIXME
%% [C] Alphabet: "amoneprtgskcli" => "amon me protege sekmet me garce en son temple oriental" <- Solution (103485299115558197372) (4)
%% deja pourquoi ca fait 4 au lieu de 2 ? (garce/garde (delta=1, +1 => 2))

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
%% modules "chrom" "config"
%%
%% Lors du display des stats/time, rajouter le timestamp / human readable
%% eg ala syslog
%%

-compile([export_all]). %% debug
-export([mate/4]).

-export([chrom/0, chrom/2]).

%% GA parameters
-define(POP_SIZE, 10000). %%16). %% 200). %%200000).

%% Mutations
-define(P_MUTATION, 10). %%1000). %% 1 chance sur 1000
-define(NB_MUTATIONS, 6).

%% CPU cooling pauses
-define(TOS, 5). %% 30). %% seconds
-define(TOM, ?TOS*1000).

-define(H_POP_SIZE, (?POP_SIZE bsr 1)).

-define(WORST(X), (X*25)).

-define(WORST_GUESS_EVER, (
	  ?WORST(4) +
	  ?WORST(2) +
	  ?WORST(7) +
	  ?WORST(6) +
	  ?WORST(2) +
	  ?WORST(5) +
	  ?WORST(2) +
	  ?WORST(3) +
	  ?WORST(6) +
	  ?WORST(8)
	 )).

-define(SERVER, ?MODULE).

-define(H1, "         1").
-define(H2, "12345678901234").
-define(HR, "--------------").

-define(TOP, 10). %% Top N display

-define(PUTSTR(X), io:format("~s~n", [X])).

-define(HINT, "noma"). %% "amon" reversed
-define(HINT_LENGTH, length(?HINT)).

worst() ->
    io:format("Worst guess ever: ~p~n", [?WORST_GUESS_EVER]).

new_chrom() ->
    spawn(?MODULE, chrom, []).
new_chrom(C) ->
    spawn(?MODULE, chrom, [C, undefined]).


%% for eprof
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
    Pids = [new_chrom() || _Counter <- lists:seq(1, ?POP_SIZE)],
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


match(?WORST_GUESS_EVER) ->
    "<- Solution ";
match(_) ->
    "".

flatten(List) ->
    flatten(List, "").
flatten([Last], Acc) ->
    Acc ++ Last;
flatten([Word|Words], Acc) ->
    flatten(Words, Acc ++ Word ++ " ").

%% TODO chrom:display(Pid)
display({Pid, Score}) ->
    S = self(),
    Ref = make_ref(),
    Pid ! {S, Ref, get},
    C = receive
	    {Ref, Alphabet} ->
		Alphabet
	end,
    io:format("[C] Alphabet: ~p => ~p ~s(~p) (~p)~n", [pp(C), flatten(capello:sentence(C)), match(Score), Score, ?WORST_GUESS_EVER-Score]).

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
    [Pid ! {Ref, evaluate} || Pid <- Pids],

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
    [display(T) || T <- Top],
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
    [LoserPid ! die || {LoserPid, _Score} <- Losers],

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
	    ?MODULE:loop(NewPids, Gen+1, Elapsed, NewNLoops);
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


%% version avec roulette
%%
%% TODO: spawner les process
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
    S = self(),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Parent1 ! {S, Ref1, get}, %% TODO Chrom1 = chrom:get(Pid)
    Parent2 ! {S, Ref2, get},
    Chrom1 = receive
		 {Ref1, C1} ->
		     C1
	     end,
    Chrom2 = receive
		 {Ref2, C2} ->
		     C2
	     end,
    Parents = {Chrom1, Chrom2},
    {Child1, Child2} = case MS rem 2 of
			   0 ->
			       xover1(Parents);
			   1 ->
			       xover2(Parents)
		       end,
    Pid1 = new_chrom(Child1),
    Pid2 = new_chrom(Child2),
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


chrom() ->
    C = create(),
    chrom(C, undefined).

chrom(C, Score) ->
    receive
	%% when needed
	{Pid, Ref, get} ->
	    Pid ! {Ref, C},
	    chrom(C, Score);

	{Ref, evaluate} when Score =:= undefined ->
	    S = capello:check(C),
	    %% TODO/WIP we don't return C anymore
	    %% ?SERVER ! {Ref, {self(), C, S}},
	    ?SERVER ! {Ref, {self(), S}},
	    chrom(C, S);

	{Ref, evaluate} ->
	    %% TODO/WIP we don't return C anymore
	    %% ?SERVER ! {Ref, {self(), C, Score}},
	    ?SERVER ! {Ref, {self(), Score}},
	    chrom(C, Score);

	{mutate, 0} ->
	    NewC = mut_reverse(C),
	    chrom(NewC, undefined);

	{mutate, 1} ->
	    NewC = mut_split_swap(C),
	    chrom(NewC, undefined);

	{mutate, 2} ->
	    NewC = mut_randomize_full(),
	    chrom(NewC, undefined);

	{mutate, 3} ->
	    NewC = mut_randomize_one(C),
	    chrom(NewC, undefined);

	{mutate, 4} ->
	    NewC = mut_swap_two_genes(C),
	    chrom(NewC, undefined);

	{mutate, 5} ->
	    NewC = mut_shift(C),
	    chrom(NewC, undefined);

	die ->
	    %% io:format("[i] ~p exiting~n", [self()]),
	    ok;

	_Other ->
	    io:format("[?] Oups got other message: ~p~n", [_Other])
    end.


%%
%% generate a random chromosome
%%
to_char(X) ->
    $a + (X rem 26).

create() ->
    %% random().
    %% full_random().
    Chrom = random(),

    %% FIXME si on utilise le hint on ne checkera qu'un seul
    %% mot de 3 lettres dont on connait deja 2 caracteres: "?on"
    %% solution: utiliser capello:three/1 si on n'utilise pas le hint
    case capello:three(Chrom) of
	true ->
	    Chrom;
	false ->
	    create()
    end.

-ifdef(USE_HINT).
random() ->
    random(?ALPHABET_SIZE-?HINT_LENGTH, ?HINT, 26-?HINT_LENGTH, lists:seq($a, $z) -- ?HINT).
-else.
random() ->
    random(?ALPHABET_SIZE, [], 26, lists:seq($a, $z)).
-endif.

random(0, Acc, _N, _S) ->
    list_to_tuple(lists:reverse(Acc));
random(Size, Acc, N, Chars) ->
    Pos = crypto:rand_uniform(0, N) + 1,
    Elem = lists:nth(Pos, Chars),
    NewChars = Chars -- [Elem],
    random(Size-1, [Elem|Acc], N-1, NewChars).

full_random() ->
    %% TODO un binary comprehension
    Bin = crypto:rand_bytes(?ALPHABET_SIZE),
    L1 = binary_to_list(Bin),
    L2 = [to_char(C) || C <- L1],
    list_to_tuple(L2).


t2b(X) ->
    list_to_binary(tuple_to_list(X)).

b2t(X) ->
    list_to_tuple(binary_to_list(X)).

%% pretty print a chromosome
pp(X) ->
    tuple_to_list(X).


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


test() ->
    {P1, P2} = T1 = {create(), create()},
    io:format("Parent1: ~p~n", [pp(P1)]),
    io:format("Parent2: ~p~n", [pp(P2)]),
    {C1, C2} = xover1(T1),
    io:format("Child1:  ~p~n", [pp(C1)]),
    io:format("Child2:  ~p~n", [pp(C2)]).


test2() ->
    {P1, P2} = T1 = {create(), create()},
    io:format("Parent1: ~p~n", [pp(P1)]),
    io:format("Parent2: ~p~n", [pp(P2)]),
    {C1, C2} = xover2(T1),
    io:format("Child1:  ~p~n", [pp(C1)]),
    io:format("Child2:  ~p~n", [pp(C2)]).

%%
%% Mutations
%%

%% -define(MUT(F,A), io:format(F, A)).
-define(MUT(F,A), io:format(".", [])).

%% 1. Reverse chromosome
mut_reverse(C) ->
    New = list_to_tuple(lists:reverse(tuple_to_list(C))),
    ?MUT("[m] Reverse chromosome: ~p -> ~p~n", [pp(C), pp(New)]),
    New.

%% 2. Split in two then swap
mut_split_swap(C) ->
    {Left, Right} = lists:split(?H_ALPHABET_SIZE, tuple_to_list(C)),
    New = list_to_tuple(Right ++ Left),
    ?MUT("[m] Split/Swap chromosome: ~p -> ~p~n", [pp(C), pp(New)]),
    New.

%% 3. Randomize full
mut_randomize_full() ->
    C = create(),
    ?MUT("[m] Randomize chromosome full: ~p~n", [pp(C)]),
    C.

%% 4. Randomize only one char
mut_randomize_one(C) ->
    Position = crypto:rand_uniform(0, ?ALPHABET_SIZE) + 1,
    <<NewChar>> = crypto:rand_bytes(1),
    Char = to_char(NewChar),
    New = setelement(Position, C, Char),
    ?MUT("[m] Randomize chromosome one at pos ~p: ~p -> ~p~n",
	 [Position, pp(C), pp(New)]),
    New.

%% 5. Swap two characters
mut_swap_two_genes(C) ->
    {Position1, Position2} = random2(),
    Char1 = element(Position1, C),
    Char2 = element(Position2, C),
    Tmp = setelement(Position1, C, Char2),
    New = setelement(Position2, Tmp, Char1),
    ?MUT("[m] Swap two genes at pos ~p/~p: ~p -> ~p~n",
	 [Position1, Position2, pp(C), pp(New)]),
    New.

%% 6. Shift a chromosome
mut_shift(C) ->
    [H|T] = tuple_to_list(C),
    New = list_to_tuple(T++[H]),
    ?MUT("[m] Shift chromosome: ~p -> ~p~n", [pp(C), pp(New)]),
    New.


test_mut_swap_two_genes() ->
    C = create(),
    N = mut_swap_two_genes(C),
    ?PUTSTR(?H1),
    ?PUTSTR(?H2),
    ?PUTSTR(pp(C)),
    ?PUTSTR(?HR),
    ?PUTSTR(pp(N)),
    ok.


split(String, Pos, Delim) ->
    {L, R} = lists:split(Pos, String),
    S = io_lib:format("~s~c~s", [L, Delim, R]),
    %% lists:flatten(S).
    S.



h1(Pos) ->
    io:format("~s~n", [split(?H1, Pos, $ )]).
h2(Pos) ->
    io:format("~s~n", [split(?H2, Pos, $|)]).
hl(Pos) ->
    io:format("~s~n", [split(?HR, Pos, $-)]).


%% TODO move to utils.erl

%%
%% take 2 random -different- integers in [1..?ALPHABET_SIZE]
%%
rnd_pos() ->
    crypto:rand_uniform(0, ?ALPHABET_SIZE) + 1.

random2() ->
    Rnd1 = rnd_pos(),
    random2(Rnd1).
random2(Rnd1) ->
    Rnd2 = rnd_pos(),
    if
	Rnd1 =:= Rnd2 ->
	    random2(Rnd1);
	true ->
	    {Rnd1, Rnd2}
    end.
