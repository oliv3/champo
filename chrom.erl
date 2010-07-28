-module(chrom).
-author('olivier@biniou.info').

-include("champo.hrl").

-export([new/0, new/1, loop/2, delete/1]).
-export([get/1, display/1]).

-define(USE_HINT, true). %% Use the "amon" word hint
-define(HINT, "noma"). %% "amon" reversed
-define(HINT_LENGTH, length(?HINT)).

-define(PUTSTR(X), io:format("~s~n", [X])).


new() ->
    Alpha = create(),
    new(Alpha).


new(Alpha) ->
    spawn_link(?MODULE, loop, [Alpha, undefined]).


delete(Pid) ->
    Pid ! die.


get(Pid) ->
    %% TODO noneed Ref
    S = self(),
    %% Ref = make_ref(),
    %% Pid ! {S, Ref, get},
    Pid ! {S, get},
    receive
	%% {Ref, Alpha} ->
	{value, Alpha} ->
	    Alpha
    end.


display({Pid, Score}) ->
    Alpha = ?MODULE:get(Pid),
    io:format("[C] Alphabet: ~p => ~p ~s(~p) (~p)~n",
	      [?PP(Alpha), flatten(capello:sentence(Alpha)), match(Score), Score, ?WORST_GUESS_EVER-Score]).


loop(Alpha, Score) ->
    receive
	%% {Pid, Ref, get} ->
	{Pid, get} ->
	    %% Pid ! {Ref, Alpha},
	    Pid ! {value, Alpha},
	    loop(Alpha, Score);

	{Pid, Ref, evaluate} when Score =:= undefined ->
	    S = capello:check(Alpha),
	    Pid ! {Ref, {self(), S}},
	    loop(Alpha, S);

	{Pid, Ref, evaluate} ->
	    Pid ! {Ref, {self(), Score}},
	    loop(Alpha, Score);

	{mutate, 0} ->
	    NewAlpha = mut_reverse(Alpha),
	    loop(NewAlpha, undefined);

	{mutate, 1} ->
	    NewAlpha = mut_split_swap(Alpha),
	    loop(NewAlpha, undefined);

	{mutate, 2} ->
	    NewAlpha = mut_randomize_full(),
	    loop(NewAlpha, undefined);

	{mutate, 3} ->
	    NewAlpha = mut_randomize_one(Alpha),
	    loop(NewAlpha, undefined);

	{mutate, 4} ->
	    NewAlpha = mut_swap_two_genes(Alpha),
	    loop(NewAlpha, undefined);

	{mutate, 5} ->
	    NewAlpha = mut_shift(Alpha),
	    loop(NewAlpha, undefined);

	die ->
	    %% io:format("[i] ~p exiting~n", [self()]),
	    ok;

	Other ->
	    io:format("[?] Oups ~p got unexpected message: ~p~n", [self(), Other]),
	    exit({unexpected, Other})
    end.


%% -------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------
to_char(X) ->
    $a + (X rem 26).


full_random() ->
    %% TODO un binary comprehension
    Bin = crypto:rand_bytes(?ALPHABET_SIZE),
    L1 = binary_to_list(Bin),
    L2 = [to_char(C) || C <- L1],
    list_to_tuple(L2).


%%
%% generate a random chromosome
%%
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


%% Tests
test_mut_swap_two_genes() ->
    C = create(),
    N = mut_swap_two_genes(C),
    ?PUTSTR(?H1),
    ?PUTSTR(?H2),
    ?PUTSTR(?PP(C)),
    ?PUTSTR(?HR),
    ?PUTSTR(?PP(N)),
    ok.


%% display
flatten(List) ->
    flatten(List, "").
flatten([Last], Acc) ->
    Acc ++ Last;
flatten([Word|Words], Acc) ->
    flatten(Words, Acc ++ Word ++ " ").

match(?WORST_GUESS_EVER) ->
    "<- Solution ";
match(_) ->
    "".
