-module(capello).
-author('olivier@biniou.info').

%% ETS/ make_tuple(ets:new(), 8).
%% list_to_tuple([undefined | [ets:new(list_to_atom(integer_to_list(I)), [set, named_table]) || I <- lists:seq(2, 8)]]).
%% et en named table. du coup prefixage avec l'atom

%% TODO version intermediaire avec le dict, coder la longueur
%% avant ie {'7', [6, 7, 3, 8, 5, 9, 5]}

%%
%% Maitre Capello
%%
%% Le module qui charge le dictionnaire et se charge
%% de verifier que des mots sont dedans
%%
-include("champo.hrl").

-compile([export_all]).

-export([start/0, loop/0, stop/0]).
-export([check/1, sentence/1]).
-export([three/1]).

%% CHEAT
-export([solve/0]).

% ETS named tables
-define(ETS_3, three).
-define(ETS_WORDS, words).

-define(SERVER, ?MODULE).

%% The riddle
%% http://www.youtube.com/watch?v=5ehHOwmQRxU
-define(RIDDLE, [
		 [1, 2, 3, 4],
		 [2, 5],
		 [6, 7, 3, 8, 5, 9, 5],
		 [10, 5, 11, 2, 5, 8],
		 [2, 5],
		 [9, 1, 7, 12, 5],
		 [5, 4],
		 ?WORD3,
		 [8, 5, 2, 6, 13, 5],
		 [3, 7, 14, 5, 4, 8, 1, 13]
		]).
%% Therefore we set
-define(MAX_WORD_LENGTH, 8).


%%
%% for eprof:
%% taken from::
%% http://lserinol.blogspot.com/2009/01/profiling-performance-of-erlang.html
%%
%% 1> capello:pid().
%% <0.41.0>
%% 2> eprof:start_profiling([capello:pid()]).
%% eprof: Starting profiling .....
%% profiling
%%

%% ==========================================================================
%% FIRST RESULTS
%%
%% 4> eprof:stop_profiling()
%%
%% 5> eprof:analyze().
%% FUNCTION                                       CALLS      TIME
%% capello:find_best_match/4                      24843333   50 %
%% capello:diff/3                                 23161712   23 %
%% capello:diff/2                                 24841792   22 %
%% capello:'-check_sentence/2-lc$^0/1-0-'/1       2609659    5 %
%% ==========================================================================
%%
pid() ->
    whereis(?SERVER).


start() ->
    io:format("[+] Loading dictionary: ", []),
    dict_load(),
    io:format("~p words (~p trigrams)~n", [ets:info(?ETS_WORDS, size), ets:info(?ETS_3, size)]),
    Pid = spawn(?SERVER, loop, []),
    register(?SERVER, Pid),
    io:format("[i] Dictionary module started, pid ~p~n", [Pid]).


stop() ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, stop},
    receive
	{Ref, stopped} ->
	    ok
    end.


three(Chrom) ->
    TWord = translate(?WORD3, Chrom),
    ets:lookup(?ETS_3, TWord) =/= [].


check(Chrom) ->
    Sentence = sentence(Chrom),
    %% io:format("Checking sentence: ~p~n", [Sentence]),
    check_sentence(Sentence).

%% ------------------------------------------------------------------

loop() ->
    receive
	{Pid, Ref, stop} ->
	    Pid ! {Ref, stopped};

	Msg ->
	    io:format("~p got message: ~p~n", [?SERVER, Msg]),
	    ?MODULE:loop()
    end.

%% ------------------------------------------------------------------

%% chargement et parsing du dictionnaire
dict_load() ->
    dict_load("dico.txt").
dict_load(File) ->
    {ok, B} = file:read_file(File),
    L = binary_to_list(B),
    L2 = string:tokens(L, [10, 13, $-, $', $ ]),
    L3 = menache(L2),
    build_ets(L3).

build_ets(Words) ->
    Tid = ets:new(words, [set, named_table]),
    Three = ets:new(three, [set, named_table]),
    insert(Words, Tid, Three).

insert([], _Tid, _Three) ->
    ok;
insert([Word|Words], Tid, Three) when length(Word) =:= 3->
    true = ets:insert(Tid, {Word}),
    true = ets:insert(Three, {Word}),
    insert(Words, Tid, Three);
insert([Word|Words], Tid, Three) ->
    true = ets:insert(Tid, {Word}),
    insert(Words, Tid, Three).

%% menache dans le dictionnaire, on ne garde
%% que les mots de taille >= 2 et <= ?MAX_WORD_LENGTH
menache(Words) ->
    [Str || Str <- Words, length(Str) >= 2 andalso length(Str) =< ?MAX_WORD_LENGTH].

%% translate a word to french
translate(Word, Chrom) ->
    [element(Letter, Chrom) || Letter <- Word].

%% translate the whole riddle
sentence(Chrom) ->
    [translate(Word, Chrom) || Word <- ?RIDDLE].

-define(BEAUCOUP, (($z-$a+1) * ?ALPHABET_SIZE)).

%% diff entre 2 strings
%% ~= algo de Hamming
%% diff(Str1, Str2) when length(Str1) =/= length(Str2) ->
%%     undefined;
diff(Str, Str) ->
    0;
diff(Str1, Str2) ->
    diff(Str1, Str2, 0).

%% diff(Str, Str, Score) ->
%%     Score;
diff([], [], Score) ->
    Score;
diff([H1|T1], [H2|T2], Score) ->
    diff(T1, T2, Score + abs(H1-H2)).

%% Truc qui fait des calculs de distance d'un mot vs un dict
find_best_match(String, Key) ->
    find_best_match(String, Key, ?BEAUCOUP).

find_best_match(_String, '$end_of_table', BestSoFar) ->
    BestSoFar;
find_best_match(String, Key, BestSoFar) when length(String) =/= length(Key) ->
    find_best_match(String, next(Key), BestSoFar);
find_best_match(String, Key, BestSoFar) ->
    Score = diff(String, Key),
    %% io:format("Score: ~p~n", [Score]),
    case Score of
	0 ->
	    0;
	S when S < BestSoFar ->
	    find_best_match(String, next(Key), S);
	_Other -> %% score superieur
	    find_best_match(String, next(Key), BestSoFar)
    end.

next(Key) ->
    ets:next(?ETS_WORDS, Key).

%% match a list of words vs a dictionary stored in an ETS table
match(Words, Key) ->
    [find_best_match(Word, Key) || Word <- Words].


%% Translate the riddle then return the score
check_sentence(Sentence) ->
    Key = ets:first(?ETS_WORDS),
    Scores = match(Sentence, Key),
    %% io:format("Scores: ~p -> ~p~n", [Scores, lists:sum(Scores)]),
    lists:sum(Scores).


%%
%% CHEATING code here
%%
-define(SOLUTION, list_to_tuple("amoneprtgskdli")).

riddle() ->
    ?RIDDLE.

solution() ->
    ?SOLUTION.

solve() ->
    Riddle = riddle(),
    Solution = solution(),
    io:format("Testing solution: ~p~nWith the riddle: ~p~n", [champo:pp(Solution), Riddle]),
    Decrypted = sentence(Solution),
    io:format("Decrypted: ~p~n", [Decrypted]),
    Key = ets:first(?ETS_WORDS),
    Matches = match(Decrypted, Key),
    io:format("Matching words: ~p~n", [Matches]),
    wip.
