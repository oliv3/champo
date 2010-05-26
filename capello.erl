-module(capello).
-author('olivier@biniou.info').

%% TODO inserer les mots dans une table ETS
%% ~1s / run pour 100 chroms en liste de strings

%%
%% Maitre Capello
%%
%% Le module qui charge le dictionnaire et se charge
%% de verifier que des mots sont dedans
%%
-include("champo.hrl").

-export([start/0, loop/1, stop/0]).
-export([check/1, two/1]).


-define(SERVER, ?MODULE).
-record(state, {words}).

%% The riddle
%% http://www.youtube.com/watch?v=5ehHOwmQRxU
-define(RIDDLE, [
		 [1, 2, 3, 4],
		 ?WORD2a,
		 [6, 7, 3, 8, 5, 9, 5],
		 [10, 5, 11, 2, 5, 8],
		 ?WORD2a,
		 [9, 1, 7, 12, 5],
		 ?WORD2b,
		 [10, 3, 4],
		 [8, 5, 2, 6, 13, 5],
		 [3, 7, 14, 5, 4, 8, 1, 13]
		]).
%% Therefore we set
-define(MAXWORDLENGTH, 8).


start() ->
    io:format("[+] Loading dictionary: ", []),
    Words = dict_load(),
    io:format("~p words~n", [length(Words)]),
    Pid = spawn(?SERVER, loop, [#state{words=Words}]),
    register(?SERVER, Pid),
    io:format("[i] ~p module started, pid ~p~n", [?SERVER, Pid]).


%% not used yet
stop() ->
    ?SERVER ! {self(), stop},
    receive
	ok ->
	    ok
    end.


two(Chrom) ->
    ?SERVER ! {self(), {two, Chrom}},
    receive
	Result ->
	    Result
    end.


check(Chrom) ->
    ?SERVER ! {self(), {check, Chrom}},
    receive
	Result ->
	    Result
    end.

%% ------------------------------------------------------------------

loop(#state{words=Words} = State) ->
    receive
	{Pid, {two, Chrom}} ->
	    Word1 = translate(?WORD2a, Chrom),
	    Word2 = translate(?WORD2b, Chrom),
	    In = lists:member(Word1, Words) andalso lists:member(Word2, Words),
	    Pid ! In,
	    loop(State);

	{Pid, {check, Chrom}} ->
	    Sentence = sentence(Chrom),
	    Score = check_sentence(Sentence, Words),
	    Pid ! Score,
	    loop(State);

	Msg ->
	    io:format("~p got message: ~p~n", [?SERVER, Msg]),
	    loop(State)
    end.

%% ------------------------------------------------------------------

%% chargement et parsing du dictionnaire
dict_load() ->
    dict_load("dico.txt").
dict_load(File) ->
    {ok, B} = file:read_file(File),
    L = binary_to_list(B),
    L2 = string:tokens(L, [10, 13]),
    menache(L2).

%% menache dans le dictionnaire, on ne garde
%% que les mots de taille <= ?MAXWORDLEN
menache(Words) ->
    [Str || Str <- Words, length(Str) =< ?MAXWORDLENGTH].

%% translate a word to french
translate(Word, Chrom) ->
    [element(Letter, Chrom) || Letter <- Word].

%% translate the whole riddle
sentence(Chrom) ->
    [translate(Word, Chrom) || Word <- ?RIDDLE].

%% diff entre 2 strings
%% ~= algo de Hamming
diff(Str1, Str2) when length(Str1) =:= length(Str2) ->
    diff(Str1, Str2, 0);
diff(_Truc1, _Truc2) ->
    undefined.

diff(Str, Str, Score) ->
    Score;
diff([], [], Score) ->
    Score;
diff([H1|T1], [H2|T2], Score) ->
    diff(T1, T2, Score + abs(H1-H2)).

%% Truc qui fait des calculs de distance d'un mot vs un dict
%%
%% > Dict.
%% ["abc","hello","world"]
%% > champo:find_in_dict("zprle", Dict).
%% {"world",5}
%% > champo:find_in_dict("prouta", Dict).
%% undefined
%%
%% FIXME could be better, partir avec BEAUCOUP = -1 et tester dessus
%% ou < si != -1
-define(BEAUCOUP, (($z-$a+1) * ?ALPHABET_SIZE)).

find_in_dict(String, Dict) ->
    find_in_dict(String, Dict, undefined, ?BEAUCOUP).

find_in_dict(_String, [], undefined, _BestSoFar) ->
    ?BEAUCOUP;
find_in_dict(_String, [], BestWord, BestSoFar) ->
    {BestWord, BestSoFar};
find_in_dict(String, [Word|Words], BestWord, BestSoFar) ->
    Score = diff(String, Word),
    case Score of
	0 ->
	    {Word, 0};
	S when S < BestSoFar ->
	    find_in_dict(String, Words, Word, S);
	_Other ->
	    find_in_dict(String, Words, BestWord, BestSoFar)
    end.

%% match a list of words vs a dict
%%
%% > Dict.
%% ["abc","hello","world"]
%% > Words = ["helli", "world", "absz"].
%% ["helli","world","absz"]
%% > champo:match(Words, Dict).         
%% [{"hello",6},{"world",0},undefined]
match(Words, Dict) ->
    [find_in_dict(Word, Dict) || Word <- Words].

%% Translate the riddle then returns the score
check_sentence(Sentence, Dict) ->
    %% NOTE S+1 pour multiplier des ints > 0,
    %% le score ideal est donc: 1
    Scores = [S+1 || {_Word, S} <- match(Sentence, Dict)],
    multiply(Scores).

multiply(Scores) ->
    lists:foldr(fun(Elem, Acc) -> Elem * Acc end, 1, Scores).
