%% Three-letter word to make a chromosom viable or not
-define(WORD3, [10, 3, 4]).

%% Idonea's enigma parameters
-define(ALPHABET_SIZE, 14).
-define(H_ALPHABET_SIZE, (?ALPHABET_SIZE bsr 1)).

%% Misc
-define(H1, "         1").
-define(H2, "12345678901234").
-define(HR, "--------------").

%% pretty print a chromosome
-define(PP(X), tuple_to_list(X)).

%% # of available mutations
-define(NB_MUTATIONS, 6).

%% for fitness computation
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
