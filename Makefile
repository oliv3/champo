all: champo.beam capello.beam

%.beam: %.erl
	erlc +native $<

clean:
	@rm -f *.beam *~ erl_crash.dump