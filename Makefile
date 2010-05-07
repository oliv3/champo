all: champo.beam

%.beam: %.erl
	erlc +hype $<

clean:
	@rm -f *.beam *~ erl_crash.dump