all: champo.beam capello.beam

#OPTIMS=+native +hipe

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump