all: champo.beam capello.beam

#OPTIMS=+native

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump