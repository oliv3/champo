all: champo.beam capello.beam

# FIXME wtf hipe is not enabled on obiwan ?!
#OPTIMS=+native

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump