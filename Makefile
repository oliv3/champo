all: champo.beam capello.beam chrom.beam

# FIXME wtf hipe is not enabled on obiwan ?!
#OPTIMS=+native +hipe
#OPTIMS=+hipe

%.beam: %.erl
	erlc $(OPTIMS) $<

clean:
	@rm -f *.beam *~ erl_crash.dump
