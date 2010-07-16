#!/bin/sh
reset
erl -sname champo -smp +A 16 +K true +P 50000 -s champo $@
