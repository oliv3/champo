#!/bin/sh
reset
#erl -sname champo +K true +P 50000 -s champo $@
#erl -sname champo -s champo $@
erl -s champo $@
