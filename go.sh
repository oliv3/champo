#!/bin/sh
reset
erl +K true +A 4 +P 500000 -s champo $@
