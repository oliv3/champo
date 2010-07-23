#!/bin/sh
reset
erl +A 4 -sname champo +K true +P 2000000 -s champo $@
