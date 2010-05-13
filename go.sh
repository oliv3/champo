#!/bin/sh
reset
erl +K true +P 500000 -s champo $@
