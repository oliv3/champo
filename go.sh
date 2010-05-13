#!/bin/sh
reset
erl +K true +P 200000 -s champo $@
