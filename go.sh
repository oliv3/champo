#!/bin/sh
reset
erl +P 200000 -s champo $@
