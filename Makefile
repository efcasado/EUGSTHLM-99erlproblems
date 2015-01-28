###========================================================================
### File: Makefile
###
### Simple Makefile solely meant to be used in the context of the EUG STHLM
### meetup.
###
### This Makefile has been deliberately left as simple as possible, so that
### EUG STHLM attendees can (easily) grasp the details of how to use `erlc`
### and `erl` applications to compile and test simple Erlang applications.
###
###
### Author: Enrique Fernandez <enrique.fernandez (at) erlang-solutions.com>
### Date:   January, 2015
###
###-- LICENSE -------------------------------------------------------------
### The MIT License (MIT)
###
### Copyright (c) 2015 Enrique Fernandez
###
### Permission is hereby granted, free of charge, to any person obtaining
### a copy of this software and associated documentation files (the
### "Software"), to deal in the Software without restriction, including
### without limitation the rights to use, copy, modify, merge, publish,
### distribute, sublicense, and/or sell copies of the Software,
### and to permit persons to whom the Software is furnished to do so,
### subject to the following conditions:
###
### The above copyright notice and this permission notice shall be included
### in all copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
### IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
### CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
### TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
### SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
###========================================================================
ERL       = $(shell which erl)
ERLC      = $(shell which erlc)
ERLC_OPTS = -o $(BIN_DIR) -DTEST
BIN_DIR   = ebin
SRC_DIR   = src
SRC_FILES = $(notdir $(shell find $(SRC_DIR) -type file -name *.erl))
BIN_FILES = $(patsubst %.erl,$(BIN_DIR)/%.beam,$(SRC_FILES))

VPATH     = $(SRC_DIR)

.PHONY: $(SRC_DIR)

## =========================================================================
##  Targets
## =========================================================================
all: compile

compile: $(BIN_DIR) $(BIN_FILES)

$(BIN_DIR):
	mkdir $(BIN_DIR)

ebin/%.beam: %.erl
	erlc $(ERLC_OPTS) $<

EUNIT_RUN = erl \
-noshell \
-pa $(BIN_DIR) \
-eval 'case eunit:test([{dir,"$(BIN_DIR)"}],[verbose]) of ok -> erlang:halt(0); _ -> erlang:halt(1) end.'

test: compile
	$(EUNIT_RUN)

clean:
	rm -r ebin/*
