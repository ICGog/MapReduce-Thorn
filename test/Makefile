
# Testing Makefile

TEST_SOURCES=$(wildcard *.erl)
TEST_TARGETS=$(patsubst %.erl, %.beam, $(TEST_SOURCES))
INCLUDES=$(wildcard ../$(INCLUDE_DIR)/*.hrl)

ERLC_OPTS=-I ../$(INCLUDE_DIR) -o ./ -Wall -v +debug_info

ERL_OPTS=-pa ../$(EBIN_DIR) -pa ./ -sname $(SMR_NODE) -noshell
WORKER_ERL_OPTS=-pa ../$(EBIN_DIR) -pa ./

all: test

compile: $(TEST_TARGETS)

%.beam: %.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

test: $(TEST_TARGETS)
	mkdir -p $(WWW_DIR)
	mkdir -p $(LOG_DIR)
	WORKER_ERL_OPTS="$(WORKER_ERL_OPTS)" erl $(ERL_OPTS) -s smr_SUITE test -s erlang halt

clean:
	rm -rf $(WWW_DIR)
	rm -rf $(LOG_DIR)
	rm -f $(TEST_TARGETS)
