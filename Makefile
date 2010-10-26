
# Main Makefile

export INCLUDE_DIR=include
export SOURCE_DIR=src
export EBIN_DIR=ebin
TEST_DIR=test

# Needed by Erjang
export ERL_ROOT=$(ERL_TOP)

INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(TEST_DIR)/%.beam, $(TEST_SOURCES))

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
EJ_OPTS=-pa $(EBIN_DIR) -pa $(TEST_DIR)

all: compile

compile: $(TARGETS)

compile_tests: $(TEST_TARGETS)

run: $(TARGETS)
	$(EJ) $(EJ_OPTS)

all_tests: $(TARGETS) $(TEST_TARGETS)
	$(MAKE) -C $(TEST_DIR) test

clean:
	rm -f $(TARGETS)
	$(MAKE) -C $(TEST_DIR) clean

##########################################################################
## Internal
##########################################################################

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(TEST_TARGETS): $(TEST_DIR)

.PHONY: $(TEST_DIR)
$(TEST_DIR):
	$(MAKE) -C $(TEST_DIR) compile

