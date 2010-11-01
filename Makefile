
# Main Makefile

SMR_NODE=smr_master
#SMR_WORKER_NODES=

export INCLUDE_DIR=include
export SOURCE_DIR=src
export EBIN_DIR=ebin
TEST_DIR=test

# Override this with path to ej to use Erjang
export EJ=erl

# Needed by Erjang
export ERL_ROOT=$(ERL_TOP)

INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(TEST_DIR)/%.beam, $(TEST_SOURCES))

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
EJ_OPTS=-pa $(EBIN_DIR) -pa $(TEST_DIR) -sname $(SMR_NODE)

all: compile

compile: $(TARGETS)

compile_tests: $(TEST_TARGETS)

.PHONY: start_worker_nodes
start_worker_nodes:
	for node in $(SMR_WORKER_NODES) ; do \
	    echo "Starting node $$node" ; \
	    echo 'code:add_pathsa(["$(EBIN_DIR)"]).' | erl_call -sname $$node -s -e ; \
	    done

.PHONY: stop_worker_nodes
stop_worker_nodes:
	for node in $(SMR_WORKER_NODES) ; do \
	    echo "Stopping node $$node" ; \
	    erl_call -sname $$node -q ; \
	    done

run: $(TARGETS)
	$(MAKE) start_worker_nodes
	$(EJ) $(EJ_OPTS)
	$(MAKE) stop_worker_nodes

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

