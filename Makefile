
# Main Makefile

export SMR_NODE=smr_master
SMR_WORKER_NODES=w1 w2 w3 w4 w5
SMR_TEST_WORKER_NODES=

export INCLUDE_DIR=include
export SOURCE_DIR=src
export EBIN_DIR=ebin
TEST_DIR=test
export LOG_DIR=log
export WWW_DIR=www
LIB_DIR=lib

WWW_ERROR_LOG=$(WWW_DIR)/log/smr.log

JINTERFACE_JAR=$(ERL_TOP)/lib/jinterface-?.?.?/priv/OtpErlang.jar

THORNROOT=lib/thorn-interp-05
THORNJARS="$(THORNROOT)/classes/fisher.jar:$(THORNROOT)/classes/junit.jar:$(JINTERFACE_JAR)"
TH=java -classpath $(THORNJARS) fisher.run.Thorn
THREPL=java -classpath $(THORNJARS) fisher.run.REPL

ADDITIONAL_ERL_SOURCES=$(LIB_DIR)/rfc4627/rfc4627.erl
ADDITIONAL_ERL_TARGETS=$(EBIN_DIR)/rfc4627.beam

INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES)) $(ADDITIONAL_ERL_TARGETS)
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(TEST_DIR)/%.beam, $(TEST_SOURCES))

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
ERL_OPTS=-pa $(EBIN_DIR) -pa $(TEST_DIR) -sname $(SMR_NODE) -eval "ok = error_logger:logfile({open, \"$(WWW_ERROR_LOG)\"})." -async_shell_start -s smr
WORKER_ERL_OPTS=-pa $(EBIN_DIR) -pa $(TEST_DIR)

all: compile

compile: $(TARGETS)

compile_tests: $(TEST_TARGETS)

run: $(TARGETS)
	$(MAKE) start_worker_nodes
	mkdir -p $(LOG_DIR)
	SMR_WORKER_NODES="$(SMR_WORKER_NODES)" WORKER_ERL_OPTS="$(WORKER_ERL_OPTS)" TH="$(TH)" erl $(ERL_OPTS)
	$(MAKE) stop_worker_nodes

run_th:
	$(TH) $(PARAMS)

run_threpl:
	$(THREPL) $(PARAMS)

all_tests: $(TARGETS) $(TEST_TARGETS)
	$(MAKE) SMR_WORKER_NODES="$(SMR_TEST_WORKER_NODES)" start_worker_nodes
	$(MAKE) -C $(TEST_DIR) test
	$(MAKE) SMR_WORKER_NODES="$(SMR_TEST_WORKER_NODES)" stop_worker_nodes

clean:
	rm -f $(WWW_ERROR_LOG)
	rm -f $(TARGETS)
	rm -rf $(LOG_DIR)
	$(MAKE) -C $(TEST_DIR) clean

.PHONY: start_worker_nodes
start_worker_nodes: $(TARGETS)
	for node in $(SMR_WORKER_NODES) ; do \
	    echo ; \
	    echo "Starting node $$node" ; \
	    echo 'code:add_pathsa(["$(realpath $(EBIN_DIR))"]), code:add_pathsa(["$(realpath $(TEST_DIR))"]).' | TH="$(TH)" erl_call -sname $$node -s -e ; \
	    done

.PHONY: stop_worker_nodes
stop_worker_nodes:
	for node in $(SMR_WORKER_NODES) ; do \
	    echo "Stopping node $$node" ; \
	    erl_call -sname $$node -q ; \
	    done

##########################################################################
## Internal
##########################################################################

$(ADDITIONAL_ERL_TARGETS): $(ADDITIONAL_ERL_SOURCES)
	erlc $(ERLC_OPTS) $<

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(TEST_TARGETS): $(TEST_DIR)

.PHONY: $(TEST_DIR)
$(TEST_DIR):
	$(MAKE) -C $(TEST_DIR) compile
