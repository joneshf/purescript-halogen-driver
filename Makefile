BOWER := npx bower
BOWER_FLAGS ?=
COMPILE_FLAGS ?= 
DEPENDENCIES := 'bower_components/purescript-*/src/**/*.purs'
NODE := node
NPM := npm
OUTPUT := output
PSA := npx psa
PSCID := npx pscid
PURS := npx purs
PURTY := npx purty
REPL_FLAGS ?=
SRC := src
TEST := test

SRCS := $(shell find $(SRC) -name '*.purs')
TESTS := $(shell find $(TEST) -name '*.purs')
SRC_OUTPUTS := $(patsubst $(SRC).%.purs,$(OUTPUT)/%/index.js,$(subst /,.,$(SRCS)))
TEST_OUTPUTS := $(patsubst $(TEST).%.purs,$(OUTPUT)/%/index.js,$(subst /,.,$(TESTS)))

define SRC_OUTPUT_RULE
$(patsubst $(SRC).%.purs,$(OUTPUT)/%/index.js,$(subst /,.,$(1))): $(1)
	$(PSA) compile $(COMPILE_FLAGS) $(DEPENDENCIES) $(SRCS)
endef

define TEST_OUTPUT_RULE
$(patsubst $(TEST).%.purs,$(OUTPUT)/%/index.js,$(subst /,.,$(1))): $(1) $(SRC_OUTPUTS)
	$(PSA) compile $(COMPILE_FLAGS) $(DEPENDENCIES) $(SRCS) $(TESTS)
endef

$(foreach source, $(SRCS), $(eval $(call SRC_OUTPUT_RULE, $(source))))

$(foreach test, $(TESTS), $(eval $(call TEST_OUTPUT_RULE, $(test))))

.DEFAULT_GOAL := build

$(OUTPUT):
	mkdir -p $@

$(OUTPUT)/test.js: | $(OUTPUT)
	echo "require('./Test.Main/index.js').main();" > $@

bower_components: bower.json node_modules
	$(BOWER) $(BOWER_FLAGS) install
	touch $@

.PHONY: build
build: bower_components $(SRC_OUTPUTS)

.PHONY: clean
clean:
	rm -rf \
	  .psc-ide-port \
	  .psci_modules \
	  bower_components \
	  node_modules \
	  output

.PHONY: format
format: node_modules
	find $(SRC) -name '*.purs' -exec $(PURTY) --write {} \;
	find $(TESTS) -name '*.purs' -exec $(PURTY) --write {} \;

node_modules: package.json
	$(NPM) install
	touch $@

.PHONY: repl
repl: bower_components
	$(PURS) repl $(REPL_FLAGS) $(DEPENDENCIES) $(SRCS)

.PHONY: test
test: $(OUTPUT)/test.js bower_components $(SRC_OUTPUTS) $(TEST_OUTPUTS)
	$(NODE) $<

.PHONY: variables
variables:
	$(info $$DEPENDENCIES is [$(DEPENDENCIES)])
	$(info $$SRC_OUTPUTS is [$(SRC_OUTPUTS)])
	$(info $$SRCS is [$(SRCS)])
	$(info $$TESTS is [$(TESTS)])

.PHONY: watch
watch:
	$(PSCID) --test
