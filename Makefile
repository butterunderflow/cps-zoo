SHELL := bash

TEST_FILES := $(wildcard *-test.rkt)

.phony: test
test: $(TEST_FILES)
	@echo "Running tests..."
		@$(foreach test, $(TEST_FILES), \
			if [[ "$(test)" == *"$(TEST)"* ]]; then \
				echo "Running $(test)"; racket $(test); \
			else \
				echo "Skipping $(test)"; \
			fi;)

