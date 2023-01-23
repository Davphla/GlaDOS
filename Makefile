NAME		:= glados
BINARY_PATH := $(shell stack path --local-install-root)

all: build

build:
	@stack build
	@cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	@stack clean

fclean: clean
	@rm -f $(NAME)

debug:
	@stack repl

func_tests:
	@./scripts/functional_tests.bash

tests_run:
	@stack test

re: fclean all

.PHONY: all \
		build \
		clean \
		fclean \
		debug \
		tests_run \
		re
