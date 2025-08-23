EMACS ?= emacs
EMACS_BATCH = $(EMACS) --batch -Q
EL_SOURCES = euslisp-mode.el
TEST_SOURCES = test/euslisp-mode-test.el

# Package dependencies
DEPS = helm s exec-path-from-shell

.PHONY: all compile test clean lint install-deps

all: compile test

# Install dependencies from MELPA
install-deps:
	@echo "Installing dependencies..."
	@$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		$(foreach dep,$(DEPS),--eval "(unless (package-installed-p '$(dep)) (package-install '$(dep)))") \
		--eval "(message \"Dependencies installed\")"

# Byte compile the package
compile: install-deps
	@echo "Compiling euslisp-mode..."
	@$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $(EL_SOURCES)

# Run tests
test: install-deps
	@echo "Running tests..."
	@$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(add-to-list 'load-path \".\")" \
		--eval "(add-to-list 'load-path \"./test\")" \
		--eval "(require 'ert)" \
		--eval "(require 'euslisp-mode)" \
		--eval "(require 'euslisp-mode-test)" \
		-f ert-run-tests-batch-and-exit

# Check package format with package-lint
lint: install-deps
	@echo "Linting package..."
	@$(EMACS_BATCH) \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(EL_SOURCES)

# Check documentation
checkdoc:
	@echo "Checking documentation..."
	@$(EMACS_BATCH) \
		--eval "(checkdoc-file \"$(EL_SOURCES)\")"

# Clean compiled files
clean:
	@echo "Cleaning..."
	@rm -f *.elc
	@rm -f test/*.elc

# Local test - run tests interactively
local-test:
	@$(EMACS) -Q -l test-local.el

# Print version
version:
	@grep "^;; Version:" $(EL_SOURCES) | cut -d' ' -f3