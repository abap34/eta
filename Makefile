PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
EXECUTABLE = eta
ENTRY = main.scm
SCHEME ?= racket
WRAPPER = eta.sh
TEST ?= 

install:
	@echo "Installing eta to $(BINDIR)/$(EXECUTABLE)"
	@mkdir -p $(BINDIR)
	@echo '#!/bin/sh' > $(WRAPPER)
	@echo '$(SCHEME)  $(shell pwd)/$(ENTRY) "$$@"' >> $(WRAPPER)
	@chmod +x $(WRAPPER)
	@cp $(WRAPPER) $(BINDIR)/$(EXECUTABLE)
	@rm $(WRAPPER)

test:
	$(SCHEME) tests/run-tests.scm $(TEST)
	@echo "Tests completed."

test-list:
	$(SCHEME) tests/run-tests.scm --help

clean:
	rm -f $(WRAPPER)
	rm -rf coverage-report