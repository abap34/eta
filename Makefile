PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
EXECUTABLE = eta
ENTRY = main.scm
SCHEME ?= scheme
WRAPPER = eta.sh

install:
	@echo "Installing eta to $(BINDIR)/$(EXECUTABLE)"
	@mkdir -p $(BINDIR)
	@echo '#!/bin/sh' > $(WRAPPER)
	@echo '$(SCHEME) --script $(shell pwd)/$(ENTRY) "$$@"' >> $(WRAPPER)
	@chmod +x $(WRAPPER)
	@cp $(WRAPPER) $(BINDIR)/$(EXECUTABLE)
	@rm $(WRAPPER)

test:
	$(SCHEME) --script tests/run-tests.scm
	@echo "Tests completed."

clean:
	rm -f $(WRAPPER)
