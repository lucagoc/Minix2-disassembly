CC = gcc
CFLAGS = -Wall -Wextra -O2

all: disassembly

disassembly: src/main.c
	$(CC) $(CFLAGS) -o disassembly src/main.c

test: disassembly
	@echo "Running tests..."
	@failed=0; \
	for f in tests/asem/*.s tests/*.c; do \
		if [ ! -f "$$f" ]; then continue; fi; \
		echo "Testing $$f..."; \
		rm -f a.out; \
		/usr/local/core/bin/m2cc -.o "$$f" >/dev/null 2>&1; \
		if [ ! -f a.out ]; then \
			echo "  [FAIL to compile] $$f"; \
			failed=1; \
			continue; \
		fi; \
		mmvm -d a.out 2> expected.txt; \
		./disassembly a.out > actual.txt; \
		if diff -q expected.txt actual.txt > /dev/null; then \
			echo "  [OK] $$f"; \
		else \
			echo "  [FAIL] $$f"; \
			failed=1; \
		fi; \
		rm -f expected.txt actual.txt a.out; \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "All tests passed!"; \
	else \
		echo "Some tests failed!"; \
		exit 1; \
	fi

clean:
	rm -f disassembly a.out expected.txt actual.txt
