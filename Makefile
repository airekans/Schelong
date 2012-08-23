all:

test:
	@for f in test_*; \
	do \
		if [ -f $$f ]; then \
			echo "running $$f"; \
			scheme --quiet --load $$f < /dev/null; \
			if [ $$? -eq 0 ]; then \
				echo "running $$f successfully"; \
			else \
				echo "running $$f failed"; \
			fi; \
			echo ; \
		fi \
	done
