all:

test:
	@for f in test_*; \
	do \
		if [ -f $$f ]; then \
			echo "running $$f"; \
			scheme --quiet --load $$f < /dev/null; \
			echo "running $$f successfully"; \
			echo ; \
		fi \
	done
