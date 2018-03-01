.PHONY: soumission

run:
	make -C src run

soumission:
	mkdir -p solutions
	mv src/solutions* solutions
	make -C src clean
	zip -r source.zip src

clean:
	make -C src clean
	rm -f source.zip
	find -name '*~' -delete
