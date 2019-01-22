TESTS = \
	experiments/010_empty_svg/main

test: $(addsuffix .run,$(TESTS))

experiments/%/main: experiments/%/prog.gpr
	@gprbuild -P $<

experiments/%/main.run: experiments/%/main
	@./experiments/$*/main > obj/$*.svg
	@xmllint --noout --dtdvalid experiments/svg11-flat-20110816.dtd obj/$*.svg
