TESTS = \
	experiments/010_empty_svg/main \
	experiments/020_arc/main \
	experiments/021_circle/main

test: $(addsuffix .run,$(TESTS))

experiments/%/main: experiments/%/prog.gpr experiments/%/*.ad? src/*.ad?
	@gprbuild -P $<

experiments/%/main.run: experiments/%/main
	@./experiments/$*/main > obj/$*.svg
	@xmllint --noout --dtdvalid experiments/svg11-flat-20110816.dtd obj/$*.svg
