TESTS = \
	experiments/010_empty_svg/main \
	experiments/020_arc/main \
	experiments/021_circle/main \
	experiments/022_arc/main \
	experiments/025_text/main \
	experiments/026_text_along_path/main

test: $(addsuffix .run,$(TESTS))

experiments/%/main: experiments/%/prog.gpr experiments/%/*.ad? src/*.ad?
	@gprbuild -p -q -P $<

experiments/%/main.run: experiments/%/main
	@mkdir -p obj
	@./experiments/$*/main > obj/$*.svg
	@xmllint --noout --dtdvalid experiments/svg11-flat-20110816.dtd obj/$*.svg

clean:
	@rm -rf obj $(TESTS)
