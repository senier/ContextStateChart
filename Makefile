TESTS = \
	experiments/010_empty_svg/main \
	experiments/020_arc/main \
	experiments/021_circle/main \
	experiments/022_arc/main \
	experiments/025_text/main \
	experiments/026_text_along_path/main \
	experiments/027_labeled_circle/main \
	experiments/040_line/main \
	experiments/041_line_cartesian/main \
	experiments/050_arrow/main \
	experiments/060_connector/main \
	experiments/061_labeled_connector/main \
	experiments/062_labeled_connector_inner/main \
	experiments/070_annular_sector/main \
	experiments/071_labeled_annular_sector/main

test: $(addsuffix .run,$(TESTS))

experiments/%/main: experiments/%/prog.gpr experiments/%/*.ad? src/*.ad?
	@gprbuild -p -q -P $<

experiments/%/main.run: experiments/%/main
	@mkdir -p obj
	@echo "[Running] $*"
	@./experiments/$*/main > obj/$*.svg
	@xmllint --noout --dtdvalid experiments/svg11-flat-20110816.dtd obj/$*.svg

clean:
	@rm -rf obj $(TESTS)
