TESTS = \
	obj/tests.run \
	obj/010_empty_svg/document.svg \
	obj/020_arc/document.svg \
	obj/021_circle/document.svg \
	obj/022_arc/document.svg \
	obj/025_text/document.svg \
	obj/026_text_along_path/document.svg \
	obj/027_labeled_circle/document.svg \
	obj/040_line/document.svg \
	obj/041_line_cartesian/document.svg \
	obj/050_arrow/document.svg \
	obj/060_connector/document.svg \
	obj/061_labeled_connector/document.svg \
	obj/062_labeled_connector_inner/document.svg \
	obj/070_annular_sector/document.svg \
	obj/071_labeled_annular_sector/document.svg \
	obj/080_graph_from_weights/document.svg \
	obj/090_labeled_graph/document.svg \
	obj/100_annular_sector_ports/document.svg \

VERBOSE ?= @

test: $(TESTS)

obj/tests.run: obj/tests
	@echo "[Running] $@"
	$(VERBOSE)./$<

obj/tests: tests/tests.gpr tests/tests.adb tests/test_cases.adb tests/test_suite.adb src/*.ad?
	@echo "[Building] $@"
	$(VERBOSE)gprbuild -p -q -P $<

obj/%/main: tests/%/prog.gpr tests/%/*.ad? src/*.ad?
	@echo "[Building] $*"
	$(VERBOSE)gprbuild -XNAME=$* -p -q -P $<

obj/%/document.svg: obj/%/main
	$(VERBOSE)mkdir -p $(dir $@)
	@echo "[Running] $*"
	$(VERBOSE)./$< > $@.tmp
	$(VERBOSE)xmllint --noout --dtdvalid tests/svg11-flat-20110816.dtd $@.tmp
	$(VERBOSE)mv $@.tmp $@

clean:
	$(VERBOSE)rm -rf obj $(TESTS)

.PHONY: test
