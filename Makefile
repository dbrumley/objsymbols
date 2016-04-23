OPTS = --symbolizer=ida ${options} --saluki-taint --propagate-taint --saluki-solve
case = *
TEST = tests/test${case}.c

all : build install
build : objsymbols

native: *.ml
	bapbuild -pkg cmdliner,re.pcre objsymbols.native

objsymbols : *.ml
		bapbuild -package cmdliner  objsymbols.plugin

install: objsymbols
		bapbundle install objsymbols.plugin

test-expect:
		make -C ../test-expect

test : all test-expect
		@TEST_OPTIONS="${OPTS}" bap-test-expect $(TEST)


bap: objsymbols
		bap ${binary} ${OPTS}

clean:
	bapbuild -clean
