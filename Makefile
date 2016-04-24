case = *

all : build install
build : objdump

native: *.ml
	bapbuild -pkg cmdliner,re.pcre objsymbols.native

objdump : *.ml
		bapbuild -pkg cmdliner -pkg re.pcre  objdump.plugin

install: objdump
		bapbundle install objdump.plugin

test-expect:
		make -C ../test-expect

test : all test-expect
		@TEST_OPTIONS="${OPTS}" bap-test-expect $(TEST)


bap: objdump
		bap ${binary} ${OPTS}

clean:
	bapbuild -clean
