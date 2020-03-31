##  Makefile

IDRIS := idris
LIB   := xml
OPTS  :=

.PHONY: clean lib clobber check test doc

install: lib
	${IDRIS} ${OPTS} --install ${LIB}.ipkg
	${IDRIS} ${OPTS} --install ${LIB}-eff.ipkg
lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

clobber : clean
	find . -name "*.ibc" -delete

check: clobber
	${IDRIS} --checkpkg ${LIB}.ipkg

test:
	${IDRIS} --testpkg ${LIB}-test.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg
