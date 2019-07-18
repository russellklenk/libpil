INSTALLDIR         = bin

LIB_OUTPUT         = libpil.a
LIB_HEADERS        = $(wildcard include/*.h)
LIB_SOURCES        = $(wildcard src/*.cc) $(wildcard src/linux/*.cc)
LIB_OBJECTS        = ${LIB_SOURCES:.cc=.o}
LIB_DEPENDENCIES   = ${LIB_SOURCES:.cc=.dep}
LIB_INCLUDE_DIRS   = -I. -Iinclude
LIB_WARNINGS       = -Wall -Wextra
LIB_CCFLAGS        = -std=c++11 -fstrict-aliasing -D__STDC_FORMAT_MACROS ${LIB_INCLUDE_DIRS} ${LIB_WARNINGS}

.PHONY: all lib clean install

all:: ${LIB_OUTPUT}

lib:: ${LIB_OUTPUT}

clean::
	rm -f *~ *.o *.dep src/*~ src/*.dep src/*.o src/linux/*~ src/linux/*.o src/linux/*.dep ${LIB_OUTPUT}

install:: ${LIB_OUTPUT}
	mkdir -p $(INSTALLDIR)
	cp -p $(LIB_OUTPUT) $(INSTALLDIR)

${LIB_OUTPUT}: ${LIB_OBJECTS}
	ar rcs $@ $^
	ranlib $@

${LIB_OBJECTS}: %.o: %.cc
	${CC} ${CCFLAGS} ${LIB_CCFLAGS} -o $@ -c $<

${LIB_DEPENDENCIES}: %.dep: %.cc Makefile
	${CC} ${CCFLAGS} ${LIB_CCFLAGS} -MM $< > $@

