OL=ol-small
OLIBS=-i third-party/robusta

.SUFFIXES: .scm .c

all: chai
chai: chai.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<
.scm.c:
	$(OL) $(OLIBS) -o $@ $<
run:
	$(OL) $(OLIBS) -r chai.scm
