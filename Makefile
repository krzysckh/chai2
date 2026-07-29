OL=ol-small
OLIBS=-i third-party/robusta

.SUFFIXES: .scm .c

all: chai
chai: chai.c
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)
.scm.c:
	$(OL) $(OLIBS) -o $@ $<
run:
	$(OL) $(OLIBS) -r chai.scm
