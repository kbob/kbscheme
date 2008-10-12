CPPFLAGS = -D_GNU_SOURCE -I.
CFLAGS = -g -Wall -Werror
YFILES = yread.y
CFILES = main.c bind.c eval.c io.c lib.c mem.c obj.c \
	 obj_bool.c \
	 print.c proc.c read.c test.c \
	 lib/base.c lib/fixnum.c
OFILES = $(CFILES:.c=.o) $(YFILES:.y=.o)
LIBS = -lreadline

DIRS = base
TARGETS = scheme

scheme:	$(OFILES) Makefile
	$(CC) -o $@ $(LDFLAGS) $(OFILES) $(LIBS)

clean:
	rm -f .coverage *~ */*~ *.o .*.d */*.o */.*.d a.out */a.out \
	      core */core $(TARGETS)

.%.d: %.c
	@rm -f $@ && \
	    $(CC) -M $(CPPFLAGS) $< > $@.$$$$ && \
	    sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@ && \
	    rm -f $@.$$$$

-include $(patsubst %.c, .%.d, $(CFILES))
