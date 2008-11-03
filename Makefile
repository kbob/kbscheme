CPPFLAGS := -D_GNU_SOURCE -I.
CFLAGS := -g -Wall -Werror # -MD -MF %.d -MP -MT .%.d
CFILES := main.c bind.c eval.c io.c lib.c obj.c print.c proc.c read.c roots.c \
	 test.c \
	 mem.c mem_scalar.c mem_fixvec.c mem_mixvec.c \
	 obj_boolean.c obj_character.c obj_fixnum.c obj_string.c obj_symbol.c \
	 obj_pair.c obj_procedure.c obj_frame.c obj_binding.c \
	 lib/base.c lib/fixnum.c
OFILES := $(CFILES:.c=.o)
LIBS := -lreadline

DIRS := base
TARGETS := scheme

scheme:	$(OFILES)
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(OFILES) $(LIBS)

clean:
	rm -f .coverage *~ */*~ *.o */*.o \
	      *.gcov */*.gcov *.gcda */*.gcda *.gcno */*.gcno \
	      .*.d */.*.d */.*.d.[0-9]* \
	      a.out */a.out gmon.out core */core $(TARGETS)

.%.d %/.%.d: %.c
	@rm -f "$@"
	@$(CC) -M -MP -MT '$*.o $@' -MF $@ $(CPPFLAGS) $< || rm -f "$@"

-include $(join $(dir $(CFILES)), $(patsubst %.c, .%.d, $(notdir $(CFILES))))
