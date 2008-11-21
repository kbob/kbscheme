         dirs := lib
     programs := scheme
 test_scripts := selftest.sh

scheme_cfiles := main.c bind.c eval.c io.c lib.c obj.c print.c proc.c \
                 read.c roots.c test.c \
                 \
                 mem.c mem_scalar.c mem_fixvec.c mem_mixvec.c \
                 \
                 obj_boolean.c obj_character.c obj_fixnum.c obj_string.c \
                 obj_symbol.c obj_pair.c obj_procedure.c obj_frame.c \
                 obj_binding.c
  scheme_libs := librnrs
scheme_ldlibs := -Wl,--no-whole-archive -lreadline

     CPPFLAGS := -D_GNU_SOURCE -I.
       CFLAGS := -g -Wall -Werror
      LDFLAGS := -Wl,--whole-archive
      libtype := static

default: all

include makefiles/project-root.make
