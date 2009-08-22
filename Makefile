         dirs := lib
     programs := scheme
 test_scripts := selftest.sh

scheme_cfiles := main.c env.c eval.c expand.c io.c lib.c obj.c print.c	\
		 printf.c proc.c read.c roots.c scan.c test.c unicode.c	\
									\
                 mem.c mem_scalar.c mem_fixvec.c mem_mixvec.c		\
									\
                 obj_boolean.c obj_bytevector.c obj_character.c		\
                 obj_fixnum.c obj_string.c obj_symbol.c obj_pair.c	\
                 obj_procedure.c obj_frame.c obj_binding.c		\
                 obj_vector.c
  scheme_libs := librnrs
scheme_ldlibs := -Wl,--no-whole-archive -lreadline

     CPPFLAGS := -D_GNU_SOURCE -I. -I/usr/local/include
       CFLAGS := -g -Wall -Werror -Wno-format
      LDFLAGS := -Wl,--whole-archive
      libtype := static

default: all

unicode.o .unicode.d: ucd_data.h

ucd_data.h: gen_ucd_data.py unicode.h UnicodeData.txt
	python gen_ucd_data.py > $@
JUNK = ucd_data.h

include makefiles/project-root.make
