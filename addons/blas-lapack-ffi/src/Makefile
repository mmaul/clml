UNAME := $(shell uname -m)
CC = gcc
CFLAGS = -O2 -shared -fPIC -fomit-frame-pointer -Wall \
	 -lmkl_gf_lp64 -lmkl_gnu_thread\
         -lmkl_core -lm -lgomp -lgfortran -lpthread
IN_FILE = lapack_wrapper.c
OUT_FILE = lapack_wrapper.so


all: dispatch

dispatch:
ifeq ($(UNAME), x86_64)
	$(CC) $(CFLAGS) -L$(MKLROOT)/lib/em64t $(IN_FILE) -o $(OUT_FILE)
endif
ifeq ($(UNAME), x86)
	$(CC) $(CFLAGS) -L$(MKLROOT)/lib/32 $(IN_FILE) -o $(OUT_FILE)
endif


clean:
	rm *.so
	rm *.fasl
	rm *.ufasl
	rm *.64ufasl
