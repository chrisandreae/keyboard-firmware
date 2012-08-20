SRCDIR = src
OBJDIR = obj

SRCS = indexes.hs	 \
	   basictypes.hs \
	   blockir.hs	 \
	   errors.hs	 \
	   errorstate.hs \
	   main.hs		 \
	   output.hs	 \
	   parser.hs	 \
	   typedast.hs

kcompiler: $(SRCS)
	ghc -o $@ $^

%.k: %.kc kcompiler
	./kcompiler -o$@ $<

clean:
	rm *.o *.hi kcompiler
