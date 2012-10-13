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

keyc: $(SRCS)
	ghc -o $@ $^

%.k: %.kc keyc
	./keyc -o$@ $<

clean:
	rm -f *.o *.hi keyc
