FC=ifort
LANG=C
FCFLAGS=-g
LDFLAGS=

OBJS=mod_brainfuck.o brainfuck.o

brainfuck: $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS)

%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $(@) $(<)

clean:
	rm -f *.o *.mod brainfuck