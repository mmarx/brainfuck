FC = gfortran
FCFLAGS = -g -std=f2003
LDFLAGS =

OBJS=mod_brainfuck.o brainfuck.o

brainfuck: $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(^)

%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $(@) $(<)

clean:
	rm -f *.o *.mod brainfuck

test: brainfuck
	./brainfuck hello.bf
