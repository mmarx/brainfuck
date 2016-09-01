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
	./brainfuck tests/hello.bf
	./brainfuck tests/issue-#1.bf
