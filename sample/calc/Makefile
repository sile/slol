LL=$(wildcard *.ll)
BC=$(LL:.ll=.bc)
INC=$(wildcard *.inc)

.SUFFIXES: .ll .bc

.ll.bc:
	m4 $< | llvm-as -o $@

all: $(BC) $(INC)

calc: calc.bc $(BC)
	llvm-link -o calc.bc $(BC)

clean:
	rm -f *.bc