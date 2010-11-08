LL=$(wildcard *.ll)
BC=$(LL:.ll=.bc)

.SUFFIXES: .ll .bc

.ll.bc:
	llvm-as $<

all: $(BC)

calc: calc.bc $(BC)
	llvm-link -o calc.bc $(BC)

clean:
	rm -f *.bc