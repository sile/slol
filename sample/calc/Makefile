LL=$(wildcard *.ll)
BC=$(LL:.ll=.bc)
INC=$(wildcard *.inc)

.SUFFIXES: .ll .bc

.ll.bc:
	m4 $< | llvm-as -o $@

all: $(BC) $(INC)
	llvm-link -o calc $(BC)

clean:
	rm -f *.bc