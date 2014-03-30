include ../make.inc

DSLSRC = xerbla.o

all: double

double: $(DSLSRC)
	$(ARCH) $(ARCHFLAGS) -o xerbla.a $(DSLSRC)

$(DSLSRC): $(FRC)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o
	rm -f *.a

.f.o: 
	$(F77) -c $<
