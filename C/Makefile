CC=gcc
CFLAGS=-Wall -g -pedantic -std=c99
LDLIBS=-lm

.SUFFIXES: .c .o .h

.PHONY: all clean

all: testing

run%: %
	./$< 

check%: %
	valgrind --leak-check=full ./$< 

testing: testing.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS)

clean:
	rm -f *.o
	rm -f testing
