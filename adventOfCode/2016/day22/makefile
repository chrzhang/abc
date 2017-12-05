# Compiler of choice, g++ -v should be at least 4.9 for regexes
CC=g++

# Flags for the compiler
CFLAGS=-c -Wall -Wfatal-errors -g -ggdb -Ofast -pedantic-errors -std=c++11

# Flags for compilers that need invoke the linker (e.g. libraries)
LDFLAGS=

# Change filenames accordingly per project
SOURCES=$(wildcard *.cpp)
EXECUTABLE=p

OBJECTS=$(SOURCES:.cpp=.o)

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.cpp.o:
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf *.o $(EXECUTABLE)
