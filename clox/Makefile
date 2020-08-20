CC=clang
CC_FLAGS=-Wall -Wextra -Wpedantic -g -Og

HEADERS=common.h chunk.h compiler.h debug.h lexer.h memory.h table.h object.h value.h vm.h
OBJ=chunk.o compiler.o debug.o lexer.o main.o memory.o table.o object.o value.o vm.o

main: $(OBJ)
	$(CC) -o clox $(OBJ)

%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c -o $@ $<

clean:
	rm *.o
	rm clox
