CC=gcc
CC_FLAGS=-Wall -Wextra -Wpedantic -g -O2 -pg

ODIR=build

HEADERS=common.h chunk.h compiler.h debug.h lexer.h memory.h table.h object.h value.h vm.h
_OBJ=chunk.o compiler.o debug.o lexer.o main.o memory.o table.o object.o value.o vm.o
OBJ=$(patsubst %,$(ODIR)/%,$(_OBJ))

clox: $(OBJ)
	$(CC) $(CC_FLAGS) -o clox $(OBJ)

$(ODIR)/%.o: %.c $(HEADERS)
	$(CC) $(CC_FLAGS) -c -o $@ $<

clean:
	rm $(ODIR)/*.o
