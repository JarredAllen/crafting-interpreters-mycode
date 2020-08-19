CC = clang
CC_FLAGS = -Wall -Wextra -Wpedantic

main: main.o
	$(CC) -o clox main.c

main.o: main.c common.h
	$(CC) $(CC_FLAGS) -c main.c

