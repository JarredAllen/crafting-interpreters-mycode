#include "common.h"
#include "chunk.h"
#include "compiler.h"
#include "lexer.h"

bool compile(const char* source, Chunk* chunk) {
    initLexer(source);
    int line = -1;
    for (;;) {
        Token token = scanToken();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("   | ");
        }
        printf(" %2d '%.*s'\n", token.type, token.length, token.start);

        if (token.type == TOKEN_EOF) {
            break;
        }
    }
    return true;
}
