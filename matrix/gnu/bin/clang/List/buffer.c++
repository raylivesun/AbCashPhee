#if 0
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    char *input = "Hello, World!";
    char *output = malloc(strlen(input) + 1);

    if (output == NULL) {
        printf("Memory allocation failed.\n");
        return 1;
    }

    strcpy(output, input);
    printf("Reversed string: %s\n", output);

    free(output);

    return 0;
}
#endif
