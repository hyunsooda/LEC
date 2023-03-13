#include <stdio.h>
#include <stdlib.h>

int main() {
    char SIZE = 5;
    int* intarr = (int*)malloc(sizeof(int) * SIZE);
    intarr[4] = 123;
    printf("%d\n", intarr[7]);
    exit(1);
}
