#include <stdio.h>
#include <stdlib.h>

int main() {
    char SIZE = 15;
    int v1 = 7;
    int v2 = 8;
    int* intarr = (int*)malloc(sizeof(int) * SIZE);
    intarr[4] = 123;
    printf("%d\n", intarr[v1 * v2]);
    exit(1);
}
