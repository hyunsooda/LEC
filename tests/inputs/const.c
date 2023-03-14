#include <stdio.h>
#include <stdlib.h>

int main() {
    const char SIZE = 5;
    int* myarr = (int*)malloc(sizeof(int) * SIZE);
    myarr[4] = 123;
    printf("%d\n", myarr[7]);
    exit(1);
}
