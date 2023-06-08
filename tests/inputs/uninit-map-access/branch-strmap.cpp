#include <iostream>
#include <map>

using namespace std;

int main() {
    map<string, float> thisismymap;
    thisismymap["123"] = 999.99;
    float v = thisismymap["123"];

    int a = 123;
    if (a == 123) {
        printf("%d\n", thisismymap["888"]);
        a *= 2;
    } else {
        a *= 3;
    }
}
