#include <iostream>
#include <map>

using namespace std;

map<string, float> thisismymap;

int main() {
    thisismymap["123"] = 999.99;
    float v = thisismymap["123"];

    int a = 123;
    if (a == 123) {
        a *= 2;
    } else {
        a *= 3;
    }

    return int(thisismymap["888"]);
}
