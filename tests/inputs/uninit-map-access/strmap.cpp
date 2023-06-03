#include <iostream>
#include <map>

using namespace std;

int main() {
    map<string, float> thisismymap;
    thisismymap["123"] = 999.99;
    float v = thisismymap["123"];
    int key_cnt = thisismymap.count("456");

    int a = 123;
    if (a == 123) {
        a *= 2;
    } else {
        a *= 3;
    }

    return int(thisismymap["888"]);
}
