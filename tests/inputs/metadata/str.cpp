#include <iostream>
#include <map>

using namespace std;

bool test123(map<string, float> &m, string key) {
    m.count("QWE123");
    auto it = m.lower_bound(key);
    auto b = it == m.end();
    auto comp = m.key_comp();
    return b || comp(key, (*it).first);
}

int main() {
    map<string, float> mymap;
    mymap.count("QWE");
    mymap["123"] = 999.999;
    printf("%d\n", mymap["456"]);
    printf("%d\n", mymap["123"]);
    printf("%d\n", test123(mymap, "888"));
}
