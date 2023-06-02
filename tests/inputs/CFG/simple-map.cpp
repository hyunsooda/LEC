#include <iostream>
#include <map>

/* using namespace std; */

int main() {
    std::map<int, int> mymap;
    mymap[123] = 999;
    printf("%d\n", mymap[555]);
    printf("%d\n", mymap[123]);
    printf("%d\n", mymap.count(456));
}
