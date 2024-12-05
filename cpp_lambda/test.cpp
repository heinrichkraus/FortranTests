#include <iostream>

int f(int a, int b) {
    return a + b;
}

int main(){

    int a;
    int b = 5;

    for (int i=0; i < 10; i++){
        auto fb = [b] (int a) {
            return f(a, b);
        };

        a = i+1;
        std::cout << a << "=>" << fb(a) << std::endl;
    }

    return 0;
}


