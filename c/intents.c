#include <stdio.h>

void setvar(int *a) {
    *a = 2;
}

void setvars(int *a, int n) {
    for (int i = 0; i < n; i++)
    {
        a[i] = i;
    }
}

struct Point3D
{
    float x;
    float y;
    float z;
};

struct Point3D Point3D(float x, float y, float z){
    struct Point3D p;
    p.x = x;
    p.y = y;
    p.z = z;
}


int main(int argc, char const *argv[])
{

    // equivalent definitions
    struct Point3D p1 = {1.0, 2.0, 3.0};
    struct Point3D p2 = Point3D(1.0, 2.0, 3.0);

    int arr[2];
    setvars(arr, 2);
    for (int i = 0; i < 2; i++)
    {
        printf("a[%i] = %i\n", i, arr[i]);
    }


    int a = 0;
    setvar(&a);
    printf("a = %i\n", a);
    return 0;
}


