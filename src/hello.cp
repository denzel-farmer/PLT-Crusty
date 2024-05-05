// TODO: support programs where we have references to integers
int sum(int a, int b) {
    int c;
    c = 3;
   //c = *a + *b;
    return c;
} 

int main() {
    int a;
    int b;
    int c;

    a = 1;
    b = 2;

    c = sum(&a, &b);
    return c;
} 
