int sum(ref int a, ref int b) {
    int c;

    c = *a + *b;

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
