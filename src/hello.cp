// TODO: support programs where we have references to integers
struct a {
    int x;
    int b;
};

int sum(ref int a, ref int b) {
    int c;
    c = *a + *b;
    return c;
} 

struct a get_x(struct a in_a) {
    return in_a;
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
