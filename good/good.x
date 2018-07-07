// Print statement
print(1);

// Arithmetic expressions
print(2+2);
print(2+2*2);
print((2+2)*2);

// Boolean expressions
print(1 < 2);
print((2 == 1) || (1 == 2));

// Variables
int a;
print(a); //Default value of undeclared int is 1
a = 2;
print(a);
int b = 3;
bool c = true;
a++;
print(a);
a *= 4;
print(a);

// Arrays
int i[5];
i[1] = 5-3;
print(i[1]); // Arrays of arrays unsupported yet

// if statements
if (a > 0) {
    print(a);
}
int d = 0; //0 -> true, int / {0} -> false
if (d) {
    print(123);
} else {
    print(321);
}

//While and for statements
while (d < 5) {
    d++;
    if (d == 3) {
        continue;
    }
}

for (d = 5; d > 1; d--) {
    if (d == 3) {
        break;
    }
}

//Functions with recursion and static binding and variable shadowing
int n = 3;
int factorial(int n) {
    if (n == 1) {
        return 1;
    } else {
        return n*(factorial(n-1));
    }
}
factorial(6);
