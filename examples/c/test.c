struct Point {
    int x;
    int y;
};

int main() {
    struct Point p = {1, 2};
    return 0;
}
// compile to llvm ir
// clang -c -S -emit-llvm swap.c -o swap.ll

// ...without optimisations
// clang -c -S -Xclang -disable-O0-optnone -emit-llvm test.c -o test.ll

// ...keep variable names
// clang -c -S -fno-discard-value-names -Xclang -disable-O0-optnone -emit-llvm swap.c -o swap.ll

// ...convert to more compact SSA form
// opt -S -mem2reg swap.ll -o swap.ll