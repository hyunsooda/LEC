# LEC
The LEC (LLVM-Based Extended Compiler for Security Improvement) tool checks for out-of-bounds access during compile time.
If such an access occurs at runtime, the instrumented program will crash and provide helpful debugging output, including variable name, index, array size, filename, and line number.
This project draws inspiration from Golang and Rust, which offer similar security features as part of their official compiler foundations.

# How to run
Navigate to the `tests/inputs` directory and run the command `./compile.sh`. The generated files will have a `*.ll` extension and can be found in the same directory.
To instrument the LLVM IR file `int.ll` with an out-of-bounds access checker and generate a new LLVM IR file named `output.ll`, run the following command: ```stack run -- lec -i tests/inputs/int.ll -o output.ll```.
To run the instrumented binary, execute the command ```lli output.ll```.
The output is as follows:
```console
Found out of bound access: ["int.c":10:20]:
         array length: 15, indexed by: 56
         variable name: "intarr", allocated at: 8
```
