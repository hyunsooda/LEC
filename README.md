# LEC
The LEC (LLVM-Based Extended Compiler for Security Improvement) tool checks for out-of-bounds access during compile time.
If such an access occurs at runtime, the instrumented program will crash and provide helpful debugging output, including variable name, index, array size, filename, and line number.
This project draws inspiration from Golang and Rust, which offer similar security features as part of their official compiler foundations.

# Currently supported checker lists
- OOB checker (Out-of-bound checker)
    - It effectively identifies out-of-bounds access during runtime, such as buffer overrun in a static array or dynamically allocated memory.
- Uninitialized map access checker
    - It accurately detects attempts to access uninitialized STL maps during runtime.

# What is different with Address Sanitizer?
1. Features
    - TBD
2. Performance
    - Address Sanitizer (ASAN) inspects violations at runtime and it is known consuming large resource (CPU and memory), while LEC builds its behavior at compile time with machine code. Therefore, LEC is much faster than ASAN.
3. Report Quality
    - Error localization with ASAN is extremely hard to figure out, whereas LEC provides another level of localization quality that assists developers in easily identifying them. Having explainable reporting is not only an important requirement but also crucial in finding bugs.

# How to run
Navigate to the `tests/inputs` directory and run the command `./compile.sh`. The generated files will have a `*.ll` extension and can be found in the same directory.
To instrument the LLVM IR file `int.ll` with an out-of-bounds access checker and generate a new LLVM IR file named `output.ll`, run the following command: ```./run.sh tests/inputs/int.ll output.ll```.
To run the instrumented binary, execute the command ```lli output.ll```.
The output is as follows:
```console
Found out of bound access: ["int.c":10:20]:
         array length: 15, indexed by: 56
         variable name: "intarr", allocated at: 8
```

# Process
To begin with, the target project is compiled with metadata information using the `-g` flag to generate LLVM IR.
Then, the LEC analyzes the resulting IR files and adds instrumentation to enhance security.
Finally, the new IR files are optimized and the metadata is stripped using the command `opt -O3 -strip-debug`.

# Environment
- Local: Run `stack test`
- Docker: Build a `Dockerfile` and runs `stack test` in the container
- Git Action: Defined at `.github/workflows/ci.yml`
I attempted to use nix-shell for the first time, but encountered an unknown error preventing the project from being compiled.
Currently, the Docker container is being used as a substitute for the nix-shell environment.
If you wish to contribute, please feel free to add a pull request for this solution. The script used is provided below:
```
with (import <nixpkgs> {});
(mkShell {
  buildInputs = [
    cabal-install
    stack
    llvmPackages_15.llvm
    ncurses
    gmp
    libxml2
  ];
})
```
