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

# Environment (testing)
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
