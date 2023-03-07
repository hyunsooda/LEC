# LEC

The LEC (LLVM-Based Extended Compiler for Security Improvement) tool checks for out-of-bounds access during compile time.
If such an access occurs at runtime, the instrumented program will crash and provide helpful debugging output, including variable name, index, array size, filename, and line number.
This project draws inspiration from Golang and Rust, which offer similar security features as part of their official compiler foundations.
