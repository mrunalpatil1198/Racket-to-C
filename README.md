# Racket-to-C

This project is aimed at transforming a Racket-based interpreter into C code by following certain steps.

## Conversion Steps

The transformation of the Racket interpreter to C code involves several key steps, each represented by a corresponding file in the project:

* **Step 01:** Vanilla interpreter that handles basic operations.
* **Step 02 to Step 05:** CPSing the interpreter and making it representation independent with tagged lists data structure.
* **Step 06:** Adding closures and making them representation independent with tagged lists data structure.
* **Step 07 to Step10:** Adding constructors for continuations and making it representation independent with tagged lists data structure.
* **Step 11:** Preparing for pc2c.
* **Step 12:** Refractoring closure handling with union.
* **Step 13:** Refractoring environment handling with union.
* **Step 14:** Refractoring continuation handling with union.
* **Step 15:** Converting to ANF.
* **Step 16:** Registerizing the interpreter.
* **Step 17:** Refractoring to use define-label.
* **Step 18:** Adding label invocations and trampolining.
* **Step 19:** Removing the header and main invocation.

To start the conversion process, navigate to the respective step file and follow the instructions within.

## Running the Program

To initiate the conversion of the interpreter to C code, follow these steps:

1. Download and run pc2c.rkt file.
2. In the associated Racket REPL with no other files loaded, type
(pc2c "step19.pc" "rtoc.c" "rtoc.h").
3. Compile the generated c code - rtoc.c using gcc or any any suitable c-compiler.
4. Run the executable.

Note: Mac users need to add a "#include <time.h>" at the top of their generated .c file to avoid getting an error.

Once you have successfully completed these steps, you will have a C code version of the interpreter.


