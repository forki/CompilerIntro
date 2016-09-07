- title : F# compiler
- description : Excursion into the F# compiler
- author : Steffen Forkmann
- theme : night
- transition : default

***

### Excursion into the F# compiler

Steffen Forkmann

***

### What is a compiler?

---

Source code 
 
   -> Compiler
   
-> Program / Error message

---

Source code 
 
   -> Lexer -> Parser -> Type checker => TAST
   
   -> Optimizer -> Code Generator
   
-> Program / Error message

***

### Lexer

* Reads source character by character and returns stream of tokens

---

![Lexer pattern match](images/Lexer.png)

---

## #1243

    let creditCardNumber = 1234_5678_9012_3456L
    let socialSecurityNumber = 999_99_9999L
    let pi = 3.14_15F
    let hexBytes = 0xFF_EC_DE_5E
    let hexWords = 0xCAFE_BABE
    let maxLong = 0x7fff_ffff_ffff_ffffL
    let nybbles = 0b0010_0101
    let bytes = 0b11010010_01101001_10010100_10010010

---

<img src="images/PR1243.png" alt="Underscores in number literals" width=700 >

***

### Parser

* Creates syntactic structure (parse tree) from a stream of tokens

---

### Euclidean algorithm

    while b != 0
        if a > b
            a := a − b
        else
            b := b − a
    return a

<img src="images/AST.svg.png" alt="AST from wikipedia" width=300 >