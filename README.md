
# 1. Introduction

Object Oriented Haskell, or OOH for short, is a fun programming language you can use to test your OOP skills. It fully supports runtime polymorphism (C++'s vtables), generic types, recursive data types (Trees,LinkedLists,etc), 1D-2D arrays, a Haskell-inspired case and many other things! 

## 1.1 Installation

Before doing the first step, check you have the following installed in your computer:


1. The Glasgow Haskell Compiler
2. Cabal (usually comes with GHC)
3. GNU Make

### 0. Clone the project

```bash
git clone https://github.com/edfullest/Object-Oriented-Haskell.git
```

### 1. Install dependencies

Assuming you already have GHC, Cabal and GNU Make, you can get started by just running the following command in the root folder:

```bash
make install_dependencies
```

All dependencies are libraries from [Hackage](https://hackage.haskell.org/)

### 2. Build from source

Just run
```bash
make
```

This will compile all the source files and generate the binary **ooh**. 

### 3. Start coding!

We have already made a simple hello world program for you. To run it, just send as a parameter the source file to the executable created in the last step (**ooh**):

```bash
./ooh Tests/HelloWorld.ooh
```

# 2. Common Programming Concepts

## 2.1 Native Types

OOH supports the following native types:

| Type        | 
| :-------------:|
| Double      |
| Money/Decimal      |
| Int | 
| Integer |
| String |
| Bool |

* **Double** is for double-precision floating point numbers. 
* **Money/Decimal** is a more precise representation of rational numbers. It can hold up to 255 decimals, and is the perfect type for applications that require precision, such as financial transactions.
* **Int** is an Int64 representation of signed integers. The max value it can hold is 2^63 - 1 and the min value is -2^63.
* **Integer** is the BigInteger representation of signed integers. There is no real limit to the number you can represent, aside from memory limitations.
* **String** for strings/array of characters
* **Bool** for True/False values.

The Type system is further augmented through the use of Classes (see section ...)


## 2.2 Variables

Variables are the mutable state of a program. They are limited through their type, as it is the latter that limits the possible values they can take.

```c++
Money m;
Double d = 2.1;
String s = "Hello World";
Class h;
Human h1 = Human();

Money [10] arrayOfMoney;
Integer [5] ints = [1,2,3,4,5];
String [10][10] names = [["Juan","Elda","Hector"],
                         ["Minerva","Alitzel"],
                         ["Liliana"]];
Human [2] humans = [h,h1];
Class [2][2] matrixOfHumans = [[h,h1]];
```
Currently, it is not possible to declare a variable and assign it an expression. This feature will be added in further releases. For doing this, you need to declare the variable and assign the expression in an **Assignment**

## 2.3 Assignments

Assignments are the way of mutating variables in a program. Assignments are limited by the type of the variable.

```c++
Money m;
m = 1 / 3;

String s;
s = "Hello" + "World";

ints[0] = ints[0] + ints[1];
```

## 2.4 Functions

Functions are the perfect way of representing computations in OOH. 

OOH code uses snake case as the conventional style for function and variable names. In snake case, all letters are lowercase and underscores separate words. Here’s a program that contains an example function definition:
```c++
func => Nothing{
  displayLn("Hello World");
}
```
This is a function that does not have any formal parameters, and returns nothing.
You can call it as:
```c++
func();
```

This will just display the ```Hello World``` message in console.

The general form of a function is the following:

```c++
func => <ReturnType> :: <Type> param1 -> <Type> param2 -> <Type> param3{
  <Statements>
  return <expression>;
}
```

Let's now do a function that receives two integers and returns the sum of both:
```c++
add => Integer :: Integer a -> Integer b{
  return a + b;
}
```

Call it as:

```c++
Integer res;
res = add(1,2);
displayLn(res);
```

This will display the value ```3```.

You can also return arrays and matrices of any type, such as a defined ```Class``` or native type:

```c++
class Human{
}

class Person : Human{
}

makeHumans => Human [2] :: Human h -> Person p{
  Human [2] humans = [h,p];
  return humans;
}
```
This makes a generic array of humans, initializing it with h and p and returns it.

## 2.5 Classes

Now that you know what variables and functions are, we can now explain classes. Classes are a practical way of grouping common attributes and functions. This helps achieve two main software engineering principles, **abstraction** and **encapsulation**. 

**[1] Abstraction** is when the client of a module does not need to know more than what is stated in the interface.
**[2] Encapsulation** is when the client isn't able to know more than what is stated in the interface.

With this in mind, we can see classes as a form of **[1] abstraction**, as it abstracts the client of the logic that lies in each class.
Through the use of **public** and **private** attributes and methods, which are member functions of a class, we achieve **[2] encapsulation**. 

Now that we are set up, classes **must** be declared with a capital letter first, followed by any lowercase/uppercase alphanumeric character.

```c++
class Human{
  [+] Integer age;
  [+] Decimal height;
  [-] String dna;
  
  Human :: Integer a -> Decimal h -> String d{
    age = a;
    height = h;
    dna = d;
  }
  
  [+] setDna => Nothing :: String d{
    dna = d;
  }
  
  [+] getDna => String{
    return dna;
  }
}
```

**[+]** is used for declaring **public** attributes and methods of a class. These members can be accessed **outside** and **inside** of the class.
**[-]** is used for declaring **private** attributes and methods of a class. These members can **only** be accessed from within the class. They **can't** be accessed from outside the class.

In this case, the ```age``` and ```height``` attributes can be accessed outside of the class, by using **dot** notation
```c++
Human h;
h.age = 12;
h.height = 180.1;
```
However, since ```dna``` is a **private** attribute (denoted by the **[-]** prefix), it cannot be accessed using dot notation:
```c++
// 👎 This won't work, since dna is a private attribute:
h.dna = "GATACA";
```

Instead, use the **public** getter and setter methods for retrieving and modifying the attribute, respectively:

```c++
// 👍 This will work
// Set the attribute
h.setDna("GATACA");
// Retrieve the attribute
displayLn(h.getDna());
```
This will print the string ```GATACA```

## 2.6 Control Flow

## Branching your code

### ```If``` expressions

### ```If```-```else``` expressions

### ```Case``` expressions

## Making loops

### Ranged Loops with ```For```

### Conditional Loops with ```While```

