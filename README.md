
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
* **Int** is an Int64 representation of signed integers. The max value it can hold is ```2^WORD_SIZE - 1``` and the min value is ```-2^WORD_SIZE```. The WORD_SIZE depends on whether you are using a 32 bit or a 64 bit operating system. You can see the size by taking a look at the config.h's parameter ```SIZEOF_HSWORD```. On a 64 bit OS, the word size is usually 64 bits.
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
Now let's call the function:
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

We can now call it:

```c++
Integer res;
// Add receives two integer arguments, 1 and 2
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
This makes a generic array of humans, initializing it with h and p and returns this newly created array.

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
  
  // Class constructor
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
h.dna = "GATTACA";
```

Instead, use the **public** getter and setter methods for retrieving and modifying the attribute, respectively:

```c++
// 👍 This will work
// Set the attribute
h.setDna("GATTACA");
// Retrieve the attribute
displayLn(h.getDna());
```
This will print the string ```GATACA```

### Constructors

Class constructors are a nifty way of initializing an object's attributes rightaway when making a variable declaration, instead of accessing the attributes to initialize them:

```c++
Human h = Human(12,180.1,"GATTACA");
```

This is equivalent to what we did in the past section.

## Advanced OOP Topics

### Inheritance

Inheritance is a way of "passing down" attributes and methods from a **parent** class to a **child** class. This is a way of improving code reusability by following the **DRY** (short for **D**on't **R**epeat **Y**ourself) software engineering principle. If there is a class that implements its basic features, but we need to specialize it even more, inheritance helps us achieve this by **inheriting** from the class we want to specialize. Using our ```Human``` class example:
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
  
  [+] speak => Nothing{
    displayLn("I'm a human!");
  }
}
```
```Human``` has some basic attributes, such as age, height and dna, as well as a ```speak``` method. However, let's say we want to specialize it, and we now need to model ```Person```s. A ```Person``` has a ```job```, a ```salary``` and a ```country``` they were born in, as well as all the basic attributes of a human, such as ```age```, ```height``` and ```dna```:

First things first, let's declare the ```Job``` class:

```c++
class Job{
  [+] String country;
  [+] String company;
}
```
Now the ```Person``` class
```c++
// The ":" reserved character is used for inheritance.
// This is analogous to saying: "A person inherits from human"
class Person : Human{
  [+] Job job;
  [+] Money salary;
  [+] String country;
  
  Person :: Integer a -> Decimal h -> String d -> Job j -> Money s -> String c{
    // Notice how we can access the inherited attributes
    age = a;
    height = h;
    dna = d;
    // These are the specialized attributes that are unique in the Person class
    job = j;
    salary = s;
    country = c;
  }
}
```

```Person``` not only inherits the attributes from ```Human```, but also its methods. Therefore, it's also possible to call ```Human``` methods from outside/inside of the class. For instance:

```c++
Person p;
p.setDna("ACAAGATGCCATTGTCCCCCGGCCTCCT");
```
### Function Overriding

The previous example addresses a way of adding new attributes and methods. However, what happens with our ```speak``` ```Human``` method? Since ```Person``` inherits from ```Human```, it also inherits the ```speak``` implementation of ```Human```. If we call the ```speak``` method from ```p```:

```c++
p.speak();
```

The string ```I'm a human!``` will be printed to console. Although this is correct, we might want our ```speak``` method to print something more accurate, such as ```I'm a person!```.

OOH fully supports function overriding, which is a way of making a specialized instance of a method. In this case, we will specialize our ```speak``` method of ```Person```:

```c++
class Person : Human{
  [+] Job job;
  [+] Money salary;
  [+] String country;
  
  Person :: Integer a -> Decimal h -> String d -> Job j -> Money s -> String c{
    // Notice how we can access the inherited attributes
    age = a;
    height = h;
    dna = d;
    // These are the specialized attributes that are unique in the Person class
    job = j;
    salary = s;
    country = c;
  }
  
  // speak just got overriden!
  [+] speak => Nothing{
    displayLn("I'm a person!");
  }
}
```

Now if we call it:

```c++
p.speak();
```

```I'm a person!``` wil be printed to console.

### Polymorphism

In the last example of the Functions section, we briefly saw how we can create generic arrays by using a parent class as a generic type, which can take many forms. This is the main reasoning behind **Polymorphism**: an object from an inherited class can be assigned to an object of a parent class:

```c++
Human human;
Person person;
// We make a polymorphic assignment, since a Person is also a Human
human = person;
// 👎 This won't compile, because a Human is NOT a Person. Polymorphic assignments are from the
// specialized class (the child class), to the parent class.
person = human;
```

By doing the ```human = person``` assignment,  ```human``` now has the ```Person``` implementation of ```speak```. If we call it:

```c++
human.speak()
```

Since ```human``` was assigned a ```person``` object, ```I'm a person!``` wil be printed to console. This also works in functions:

```c++
polyFunction => Nothing :: Human h{
  h.speak();
}
```

If we send a ```Human``` object, ```I'm a human!``` will be printed. If we send a ```Person``` object, ```I'm a person!``` will be printed:

```c++
Human human;
Person person;
// "I'm a human!"
polyFunction(human);
// "I'm a person!"
polyFunction(person);
// We do a polymorphic assignment
human = person;
// "I'm a person!", since human was assigned a Person object
polyFunction(human);
```

### Generic arrays

As you already learned, you can also create generic arrays:

```c++
Human h;
Person p;
Human [2] humans = [h,p];
Human human;
human = humans[0];
// "I'm a human!"
human.speak();
human = humans[1];
// "I'm a person!"
human.speak();
```

## 2.6 Control Flow

## Branching your code

These expressions help you branch your code, making it take different routes.

### ```If``` expressions

An ```if``` expression takes a ```Bool``` expression and execute its inner statements if the expression evaluates to ```True```:
```c++
Int i;
read(i);
if (i > 0){
  displayLn("i is greater than 0");
}
```

### ```If```-```else``` expressions

The ```if```-```else``` expression is an extension to the ```if``` expression. If the expression evaluates to ```False```, the ```else``` inner statements get executed:

```c++
Int i;
read(i);
if (i > 0){
  displayLn("i is greater than 0");
}
else{
  displayLn("i is less than or equal to 0");
}
```

### ```Case``` expressions

The ```case``` expression matches expressions:

```c++
Int i,j;
read(i);
read(j);
case (i * 2) of
  j => displayLn("i * 2 == j");
  j / 2 => displayLn("i * 2 == j / 2");
  otherwise => displayLn("i,j",i,j);
```

The ```otherwise``` statements get executed in case the past expressions did not get matched.
## Making loops

These expressions help you make repetitions with loops.

### Ranged Loops with ```For```

There are several ways you can make a ```For``` loop in OOH:

```c++
// This is an inclusive for loop from both ranges
// Output: 0 1 2 3 4 5 6 7 8 9
for i in [0..9]{
  display(i);
}

// This is an inclusive for loop from the lower range, but not inclusive in the upper range
// Output: 0 1 2 3 4 5 6 7 8
for i in [0..9){
  display(i);
}

// This is an inclusive for loop from the upper range, but not inclusive in the lower range
// Output: 1 2 3 4 5 6 7 8 9
for i in (0..9]{
  display(i);
}

// This is a non-inclusive for loop from both ranges
// Output: 1 2 3 4 5 6 7 8
for i in (0..9){
  display(i);
}

```
### Conditional Loops with ```While```

A ```While``` loop takes a ```Bool``` expression, and loops it if and only if the expression evaluates to ```True```
```c++
Int i;
// Output: 0 1 2 3 4 5 6 7 8 9
while(i <= 9){
  displayLn(i);
  i++;
}
```
