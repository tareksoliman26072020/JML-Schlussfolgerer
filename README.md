# JML-Schlussfolgerer
this project is a software suite written completely in haskell, and it includes two softwares
- the first is for parsing java syntax.
- the second is to process the parsed java code and process it to produce the corresponding [JML](https://en.wikipedia.org/wiki/Java_Modeling_Language)-syntax for the represented java source code.
> The Java Modeling Language (JML) is a specification language for Java programs, using Hoare style pre- and postconditions and invariants, that follows the design by contract paradigm. Specifications are written as Java annotation comments to the source files, which hence can be compiled with any Java compiler.
---
### Examples for the utility of the parser
The following java method will be fed into a stateful monadic funcion in haskell to be parsed
```java
public int foo(){
    return 666;
}
```

which gives:

```haskell
FunDef {funModifier = [Public],
        isPureFlag = False,
        funDecl = FunCallStmt {funCall = FunCallExpr {funName = VarExpr {varType = Just (BuiltInType Int),
                                                                         varObj = [],
                                                                         varName = "foo"},
                                                      funArgs = []}},
        throws = Nothing,
        funBody = CompStmt {statements = [ReturnStmt {returnS = Just (IntLiteral 666)}]}}

```

This is done as the following: 
  - Make sure that you installed [stack](https://docs.haskellstack.org/en/stable/README/)
  - Then build the project: `stack build`
  - Use the command: `stack ghci --only-main`
  - store the java method as a string, then pass it as an argument: `fromRight undefined (parse parseExtDecl "error message" "public int foo(){return 666;}")`
Note: The parser is not completely implemented in the sense that not every input can be parsed.
      Have a look at ./test.java to be familiar with the kind of functions that can be parsed.

---
### Examples for JML:
```java

/*@ normal behavior
  @ requires i<=y &&
  @          i*i==y;
  @ assignable \nothing;
  @ ensures \result = i;
  @ also
  @ exceptional behavior
  @ requires i<=y &&
  @          i*i!=y &&
  @          i==y;
  @ signals Exception;
  @*/
public static int sqrt(int y) throws Exception{
  for(int i = 0; i <= y; i = i + 1){
    int j = i * i;
    if(j == y){
      return i;
    }
    else{
      if(i == y){
        throw new Exception("not found");
      }
    }
  }
}
```
```java
/*@ normal behavior
  @ requires i>=0;
  @ assignable \nothing;
  @ ensures \result = i;
  @ also
  @ normal behavior
  @ requires i<0;
  @ assignable \nothing;
  @ ensures \result = (-1)*i;
  @*/
public /*@ pure @*/ int boo27(int i){
  if(i >= 0){
    return i;
  }
  else{
    int res = -1 * i;
    return res;
  }
}

/*@ exceptional behavior
  @ requires true;
  @ signals Exception;
  @ also
  @ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 5;
  @*/
public int boo28(){
  try{
    int x = 3;
    if(x == 3){
      throw new Exception("something");
    }
    else{
      return 1;
    }
  }
  catch(Exception e){
    return boo27(5);
  }
}

/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 1;
  @ also
  @ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 5;
  @*/
public int boo28_2(){
  try{
    int x = 0;
    if(x == 3){
      throw new Exception("something");
    }
    else{
      return 1;
    }
  }
  catch(Exception e){
    return boo27(-5);
  }
}
```

this is done as the following: pass the source code as a java file to stack `stack build && stack exec JML-Schlussfolgerer-exe test.java`

---
The tool is useable and can be utilied
BUT: the _parser_ and the _JML processer_ is not in a complete state in the sense that not everything can be yet parsed because this project was mainly begun with for research reasons. however it's meant to be re-written in the near future to suit the needs of any casual java programmer.

The main focus was so far on five tasks:
- Destinguishing pure methods from impure methods, which was succesfully achieved using the funcion `isPure`
- Distinguishing normal behavioral from exceptional behavior which is of relevance to produce jml syntax.
- Handling interprocedural function calls.
- Building a simplistic Compiler and using it to evaluate passed arguments (actual parameters) to interprocedural calls.
- evaluating global variables and recognising their re-assignment.
and for these tasks a minimal implementation of the parser is sufficient, which results in a minimal implementation of the JML producer which in itself is complete for the wished to-be-discussed criteria.
Handling void methods have been unsuccessful in regard to global variables due to the costs of refactoring of the code.
---

The project is built using [stack 2.5.1](https://docs.haskellstack.org/en/stable/README/)

[INTELLIJ idea](https://www.jetbrains.com/de-de/idea/) is being used as an editor, and the plugin [IntelliJ-Haskell](https://plugins.jetbrains.com/plugin/8258-intellij-haskell) as the language support for INTELLIJ
