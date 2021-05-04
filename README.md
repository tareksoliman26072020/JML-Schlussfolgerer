# JML-Schlussfolgerer
this project is a software suite written completely in haskell, and it includes two softwares
- the first is for parsing java syntax.
- the second is to process the parsed java code and process it to produce the corresponding JML-syntax for the represented java source code.
> The Java Modeling Language (JML) is a specification language for Java programs, using Hoare style pre- and postconditions and invariants, that follows the design by contract paradigm. Specifications are written as Java annotation comments to the source files, which hence can be compiled with any Java compiler.
---
### Examples for the utility of the parser
The following java method will be fed into a stateful monadic funcion in haskell to be parsed
```java
public int increaseByFour(int x){
    int y = x + 4;
    z = 0;
    return y;
}

```

which gives:

```haskell
Just (
  FunDef {funModifier = [Public],
          funDecl = FunCallStmt {funCall = FunCallExpr {
                    funName = VarExpr {varType = Just (BuiltInType Int),
                                       varObj = [], varName = "increaseByFour"},
                    funArgs = [VarExpr {varType = Just (BuiltInType Int),
                                        varObj = [], varName = "x"}]}},
          throws = Nothing,
          funBody = CompStmt {statements = [
                    AssignStmt {varModifier = [],
                                assign = AssignExpr {assEleft = VarExpr {varType = Just (BuiltInType Int), varObj = [], varName = "y"},
                                                     assEright = BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "x"},
                                                                            binOp = Plus,
                                                                            expr2 = IntLiteral 4}}},
                    AssignStmt {varModifier = [],
                                assign = AssignExpr {assEleft = VarExpr {varType = Nothing, varObj = [], varName = "z"},
                                                     assEright = IntLiteral 0}},
                    ReturnStmt {returnS = Just (VarExpr {varType = Nothing,
                                                         varObj = [],
                                                         varName = "y"})}]}},
  "")
```

This is done as the following: store the java method as a string, then pass it as an argument: `parse parseFunDef method`

---
### Examples for JML:
```java

/*@ normal behavior
  @ requires ((i<=y) && (j==y));
  @ assignable \nothing;
  @ ensures i;
  @ also
  @ exceptional behavior
  @ requires i<=y && j!=y && i==y;
  @ signals (Exception) true;
  @*/
public static int sqrt(int y) throws Exception{
  for(int i=0; i<=y; i=i+1){
    int j = i*i;
    if(j==y){
      return i;
    }
    else{
      if(i==y){
        throw new Exception("not found");
      }
      else{}
    }
  }
}
```
```java
/*@ exceptional behavior
  @ requires x=3;
  @ signals (Exception) true;
  @ also
  @ normal behavior
  @ requires x!=3;
  @ assignable \nothing;
  @ ensures 1;
  @ also
  @ normal behavior
  @ requires (boo27->i)>=0;
  @ assignable \nothing;
  @ ensures (boo27->i);
  @ also
  @ normal behavior
  @ requires (noo27->i)<0;
  @ assignable \nothing;
  @ ensures \result = (-1)*(boo27->i);
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
  @ requires i>=0;
  @ assignable \nothing;
  @ ensures \result = i;
  @ also
  @ normal behavior
  @ requires i<0;
  @ assignable \nothing;
  @ ensures \result = (-1)*i;
  @*/
public int boo27(int i){
  if(i >= 0){
    return i;
  }
  else{
    int res = -1 * i;
    return res;
  }
}
```

this is done as the following: sage the method as a string, then call: `getRequireBehavior False method "boo27"`

---
The tool is useable and can be utilied
BUT: the _parser_ and the _JML processer_ is not in a complete state in the sense that not everything can be yet parsed because this project was mainly begun with for research reasons. however it's meant to be re-written in the near future to suit the needs of any casual java programmer.

The main focus was so far on two tasks:
- destinguishing pure methods from impure methods, which was succesfully achieved using the funcion `isPure`
- distinguishing normal behavioral from exceptional behavior which is of relevance to produce jml syntax.
and for these tasks a minimal implementation of the parser is sufficient, which results in a minimal implementation of the JML producer which in itself is complete for the wished to-be-discussed criteria.
---

The project is built using [stack 2.5.1](https://docs.haskellstack.org/en/stable/README/)

[INTELLIJ idea](https://www.jetbrains.com/de-de/idea/) is being used as an editor, and the plugin [IntelliJ-Haskell](https://plugins.jetbrains.com/plugin/8258-intellij-haskell) as the language support for INTELLIJ
