module Examples where
import Data.Maybe

example75 = "public static void main(String[] args) throws ArithmeticException {" ++ "\n"
         ++ "  int x = 5;" ++ "\n"
         ++ "  Integer apply = x;" ++ "\n"
         ++ "  System.out.println(apply);" ++ "\n"
         ++ "}"

example76 = "static void checkAge(int age) throws ArithmeticException {" ++ "\n"
         ++ "  if (age < 18) {" ++ "\n"
         ++ "    throw new ArithmeticException(\"Access denied - You must be at least 18 years old.\");" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  else {" ++ "\n"
         ++ "    System.out.println(\"Access granted - You are old enough!\");" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example77 = "if (age < 18) {" ++ "\n"
         ++ "  throw new ArithmeticException(\"Access denied, You must be at least 18 years old.\");" ++ "\n"
         ++ "}" ++ "\n"
         ++ "else {" ++ "\n"
         ++ "  System.out.println(\"Access granted - You are old enough!\");" ++ "\n"
         ++ "}"

example78 = "public int increaseByFour(int x){" ++ "\n"
         ++ "    int y = x + 4;" ++ "\n"
         ++ "    z = 0;" ++ "\n"
         ++ "    return y;" ++ "\n"
         ++ "}"
  
example79 = "public int myRound(int input){" ++ "\n"
         ++ "    if(input > 5){" ++ "\n"
         ++ "        return 5;" ++ "\n"
         ++ "    }else{return round(input)/10;}" ++ "\n"
         ++ "}"

example80 = "    for(int i = 3; i < 5; i=i+1){" ++ "\n"
         ++ "        myRound(i);" ++ "\n"
         ++ "    }"

example81 = "public int getBalance() throws BankingException{" ++ "\n"
         ++ "    if (!this.isLocked){" ++ "\n"
         ++ "        return this.balance;" ++ "\n"
         ++ "    }else{" ++ "\n"
         ++ "         throw new BankingException ();" ++ "\n"
         ++ "    }" ++ "\n"
         ++ "}"

example83 = "public int foo(int x){meow();}"

example84 = "public int boo(){wof();}"

example85 = "public int boo2(){" ++ "\n"
         ++ "  while(!true){" ++ "\n"
         ++ "    z = 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example86 = "public int boo3(){" ++ "\n"
         ++ "  for(int i = 0; i<10; i=i+2){" ++ "\n"
         ++ "    z = 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example87 = "public int boo4(){" ++ "\n"
         ++ "  if(b<10){" ++ "\n"
         ++ "    int z = 0;" ++ "\n"
         ++ "  }else{" ++ "\n"
         ++ "    int t = 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example88 = "public int boo5(){" ++ "\n"
         ++ "  try{" ++ "\n"
         ++ "    int x = 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  catch(Exception e){" ++ "\n"
         ++ "    int z = 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  return 0;" ++ "\n"
         ++ "}"

example89 = "public int boo6(){" ++ "\n"
         ++ "  return x;" ++ "\n"
         ++ "}"

example90 = "public int boo7(){" ++ "\n"
         ++ "  return boo5();" ++ "\n"
         ++ "}"
         
example91 = "public int boo8(){" ++ "\n"
         ++ "  int x = boo5();" ++ "\n"
         ++ "  return x;" ++ "\n"
         ++ "}"

example92 = "public int boo9(){" ++ "\n"
         ++ "  if(1<2){" ++ "\n"
         ++ "    int x;" ++ "\n"
         ++ "    x = boo8();" ++ "\n"
         ++ "    return x;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  else{" ++ "\n"
         ++ "    return 0;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example93 = "public int boo10(){" ++ "\n"
         ++ "  int b = 1;" ++ "\n"
         ++ "  if(b<10){" ++ "\n"
         ++ "    int z = 2;" ++ "\n"
         ++ "    return z;" ++ "\n"
         ++ "  }else{" ++ "\n"
         ++ "    int t = 3;" ++ "\n"
         ++ "    return t;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example93_2 = "public void boo10_2(){" ++ "\n"
         ++ "  int b = 1;" ++ "\n"
         ++ "  if(b<10){" ++ "\n"
         ++ "    int z = 2;" ++ "\n"
         ++ "  }else{" ++ "\n"
         ++ "    int t = 3;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

example94 = "public int boo11(){" ++ "\n"
         ++ "  for(int i = 0; i < 10; i=i+1){}" ++ "\n"
         ++ "  return 0;" ++ "\n"
         ++ "}"

example95 = "public int boo12(){" ++ "\n"
         ++ "  for(i = 0; i < 10; i=i+1){}" ++ "\n"
         ++ "  return 0;" ++ "\n"
         ++ "}"

example96 = "public int boo13(){" ++ "\n"
         ++ "  for(int i = 0; j < 10; i=i+1){}" ++ "\n"
         ++ "  return 0;" ++ "\n"
         ++ "}"

example97 = "public int boo14(){" ++ "\n"
         ++ "  return i;" ++ "\n"
         ++ "}"

example98 = "public int boo15(){" ++ "\n"
         ++ "  return 0;" ++ "\n"
         ++ "}"

--purity
example82 = example76 ++ "\n\n" ++ example78 ++ "\n\n" {-++ example79 ++ "\n\n"-} ++ example81 ++ "\n\n" ++ {-example83 ++ "\n\n" ++ example84 ++ "\n\n" ++ -}example85 ++ "\n\n" ++ example86 ++ "\n\n" ++ example87 ++ "\n\n" ++ example88 ++ "\n\n" ++ example89 ++ "\n\n" ++ example90 ++ "\n\n" ++ example91 ++ "\n\n" ++ example92 ++ "\n\n" ++ example93 ++ "\n\n" ++ example94 ++ "\n\n" ++ example95 ++ "\n\n" ++ example96 ++ "\n\n" ++ example97 ++ "\n\n" ++ example98 ++ "\n\n"

example99 = "public int boo15(){" ++ "\n"
         ++ "  if(true){" ++ "\n"
         ++ "    return 5;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  else{" ++ "\n"
         ++ "    return 10;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"
        
example100 = "public int boo16(){" ++ "\n"
         ++ "  if(i>10){" ++ "\n"
         ++ "    throw new Exception(\"meow\");" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  else{}" ++ "\n"
         ++ "}"

example101 = "public int boo17(){" ++ "\n"
          ++ "  int x;" ++ "\n"
          ++ "  int y = 5;" ++ "\n"
          ++ "  if(y < 1){" ++ "\n"
          ++ "    return 5;" ++ "\n"
          ++ "  }else{" ++ "\n"
          ++ "     throw new Exception(\"meow\");" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"
          
example102 = "public int boo18(){" ++ "\n"
          ++ "  for(int i=0; i<10; i=i+1){" ++ "\n"
          ++ "    if(i==5){" ++ "\n"
          ++ "      throw new Exception();" ++ "\n"
          ++ "    }else{" ++ "\n"
          ++ "      return 5;" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"

example103 = "public int boo19(){" ++ "\n"
          ++ "  for(int i=0; i<10; i=i+1){" ++ "\n"
          ++ "    if(i==5){" ++ "\n"
          ++ "      throw new Exception();" ++ "\n"
          ++ "    }else{" ++ "\n"
          ++ "      return boo17();" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"

example103_2 = "public int boo19_2(int i){" ++ "\n"
            ++ "  if(i==5){" ++ "\n"
            ++ "      throw new Exception();" ++ "\n"
            ++ "  }" ++ "\n"
            ++ "  else{" ++ "\n"
            ++ "    return boo17();" ++ "\n"
            ++ "  }" ++ "\n"
            ++ "}"

example103_3 = "public int boo19_3(){" ++ "\n"
            ++ "  return boo17();" ++ "\n"
            ++ "}"

example104 = "public int boo20(int n){" ++ "\n"
          ++ "  if(n>0){" ++ "\n"
          ++ "    return 0;" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "  else{" ++ "\n"
          ++ "    if(n>5){" ++ "\n"
          ++ "      return 5;" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "    else{" ++ "\n"
          ++ "      return 10;" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"
          
----------

{-
/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 5;
@*/

[(Nothing,JMLExpr (BoolLiteral True),Just (JMLExpr (IntLiteral 5)))]
-}
example105 = "public int boo21(){" ++ "\n"
          ++ "  return 5;" ++ "\n"
          ++ "}"

{-
/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 5;
@*/

[(Nothing,JMLExpr (BoolLiteral True),Just (JMLExpr (IntLiteral 5)))]
-}
example106 = "public int boo22(){" ++ "\n"
          ++ "  return boo21();" ++ "\n"
          ++ "}"

{-
/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 5;
@*/

[(Nothing,JMLExpr (BoolLiteral True),Just (JMLExpr (IntLiteral 5)))]
-}
example106_2 = "public int boo22_2(){" ++ "\n"
            ++ "  int x = boo21();" ++ "\n"
            ++ "  return x;" ++ "\n"
            ++ "}"
            
{-
/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 3+5;
@*/

[(Nothing,
  JMLExpr (BoolLiteral True),
  Just (JMLExpr (BinOpExpr {expr1 = IntLiteral 3, binOp = Plus, expr2 = IntLiteral 5})))]
-}
example108 = "public int boo23(){" ++ "\n"
          ++ "  int x = 3 + boo21();" ++ "\n"
          ++ "  return x;" ++ "\n"
          ++ "}"

{-
/*@ exceptional behavior
  @ requires (boo25->i)>10;
  @ signals (Exception)
@*/

[(Just Exception,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "boo25->i"}, binOp = Greater, expr2 = IntLiteral 10}),
  Nothing)]
-}
example109 = "public int boo24(){" ++ "\n"
          ++ "  int x = 3 + boo25(5);" ++ "\n"
          ++ "  return x;" ++ "\n"
          ++ "}"

{-
/*@ exceptional behavior
  @ requires i>10;
  @ signals (Exception)
  @ also
  @ normal behavior
  @ requires i<=10;
  @ assignable \nothing;
  @ ensures \result = 6;
  @*/

[(Just Exception,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "i"}, binOp = Greater, expr2 = IntLiteral 10}),
  Nothing),
  
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "i"}, binOp = LessEq, expr2 = IntLiteral 10}),
  Just (JMLExpr (IntLiteral 6)))]
-}
example110 = "public int boo25(int i){" ++ "\n"
         ++ "  if(i>10){" ++ "\n"
         ++ "    throw new Exception(\"meow\");" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "  else{" ++ "\n"
         ++ "    return 6;" ++ "\n"
         ++ "  }" ++ "\n"
         ++ "}"

{-
/*@ normal behavior
  @ requires (boo27->i)>=0;
  @ assignable \nothing;
  @ ensures \result = (boo27->i);
  @ also
  @ normal behavior
  @ requires (boo27->i)<0;
  @ assignable \nothing;
  @ ensures \result = (-1)*(boo27->i);
  @*/

[(Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}, binOp = GreaterEq, expr2 = IntLiteral 0}),
  Just (JMLExpr (VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}))),
 
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}, binOp = Less, expr2 = IntLiteral 0}),
  Just (JMLExpr (BinOpExpr {expr1 = IntLiteral (-1), binOp = Mult, expr2 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}})))]
-}
example111 = "public int boo26(){" ++ "\n"
          ++ "  return boo27(5);" ++ "\n"
          ++ "}"

{-
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

[(Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "i"}, binOp = GreaterEq, expr2 = IntLiteral 0}),
  Just (JMLExpr (VarExpr {varType = Nothing, varObj = [], varName = "i"}))),
 
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "i"}, binOp = Less, expr2 = IntLiteral 0}),
  Just (JMLExpr (BinOpExpr {expr1 = IntLiteral (-1), binOp = Mult, expr2 = VarExpr {varType = Nothing, varObj = [], varName = "i"}})))]
-}
example112 = "public int boo27(int i){" ++ "\n"
          ++ "  if(i >= 0){" ++ "\n"
          ++ "    return i;" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "  else{" ++ "\n"
          ++ "    int res = -1 * i;" ++ "\n"
          ++ "    return res;" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"

{-
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

[(Just Exception,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "x"}, binOp = Eq, expr2 = IntLiteral 3}),
  Nothing),
 
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "x"}, binOp = Neq, expr2 = IntLiteral 3}),
  Just (JMLExpr (IntLiteral 1))),
 
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}, binOp = GreaterEq, expr2 = IntLiteral 0}),
  Just (JMLExpr (VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}))),
 
 (Nothing,
  JMLExpr (BinOpExpr {expr1 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}, binOp = Less, expr2 = IntLiteral 0}),
  Just (JMLExpr (BinOpExpr {expr1 = IntLiteral (-1), binOp = Mult, expr2 = VarExpr {varType = Nothing, varObj = [], varName = "boo27->i"}})))
-}
example113 = "public int boo28(){" ++ "\n"
          ++ "  try{" ++ "\n"
          ++ "    int x = 3;" ++ "\n"
          ++ "    if(x == 3){" ++ "\n"
          ++ "      throw new Exception(\"something\");" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "    else{" ++ "\n"
          ++ "      return 1;" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "  catch(Exception e){" ++ "\n"
          ++ "    return boo27(5);" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"

{-
/*@ normal behavior
  @ requires ((i<=y) && (j==y));
  @ assignable \nothing;
  @ ensures i;
  @ also
  @ exceptional behavior
  @ requires i<=y && j!=y && i==y;
  @ signals (Exception) true;
@*/
-}
example114 = "public static int sqrt(int y) throws Exception{" ++ "\n"
          ++ "  for(int i=0; i<=y; i=i+1){" ++ "\n"
          ++ "    int j = i*i;" ++ "\n"
          ++ "    if(j==y){" ++ "\n"
          ++ "      return i;" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "    else{" ++ "\n"
          ++ "      if(i==y){" ++ "\n"
          ++ "        throw new Exception(\"not found\");" ++ "\n"
          ++ "      }" ++ "\n"
          ++ "      else{}" ++ "\n"
          ++ "    }" ++ "\n"
          ++ "  }" ++ "\n"
          ++ "}"

example107 = unlines [example105,example106,example106_2,example108,example109,example110,example111,example112,example113,example114]

spaces n = replicate n ' '

-------------------------------------------------
--TODO: parse list of generics instead of only single generic: in parsing variables, functions
--TODO: parseBinOp for ++, --, += , ....etc
--TODO: parseReturn
--TODO: parse the catch
--TODO: parse lambda expressions
--TODO: parse lambda expressions for assigns
--TODO: parse comments
--TODO: parseAssign for right side is far from complete
--TODO: parse if without the necessity of the existence of else
--TODO: parse inner single statements without the necessity of the existence of brackets
--TODO: and MUCH more
