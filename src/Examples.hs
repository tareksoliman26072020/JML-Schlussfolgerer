module Examples where
import Parser
import Data.Maybe

-- Method has modifier and type and name
example1 =
  "static void myMethod() {\n" ++
  "  // code to be executed\n" ++
  "}"
  
-- Method has modifier and type and name
example2 =
  "             public static void main(String []args) {\n" ++
  "  // code to be executed\n" ++
  "}"

-- type int
example3 = "    int x = 0;"

-- Anytype without generic: String
example4 = "    String x = \"wof\";"

-- Anytype with generic: ArrayList with generic
example5 = "    ArrayList<Integer> x = new ArrayList<>();"

-- ArrayType of not built-in type
example6 = "String[] cars = {\"Volvo\", \"BMW\", \"Ford\", \"Mazda\"};"

-- ArrayType of built-in type
example7 = "int[] numbers = {1,2,3,4};"

-----------------------------------------------------

-- from example8 to example15: testing parseVariable
-- no modifier, no type 
example8 = " x;"

-- no modifier, with type
example9 = "int x;"

-- modifier with type
example10 = "private int x;"

-- multiple modifiers, with type, with object
example11 = "private final int y.x;"

-- multiple modifiers, with generic type, with object
-- Es muss keinen Leerzeichen zwischen Type and < geben.
example12 = "private final ArrayList<Integer> y.x;"

-- with generic type
example13 = "ArrayList<Integer> x;"

-- modifier, with generic type
example14 = "private ArrayList<Integer> x;"

-- multiple modifiers, with generic type
example15 = "private final ArrayList<Integer> x;"

example15_2 = "    private final ArrayList  <  Integer   >   x  .  y  .  z        ;"

----------------------------------------------------

example16 = " x+"

example17 = "(x);"

example18 = "(x));"

example19 = "(x.(y));"

example20 = "((x.(y));"

example21 = "x.(y));"

-----------------------------------------------------

example22 = "1    +2;"

example23 = "x = 1    +2;"

example24 = "int x = 1    +2;"

example25 = "int x.y = 1    +2;"

example26 = "int x.y = (1    +2);" --TODO: Mit Klammern umzugehen

example27 = "int x.y = (1    +2)+3;" --TODO: Mit Klammern umzugehen

example27_2 = "private static int x.y = \"meow\" + \"wof\";"

-----------------------------------------------------

example28 = "!meow.wof;"

-----------------------------------------------------

example29 = "x[i];"

example30 = "x[i+j];"

example31 = "x[1];"

example31_2 = "x[];"

example31_3 = "x.y[z.t()]"

example31_4 = "x.y[z.t(1,r.wof(\"meow\"))]"

-----------------------------------------------------

example32 = "meow();"

example33 = "meow(i);"

example34 = "beek.meow();"

example35 = "meow(1,2,\"wof1\",3+4,\"wof2\",3,4,i);"

example36 = "beek.meow(1,2,\"wof1\",3+4,\"wof2\",3,4,i);"

example37 = "meow(1,wof());"

example38 = "beek1.meow(1,beek2.wof());"

-----------------------------------------------------

example39 = "1 == 1 ? 42 : 43;"

example40 = "1 == 1 ? System.out.println(\"42\") : 43;"

example41 = "1 == 1 ? System.currentTimeMillis() : 43;"

example42 = "getDeliveryAddress() != 1 ? getDeliveryAddress() : getPaymentAddress();"

example43 = "amount != 5 ? amount.toPlainString() : \"0.00\";"

-----------------------------------------------------

example44 = "throw new IllegalArgumentException();"

example45 = "throw new ArrayIndexOutOfBoundsException(\"meow\");"

example46 = "throw new MeowException(\"wof\");"

-----------------------------------------------------

--NOTES: when parsing Type: no whitespaces in between, even when dealing with generics
--       initializing was not parsed.

-----------------------------------------------------

example47 = "int x1 = 1+2;" ++ "\n"
         ++ "int x2 = 3+4;" ++ "\n"
         ++ "int x3 = 5+6;" ++ "\n"
         ++ "int x4 = 7+8;" ++ "\n"

example48 = "int i = 0, j = 1; i < 10"

example48_2 = "; i < 10; i++,j++"

example49 = "int time = 20;" ++ "\n"
         ++ "if (time < 18) {" ++ "\n"
         ++ "  System.out.println(\"Good day.\");" ++ "\n"
         ++ "} else {" ++ "\n"
         ++ "  System.out.println(\"Good evening.\");"
         ++ "}"
      
example50 = "if(true){" ++ "\n"
         ++ unlines (map ("  "++) (lines example47))
         ++ "}" ++ "\n"
         ++ "else{" ++ "\n"
         ++ unlines (map ("  "++) (lines example47))
         ++ "}"

--TODO
example51 = "if(false){} else{}"

example52 = "if(false == true){} else{}"

example52_2 = "if(true){} else {  System.out.println(\"meow\");}"

--TODO
example53 = "if (time < 18) {" ++ "\n"
         ++ "  System.out.println(\"Good day.\");" ++ "\n"
         ++ "} else {" ++ "\n"
         ++ "  System.out.println(\"Good evening.\");}"
         
example54 = "for(int i = 0; i<10; i=i+1){}"

example55 = "for(int i = 0; i<10; i=i+1){System.out.println(i);}"

example56 = "int time = 20;" ++ "\n"
         ++ "if(false == true){} else{}" ++ "\n"
         ++ "for(int i = 0; i<10; i=i+1){System.out.println(i);}"
         
example57 = "while(true){int i = 0; System.out.println(String.format(\"mew\",i));}"

example58 = "while(i < 10){int i = 0; System.out.println(String.format(\"mew\",i));}"

example59 = "try{System.out.println(123456); int i = 0;} catch(Exception e){System.out.println();}"

example60 = "try{System.out.println(123456); int i = 0;} catch(Exception e){} finally{int t = 9;}"

example61 = "return 5;"

example62 = "return;"

example63 = "return meow(beef);"

example64 = "return true;"

example65 = "return \'c\';"

example66 = "return beobeo.meow(beef);"

example67 = "return x[8];"

example68 = "return x;"

example69 = "return \"meow\";"

example70 = "return x.y;"

example71 = "try {" ++ "\n"
         ++ "  int myNumber = 3;" ++ "\n"
         ++ "  System.out.println(myNumber);" ++ "\n"
         ++ "} catch (Exception e) {" ++ "\n"
         ++ "    System.out.println(\"Something went wrong.\");" ++ "\n"
         ++ "}"

example72 = "try {" ++ "\n"
         ++ "  int myNumber = 3;" ++ "\n"
         ++ "  System.out.println(myNumber);" ++ "\n"
         ++ "} catch (Exception e) {" ++ "\n"
         ++ "    System.out.println(\"Something went wrong.\");" ++ "\n"
         ++ "} finally {" ++ "\n"
         ++ "    System.out.println(\"The 'try catch' is finished.\");" ++ "\n"
         ++ "}"

example73 = "int myNumber = 3;" ++ "\n"
         ++ "System.out.println(myNumber);"
         
-------------------------------------------------

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

example93 = "public void boo10(){" ++ "\n"
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
         ++ "  }"
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
          ++ "  }"
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

example107 = example105 ++ example106 ++ example106_2 ++ example108 ++ example109 ++ example110 ++ example111 ++ example112 ++ example113 ++ example114

spaces n = replicate n ' '

banana = ((fst . fromJust) .) . parse
-------------------------------------------------
--TODO: parse list of generics instead of only single generic: in parsing variables, functions
--TODO: parseBinOp for ++, --, += , ....etc
--TODO: parseReturn
--TODO: parse the catch
--TODO: parse lambda expressions
--TODO: parse lambda expressions for assigns
--TODO: parse comments
--TODO: parseAssign for right side is far from complete
