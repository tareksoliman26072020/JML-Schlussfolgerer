/*@ normal behavior
  @ requires True;
  @ assignable \nothing;
  @ ensures \result=5;
  @*/
public /*@ pure @*/ int boo21(){
  return 5;
}

/*@ normal behavior
  @ requires True;
  @ assignable \nothing;
  @ ensures \result=5;
  @*/
public /*@ pure @*/ int boo22(){
  return boo21();
}

/*@ normal behavior
  @ requires True;
  @ assignable \nothing;
  @ ensures \result=5;
  @*/
public /*@ pure @*/ int boo22_2(){
  int x = boo21();
  return x;
}

/*@ normal behavior
  @ requires True;
  @ assignable \nothing;
  @ ensures \result=3+5;
  @*/
public /*@ pure @*/ int boo23(){
  int x = 3 + boo21();
  return x;
}

/*@ exceptional behavior
  @ requires (boo25->i)>10;
  @ signals Exception;
  @ also
  @ normal behavior
  @ requires True;
  @ assignable \nothing;
  @ ensures \result=3+6;
  @*/
public int boo24(){
  int x = 3 + boo25(5);
  return x;
}

/*@ exceptional behavior
  @ requires i>10;
  @ signals Exception;
  @ also
  @ normal behavior
  @ requires i<=10;
  @ assignable \nothing;
  @ ensures \result=6;
  @*/
public int boo25(int i){
  if(i>10){
    throw new Exception("meow");
  }
  else{
    return 6;
  }
}

/*@ normal behavior
  @ requires (boo27->i)>=0;
  @ assignable \nothing;
  @ ensures \result=(boo27->i);
  @ also
  @ normal behavior
  @ requires (boo27->i)<0;
  @ assignable \nothing;
  @ ensures \result=(-1)*(boo27->i);
  @*/
public /*@ pure @*/ int boo26(){
  return boo27(5);
}

/*@ normal behavior
  @ requires i>=0;
  @ assignable \nothing;
  @ ensures \result=i;
  @ also
  @ normal behavior
  @ requires i<0;
  @ assignable \nothing;
  @ ensures \result=(-1)*i;
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
  @ requires x==3;
  @ signals Exception;
  @ also
  @ normal behavior
  @ requires x!=3;
  @ assignable \nothing;
  @ ensures \result=1;
  @ also
  @ normal behavior
  @ requires (boo27->i)>=0;
  @ assignable \nothing;
  @ ensures \result=(boo27->i);
  @ also
  @ normal behavior
  @ requires (boo27->i)<0;
  @ assignable \nothing;
  @ ensures \result=(-1)*(boo27->i);
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
  @ requires i<=y && j==y;
  @ assignable \nothing;
  @ ensures \result=i;
  @ also
  @ exceptional behavior
  @ requires i<=y && j!=y && i==y;
  @ signals Exception;
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