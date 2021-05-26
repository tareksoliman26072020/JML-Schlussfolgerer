/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 0;
  @ also
  @ normal behavior
  @ requires false;
  @ assignable \nothing;
  @ ensures \result = 1;
  @*/
public /*@ pure @*/ int foo1(){
  if(true){
    return 0;
  }
  else{
    return 1;
  }
}

/*@ normal behavior
  @ requires true;
  @ assignable \nothing;
  @ ensures \result = 0;
  @*/
public /*@ pure @*/ int foo2(){
  return foo1();
}

/*@ normal behavior
  @ requires foo4$n >= 0;
  @ assignable \nothing;
  @ ensures \result = foo4$n;
  @*/
public /*@ pure @*/ foo3(){
  return foo4(4);
}

/*@ normal behavior
  @ requires n >= 0;
  @ assignable \nothing;
  @ ensures \result = n;
  @ also
  @ normal behavior
  @ requires n < 0;
  @ assignable \nothing;
  @ ensures \result = -1 * n;
  @*/
public /*@ pure @*/ foo4(int n){
  if(n >= 0){
    return n;
  }
  else{
    return -1 * n;
  }
}

