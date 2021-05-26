public int foo1(){
  if(true){
    return 0;
  }
  else{
    return 1;
  }
}

public int foo2(){
  return foo1();
}

public foo3(){
  return foo4(4);
}

public foo4(int n){
  if(n>=0){
    return n;
  }
  else{
    return (-1)*n;
  }
}
