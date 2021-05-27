public int boo21(){
  return 5;
}
public int boo22(){
  return boo21();
}
public int boo22_2(){
  int x = boo21();
  return x;
}
public int boo23(){
  int x = 3 + boo21();
  return x;
}
public int boo24(){
  int x = 3 + boo25(5);
  return x;
}
public int boo25(int i){
  if(i>10){
    throw new Exception("meow");
  }
  else{
    return 6;
  }
}
public int boo26(){
  return boo27(5);
}
public int boo27(int i){
  if(i >= 0){
    return i;
  }
  else{
    int res = -1 * i;
    return res;
  }
}
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
public boolean boo29(){
  if(true){
    return false;
  }
  else{
    return true;
  }
}
public void boo30(){
  int x = 0;
  int y = 0;
}
public void boo31(){
  int x = 0;
  int y = 0;
  int z = boo32();
}
public int boo32(){
  throw new Exception("wof");
}
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

