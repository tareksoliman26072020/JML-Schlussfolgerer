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
public int boo26_2(){
  return boo27(-1);
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
public int boo30(int z){
  int x1 = 0;
  int x2 = 0;
  y = 0;
  y1 = 0;
  y2 = 0;
  if(z>=0){
    t1 = 7;
    return t1;
  }
  else{
    t2 = 17;
    return t2;
  }
}
public int boo31(){
  z = 0;
  int x = z;
  return x;
}
public int boo31_2(){
  z = 0;
  int x = z;
  return boo30(1);
}
public int boo32(){
  int x = y1 + y2 + y3;
  return x;
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


