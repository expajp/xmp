#include <stdio.h>

double foo(double x){

  if(x < 0){
    return foo((double)(x+1));
  }else if(x < 0.5){
    return (double)(2*x);
  }else if(x < 1.0){
    return (double)(2-2*x);
  }else{
    return foo((double)(x-1));
  }

}

int main(int argc, char* argv[]){
  double x;

  scanf("%lf", &x);

  printf("%f\n", foo(x));

  return 0;

}
