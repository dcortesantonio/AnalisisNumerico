Newton = function (f,fp,x0,tol,maxiter)
{
  k=0
  repeat{
    correcion = f(x0)/fp(x0)
    x1 = x0 - correcion
    dx = abs(x1-x0)
    x0 = x1
    k = k+1
    if(dx<=tol || k >maxiter)
      break
    
  }
  if(k > maxiter){
    cat("Numero máximo de Iteraciones alcanzado.")
    cat("Iteraciones=", k, " X: ",x1," f(x):",f(x1)," h: ", correcion)
  }
  else{
    cat("Iteraciones=", k, " X: ",x1," f(x):",f(x1)," h: ", correcion)
    
  }
}
Newtonv2=function(f,fp,a,b,tol,maxiter)
{
  i=0
  h=0
  repeat
  {
    i=i+1
    c=(a+b)/2
    if(a<(c-f(c)/fp(c))&&(c-f(c)/fp(c))<b){
      h=f(c)/fp(c)
      x1=c-h
    }
    else
      x1=(a+b)/2
    if(abs(f(x1))<tol || i>maxiter) break;
    if(f(a)*f(x1)<0){
      b=x1
    } else{
      a=x1
    }
  }
  if(i > maxiter){
    cat("Numero máximo de Iteraciones alcanzado.")
    cat("Iteraciones=", i, " X: ",x1," f(x):",f(x1)," h: ", h)
  }
  else{
    cat("Iteraciones=", i, " X: ",x1," f(x):",f(x1)," h: ", h)
    
  }
}
Newtonv3=function(f,fp,a,b,tol,maxiter){
  flag = FALSE
  i=1
  h=0
  for(i in 1:2){
    x=(a+b)/2
    if(f(x)==0 || abs(f(x))<tol){
      flag = TRUE
      break
    } 
    if(f(a)*f(x)<0) {
      b=x
    } else{
      a=x
    }
  }
  if(flag != TRUE){
    
    repeat{
      i=i+1
      h=(f(x)/fp(x))
      x2=x-h
      if(f(x2)<tol || i>maxiter){
        x=x2
        break
      } 
      x=x2
    }
  }
  cat("Iteraciones= ",i, "X: ",x, "f(x): ",f(x), "h: ", h)
}
f= function(x) 0.2*sin(16*x)+1.75
fp= function(x) 3.2*cos(16*x)
Newton(f,fp,1.5,1e-8,4000)
Newtonv2(f,fp,1,2,1e-8,4000)
Newtonv3(f,fp,1,2,1e-8,4000)