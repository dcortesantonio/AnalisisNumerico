Secante = function (f,x0,x1,tol,maxiter)
{
  y0=f(x0)
  y1=f(x1)
  i=1
  Iteraciones = c()
  Errores = c()
  Errori = c()
  Errorj = c()
  cat(formatC(c("i","x_1","x_2","Error est."), width = -15, format = "f", flag = " "),"\n")
  while(abs(x1-x0)>tol&&i<=maxiter)
  {
    i=i+1
    pendiente = (y1-y0)/(x1-x0)
    if(pendiente==0) return(cero=NA,f.cero = NA, iter=k,ErrorEst=NA)
    x2=x1-y1/pendiente
    y2=f(x2)
    x0 = x1;y0=y1
    x1=x2;y1=y2
    Iteraciones = c(Iteraciones, i)
    Error = abs(x1-x0)
    Errores = c(Errores, Error)
    cat(formatC( c(i,x1,x2,Error), digits = 8, width = -15, format = "f", flag = " "), "\n")
  }
  if(i>maxiter)
  {
    cat("N?mero m?ximo de iteraciones alcanzado")
    cat("N?mero de iteraciones: ",i, "X1: ", x1, "X2: ", x2, "Error: ", abs(x1-x0))
  }
  cat("N?mero de iteraciones: ",i, "X1: ", x1, "X2: ", x2, "Error: ", abs(x1-x0))
  plot(Iteraciones,Errores, type = "l", xlab = "No. Iteraciones",ylab="Error")
  #Errores Ei vs Ei+1
  for(b in 1:i){
    if(b!=i){
      Errori[b]=Errores[b]
      Errorj[b]=Errores[b+1]  
    }
  }
  plot(Errori,Errorj, type = "l", xlab = "Error i+1",ylab="Error i")
  
}
f=function(x) (x-1.35)^3
plot(f,0,10)
Secante(f,0,2,1e-8,1000)