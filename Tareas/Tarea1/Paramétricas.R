#install.packages("pracma")
require("pracma")
#install.packages("lattice")
require("lattice")
Newton = function (f,x0,tol,maxiter){
  k=0
  errores = c()
  iteraciones = c()
  Errori=c()
  Errorj=c()
  cat(formatC(c("x0","x1","dx","Error est."), width = -15, format = "f", flag = " "),"\n")
  repeat{
    
    correcion = f(x0)/fderiv(f,x0,n=1,h=0)
    x1 = x0 - correcion
    dx = abs(x1-x0)
    x0 = x1
    errores[k+1]=abs(correcion)
    iteraciones[k+1]=k+1
    k = k+1
    cat(formatC( c(x0,x1,dx,correcion), digits = 7, width = -15, format = "f", flag = " "), "\n")
    if(dx<=tol || k >maxiter)
      break
    
  }
  for(b in 1:(k+1)){
    if(b!=k+1){
      Errori[b]=errores[b]
      Errorj[b]=errores[b+1]
    }
  }
  if(k > maxiter){
    cat("Numero máximo de Iteraciones alcanzado.")
    cat("T tal que a(t)=(0,0,t): ",x1,"  Error Estimado: ", correcion)
  }
  else{
    cat("T tal que a(t)=(0,0,t): ",x1," Error Estimado: ", correcion)
    
  }
  plot(iteraciones,errores,type = "l", xlab = "N iteraciones", ylab= "Error")
  plot(Errori,Errorj, type = "l", xlab= "Error i", ylab = "Error i+1")
  t<-seq(0, 2*pi, length.out=200)
  cloud(z~x+y,data.frame(x=cos(t),y=sin(t)-1, z=t))
}

z=function(t) t
x= function(t) cos(t)
y= function(t) sin(t)-1
Ft= function(t) x(t)-y(t)
Ftp = function(t) D(Ft ~ t)
Newton(Ft,1.6,1e-8,1000)