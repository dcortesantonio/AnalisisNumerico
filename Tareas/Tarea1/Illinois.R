Illinois=function(f,s,t,e,maxiter)
{
  fs = f(s)
  ft = f(t)
  side=0
  i = 1
  Iteraciones = c()
  Errores = c()
  Errori = c()
  Errorj = c()
  cat(formatC(c("i","x_i","f(x)","Error est."), width = -15, format = "f", flag = " "),"\n")
  
  while(i <= maxiter)
  {
    r = (fs*t-ft*s)/(fs-ft)
    if(abs(t-s)<e*abs(t+s))
      break
    fr = f(r)
    Iteraciones = c(Iteraciones, i)
    Error = abs(t-s)
    Errores = c(Errores, Error)
    cat(formatC( c(i,r,f(r),Error), digits = 8, width = -15, format = "f", flag = " "), "\n")
    
    if(fr * ft > 0)
    {
      t=r
      ft=fr
      if(side==-1)
        fs= fs*0.5
      side = -1
    }
    else if(fs*fr >0)
    {
      s = r
      fs = fr
      if(side == +1){
        ft = ft*0.5
      }
      side = +1
    }
    else
      break
    i = i + 1
  }
  cat("Cero de funcion: ", r, " con error <=", abs(t - s), "Iteraciones: ", i)
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

f= function(x) exp(x)-pi*x
Illinois(f,0,1,1e-6,1000)