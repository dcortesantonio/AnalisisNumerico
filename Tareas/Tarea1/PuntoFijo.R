#Algoritmo punto fijo 
PuntoFijo = function(f, x0, tol, numIteraciones){
  k=1
  errores = c()
  iteraciones = c()
  Errori=c()
  Errorj=c()
  repeat{
    x1=f(x0)
    dx=abs(x1-x0)
    x0=x1
    #Imprimir estado
    cat("x",k, "= ", x1,"\n")
    iteraciones[k+1]=k+1
    errores[k+1]=dx
    k=k+1
    #Hasta
    if(dx< tol || k>numIteraciones) break;
  }
  #Mensaje salida 
  if(dx>tol){
    cat("No hubo convergencia  ")
    #return(NULL)
  }else{
    cat("x* es aproximadamente ", x1, "con error menor que ",tol," en ",k," iteraciones.")
  }
  for(b in 1:(k+1)){
    if(b!=k+1){
      Errori[b]=errores[b]
      Errorj[b]=errores[b+1]
    }
  }
  plot(iteraciones,errores,type = "l", xlab = "N iteraciones", ylab= "Error")
  plot(Errori,Errorj, type = "l", xlab= "Error i", ylab = "Error i+1")
}
f= function(x) exp(x)/pi
PuntoFijo(f,1,1e-8,100)
