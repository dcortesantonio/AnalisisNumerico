#funcion representa el polinomio a evaluar
#g representa el máximo exponente del polinomio
#x0 representa el x0 del polinomio
Horner = function (funcion, g, x0){
  resultado=funcion[1]
  s=0
  p=0
  for(i in 2:(g+1)){
    resultado= resultado*x0 + funcion[i]
    if(funcion[i]!=0){
      s=s+1
    }
    p=p+1
  }
  cat("El resultado del polinomio es: ", resultado, " en ",s,"sumas y ",p," productos.")
}
funcion<-c(2,0,-3,3,-4)
Horner(funcion,4,-2)