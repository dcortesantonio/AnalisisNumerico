Cuadratica = function (a,b,c){
  #La variación de la fórmula se utilizará en la resta para evitar el inconveniente.
  #--------------------------
  #Método normal (suma)
  x1=-(b+sqrt(b^2-4*a*c))/(2*a)
  #Fórmula racionalizada por el conjugado
  x2=-(2*c)/(b+sqrt(b^2-4*a*c))
  cat("Con 8 decimales.\n")
  options(digits=8)
  cat("Raíz 1: ",x1, " Raíz 2: ",x2,"\n")
  options(digits=16)
  cat("Con 16 decimales.\n")
  cat("Raíz 1: ",x1, " Raíz 2: ",x2)
}
Cuadratica(3,9^12,-3)