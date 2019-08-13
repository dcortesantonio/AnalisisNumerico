#install.packages("pracma")
require(pracma)
r1= function (x) sin(x)
r2 = function (x) sin(2*x)
R = function (x) r1(x)-r2(x)
biseccion = function(f, a1, b1, tol)
{
  a = a1;
  b = b1;
  i = 0;
  errores = c()
  iteraciones = c()
  Errori = c()
  Errorj = c()
  if( sign(f(a1)) == sign(f(b1)) )
  {
    i=200
    cat("lol")
  }
  repeat
  {
    
    m = a + 0.5 * ( b - a )
    if(f(m) == 0)
    {
      cat("Intersecto en el cuadrante límitado por: ",a1," y ",b1, " es aproximadamente: ",m,"\n")
    }
    if(sign(f(a)) != sign(f(m)))
    {
      b = m
    }
    else
    {
      a = m
    }
    #Calcular el error generado
    estError = ( b - a ) / 2
    errores = c(errores,estError)
    iteraciones = c(iteraciones,i)
    # Hacer update de Index (Iteraciones)
    i = i + 1
    #Condición del ciclo (Tolerancia de Error)
    if( estError < tol || i>200)
    {
      cat("Intersecto en el cuadrante límitado por: ",a1," y ",b1, " es aproximadamente: ",m," con error de: ",estError,"\n")

      break;
    }
  }
  if(i>=200){
    cat("No se encontró una intersección en el cuadrante limitado por: ",a1,b1)
  }
  plot(iteraciones,errores, type = "l", xlab = "N iteraciones",ylab="Error")
  #Error i y Error i+1=j
  for(b in 1:i){
    if(b!=i){
      Errori[b]=errores[b]
      Errorj[b]=errores[b+1]  
    }
  }
  plot(Errori,Errorj, type = "l", xlab = "Error i+1",ylab="Error i")
}

polarIntersect=function(R,r1,r2){
  Sec<-seq(0,2*pi,pi/100)
  biseccion(R,pi/4,pi/2,0.000000001)
  biseccion(R,pi/4,2*pi,0.00000001)
  biseccion(R,0,2*pi,0.00000001)
  polar(Sec,r1(Sec),col = "blue")
  polar(Sec,r2(Sec),col = "green",add = TRUE)
 
}
polarIntersect(R,r1,r2)