HibridNB = function(f, df, a, b, tol)
{
  i = 1
  c = (a+b) * 0.5
  Errores = c()
  Iteraciones = c()
  
  if(a < c - (f(c)/df(c)))
  {
    if(c - (f(c)/df(c)) < b)
    {
      xi = c - (f(c)/df(c))
    }
  }
  else
    xi = (a+b)*0.5
  while(abs(f(xi))> tol)
  {
    Errores = c(Errores,abs(f(xi)))
    Iteraciones = c(Iteraciones,abs(i))
    if(f(a)*f(xi) < 0)
      b = xi
    else
      a=xi
    c = (a+b)*0.5
    if(a < c - (f(c)/df(c)))
    {
      if(c - (f(c)/df(c)) < b)
      {
        xi = c - (f(c)/df(c))
      }
    }
    else
      xi = (a+b)*0.5
    i= i+1
    
  }
  plot(Iteraciones,Errores, type = "l")
  cat("Raíz: ", xi, "Iteraciones Necesarias: ", i)
}

f = function(x) exp(x)-pi*x
tol = 0.000000001
x0=0
a=0
b=0
df = function(x) exp(x)-pi

HibridNB(f,df,0,2,tol)
