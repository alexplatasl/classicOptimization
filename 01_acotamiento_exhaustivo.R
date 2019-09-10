# Optimizaión clásica
# Alejandro Platas López
# 2/septiembre/2019
# Método de acotamiento: búsqueda exhaustiva

acotamiento <- function(a, b, n=10, evalua = function(x){x^2 + 54/x}){
  ## Parámetros
  # a: Límite inferior del intervalo.
  # b: Límite superior del intervalo.
  # n: Número de intervalos (default 10).
  # evalua: Función objetivo (default x² + 54/x).

  # Almacenar solución
  solucion <- c()
  # Contador
  iter <- 1

  # Paso 1
  x1 = a
  delta_x <- (b-a)/n
  x2 = x1 + delta_x
  x3 = x2 + delta_x

  # Paso 3a  
  while(x3 <= b  & length(solucion) == 0){
    fx1 = evalua(x1)
    fx2 = evalua(x2)
    fx3 = evalua(x3)
    # Paso 2
    if( fx1  >= fx2  & fx2 <= fx3 ){
      cat("\nFinalizado en ",iter,"Iteraciones\n")
      solucion <- c(x1,x3)
      return(list(Solucion=solucion))
    }else{
      cat("Iteración:",iter,"\t  x1:",x1,"\t  x2:",x2,"\t  x3:",x3,"\n")
      iter <- iter + 1

      # Actualiza valores
      x1 = x2
      x2 = x3
      x3 = x2 + delta_x
    } # Fin paso 2
  } # Fin paso 3a
  # Paso 3b
  cat("\nFinalizado en ",iter,"Iteraciones: Sin solución\n")
  return(list(Rango =  c(a,b)))
}

# 
acotamiento(0,5,500,  function(x){x^2 + 54/x})
