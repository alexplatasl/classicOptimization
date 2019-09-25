# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 25/SEPTIEMBRE/2019
# Métodos Basados en Gradiente
  # Método Newton-Raphson

deltaX <- function(x){ 
  if (x>0.001){0.01*x}else{0.0001}
}

newton_raphson <- function(x = 1, epsilon = 0.001,eval=function(x){x^2+54/x}){
  solucion <- c()
  # Paso 1:
  k = 1
  delta_x <- deltaX(x)
  d1_x <- (eval(x+delta_x) - eval(x-delta_x)) / (2*delta_x)
  
  while(is.null(solucion)){
  # Paso 2:
    d2_x <- (eval(x + delta_x) - 2*eval(x) + eval(x-delta_x) ) / delta_x^2
  # Paso 3:
    x <- x - (d1_x / d2_x)
    d1_x <- (eval(x+delta_x) - eval(x-delta_x)) / (2*delta_x)
  # Paso 4:
    if (abs(d1_x) < epsilon){
      cat("Terminado: ",k, paste0("\tx(",k+1,"):\t"),x,"\n")
      solucion <- x
    }else{
      cat("Iteración: ",k, paste0("\tx(",k+1,"):\t"),x,"\n")
      k = k + 1
    }
  }
  solucion
}

newton_raphson(x=0.01, epsilon = 0.001)
