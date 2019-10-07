# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 1°/OCTUBRE/2019
# Métodos Basados en Gradiente
  # Método de la secante

secante <- function(L = 2, R = 5, epsilon = 0.001, eval=function(x){x^2+54/x}){
  # Guarda la solución
  solucion <- c()

  # Contador de iteraciones
  k = 1

  # Pequeños delta para computar la derivada  
  delta_L <- ifelse(L>0.001,0.01*L,0.0001)
  delta_R <- ifelse(R>0.001,0.01*R,0.0001)
  
  # Step 1: Calcular las derivadas de a y b, verificar que tienen signo contrario
  d1_L <- (eval(L+delta_L) - eval(L-delta_L)) / (2*delta_L)
  d1_R <- (eval(R+delta_R) - eval(R-delta_R)) / (2*delta_R)
  
  if (!((d1_L * d1_R) < 0)){
    stop("Cambia los puntos iniciales 'L' y 'R'\n")
  }
  
  while(is.null(solucion)){
    # Step 2: Computa un nuevo punto medio, y su derivada
    z <- R - (((d1_R)* (R-L)) / (d1_R - d1_L))
    delta_z <- ifelse(z>0.001,0.01*z,0.0001)
    d1_z <- (eval(z+delta_z) - eval(z-delta_z)) / (2*delta_z)

    # Step 3: Para el algoritmo o determina que región se elimina
    if (abs(d1_z)<=epsilon){
      cat("Terminado: ",k, "\t-----------\n")
      solucion <- z
    } else if (d1_z < 0 ){
      cat("Iteración: ",k, "\tz: ",z,"\n")      
      L = z
      d1_L <- d1_z
      k = k + 1
    } else if (d1_z > 0){
      cat("Iteración: ",k, "\tz: ",z,"\n")      
      R = z
      d1_R <- d1_z
      k = k + 1
    }
  }
  solucion
}

secante(L=-1, R=10,epsilon = 0.00001)
