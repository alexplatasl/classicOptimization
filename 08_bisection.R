# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 1°/OCTUBRE/2019
# Métodos Basados en Gradiente
  # Método de bisección

bisection <- function(a = 2, b = 5, epsilon = 0.001, eval=function(x){x^2+54/x}){
  # Guarda la solución
  solucion <- c()

  # Contador de iteraciones
  k = 1

  # Pequeños delta para computar la derivada  
  delta_a <- ifelse(a>0.001,0.01*a,0.0001)
  delta_b <- ifelse(b>0.001,0.01*b,0.0001)
  
  # Step 1: Calcular las derivadas de a y b, verificar que tienen signo contrario
  d1_a <- (eval(a+delta_a) - eval(a-delta_a)) / (2*delta_a)
  d1_b <- (eval(b+delta_b) - eval(b-delta_b)) / (2*delta_b)
  
  if (d1_a > 0){
    stop("ERROR: Elige otro punto inicial 'a', tal que su derivada sea negativa\n")
  } else if (d1_b < 0){
    stop("ERROR: Elige otro punto inicial 'b', tal que su derivada sea positiva\n")
  } else if (!(d1_a < 0 & d1_b > 0)){
    stop("ERROR: Elige otros puntos iniciales 'a' y 'b'\n")
  }
    
  while(is.null(solucion)){
    # Step 2: Computa un nuevo punto medio, y su derivada
    z <- (a+b)/2
    delta_z <- ifelse(z>0.001,0.01*z,0.0001)
    d1_z <- (eval(z+delta_z) - eval(z-delta_z)) / (2*delta_z)

    # Step 3: Para el algoritmo o determina que región se elimina
    if (abs(d1_z)<=epsilon){
      cat("Terminado: ",k, "\t---------\t---------\t---------\n")
      solucion <- z
    } else if (d1_z < 0 ){
      a = z
      cat("Iteración: ",k, "\ta: ", a,"\tb: ",b,"\tz: ",z,"\n")
      k = k + 1
    } else if (d1_z > 0){
      b = z
      cat("Iteración: ",k, "\ta: ", a,"\tb: ",b,"\tz: ",z,"\n")
      k = k + 1
    }
  }
  solucion
}

# Ejemplo:
bisection(a=2, b= 5, epsilon = 0.00001)
