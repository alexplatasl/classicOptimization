# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 11/SEPTIEMBRE/2019
# Métodos de acotamiento
  # Métodos de eliminación de regiones
    # Método de seccion dorada

w <- function(x,a,b){
  (x-a)/(b-a)
}

seccion_dorada <- function(a = 0, b = 5, epsilon = 0.001, eval = function(w){25*w^2 + 54/(5*w)}){
  solucion <-  c()
  # Paso 1:
  aw <- w(a,a,b)
  bw <- w(b,a,b)
  Lw <- bw - aw
  k = 1
  
  # Paso 2:
  while (is.null(solucion)) {
    w1 = aw + (0.61803398874988)*Lw
    w2 = bw - (0.61803398874988)*Lw
    
    fw1 <- eval(w1)
    fw2 <- eval(w2)

    #  Rule
    if  (fw1 < fw2){
      aw <- w2
    } else {
      bw <- w1
    }
    Lw <- bw - aw
    # Paso 3
    if (abs(Lw) < epsilon){
      solucion <- c(aw,bw)*5
      cat("Solución:\nIteración:",k,"\ta:", aw*5,"\tb:",bw*5,"\n")
    } else{
      cat("Iteración:",k,"\ta:", aw*5,"\tb:",bw*5,"\n")
      k = k + 1
    }
  }
  solucion
}

seccion_dorada(a=0, b=1, epsilon = 0.001)
