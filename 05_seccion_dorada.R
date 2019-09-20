# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 11/SEPTIEMBRE/2019
# Métodos de acotamiento
  # Métodos de eliminación de regiones
    # Método de seccion dorada

w <- function(x){
  (x-a)/(b-a)
}

seccion_dorada <- function(a = 0, b = 5, epsilon = 0.001, eval = function(x){x^2 + 54/x}){
  solucion <-  c()
  # Paso 1:
  aw <- w(a)
  bw <- w(b)
  Lw <- bw - aw
  k = 1
  
  # Paso 2:
  while (is.null(solucion)) {
    w1 = aw + (0.618)*Lw
    w2 = bw - (0.618)*Lw
    
    # Paso 3
    Lw <- bw - aw
    if (abs(Lw) < epsilon){
      solucion <- c(aw,bw)
    }
  }
  
}

seccion_dorada(n=3)
