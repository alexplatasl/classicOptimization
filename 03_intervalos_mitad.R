# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 9/SEPTIEMBRE/2019
# Métodos de acotamiento
  # Métodos de eliminación de regiones
    # Método de Intervalos por la mitad


acotamiento_mitad <- function(a= 0, b=5, epsilon = 0.001, evalua = function(x){x^2 + 54/x}){
  iter <- 0
  solucion <- c()

  # Paso 1:
  xm = (a+b)/2
  L0 <- L <- b-a

  while (abs(L) > epsilon | is.null(solucion)) {
    # Paso 2:
    x1 = a + L/4
    x2 = b - L/4
    fx1 = evalua(x1) 
    fx2 = evalua(x2)
    
    # Paso 3:
    if (fx1 < evalua(xm)){
      b = xm
      xm = x1
      
    }
    # Paso 4:
    else if(fx2 < evalua(xm)){
      a = xm
      xm = x2
      
    }else{
      a = x1
      b = x2
    }
    
    # Paso 5:
    L = b - a
    if (abs(L) < epsilon){
      solucion <- c(a,b)
    }
    iter = iter + 1
    cat("Iteracion:", iter, "\t a:", a, "\t b:", b, "\n")
  }
  solucion
}

# Ejemplo
acotamiento_mitad(epsilon =  0.000001)
