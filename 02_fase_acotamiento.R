# Optimizaión clásica
# Alejandro Platas López
# 2/septiembre/2019
# Método de acotamiento: Método de fase de acotamiento


fase_acotamiento <- function(x0 = 0.6, delta = 0.5, evalua = function(x){x^2 + 54/x}){
  solucion <- NULL
  x <- x0
  
  #  Paso 1:
  k = 0

  # Paso 2:
  fx_minus_delta = evalua(x0 - abs(delta))
  fx0 = evalua(x0)
  fx_plus_delta = evalua(x0 + abs(delta))
  if (fx_minus_delta  >= fx0  & fx0 >= fx_plus_delta){
    delta = abs(delta)
  }else if(fx_minus_delta  <= fx0  & fx0 <= fx_plus_delta){
    delta = abs(delta) * -1
  } else{
    stop("Elige un nuevo punto inicial")
  }

  while(is.null(solucion)){
    # Paso 3:
    i = k + 1
    x_k_plus_1 <- x[i] + 2^k*delta
    x_k_plus_1
    x <- c(x,x_k_plus_1)

    # Paso 4:
    if (evalua(x_k_plus_1) < evalua(x[i])){
      k = k + 1
      cat("Iteracion:", k+1, "\tValor de x: ",x[length(x)],"\n")
    }else{
      solucion <- c(x[i-1],x[i+1])
    }
  }
  cat("\nEl mínimo se encuentra en el intervalo:\n")
  sort(solucion)
}

# Ejemplo
fase_acotamiento(x0=0.6, delta = 0.000001)
