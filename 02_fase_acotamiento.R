# Optimizaión clásica
# Alejandro Platas López
# 2/septiembre/2019
# Método de acotamiento: Método de fase de acotamiento


fase_acotamiento <- function(x0 = 0.6, delta = 0.5, evalua = function(x){x^2 + 54/x}, trace = FALSE){
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
    assign( paste0("x", k+1), get(paste0("x",k)) + 2^k*delta)

    # Paso 4:
    if (evalua( get(paste0("x",k+1)) ) < evalua( get(paste0("x",k))) ){
      k = k + 1
      if (trace) cat("Iteracion:", k, "\tx: ", get(paste0("x",k)) , "\tf(x)", evalua(get(paste0("x",k))) ,"\n")
    }else{
      solucion <- c( get(paste0("x", k-1)),   get(paste0("x", k+1))  )
    }
  }
  if (trace)  cat("\nEl mínimo se encuentra en el intervalo:\n")
  sort(solucion)
}

# Ejemplo
fase_acotamiento(x0=0.6, delta = 0.000001, trace = TRUE)

# Ejercicio direcciones conjugadas de powell
fase_acotamiento(x0=1.55, delta = 0.000001, evalua = function(a){ (a^2-7)^2 + (a+9)^2})
fase_acotamiento(x0=2.6, delta = 0.000001, evalua = function(a){ (2.08^2 + (4+a) -11)^2 + (2.08 + (4+a)^2 -7)^2})
fase_acotamiento(x0=-1.59, delta = 0.000001, evalua = function(a){ ((a+2.08)^2+2.408-11)^2 + ((a+2.08)+2.408^2-7)^2 })
