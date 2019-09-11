# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 11/SEPTIEMBRE/2019
# Métodos de acotamiento
  # Métodos de eliminación de regiones
    # Método de Intervalos por la mitad

fibonacci <- function(n){
  a <- 0
  b <- 1
  for (i in 1:n){
    temp <- b
    b <- a
    a <- a + temp
  }
  return(a)
}

#fibonacci(4)

acotamiento_fibonacci <- function(a = 0, b = 5, n = 3, k = 2, eval = function(x){x^2 + 54/x}){
  solucion <- c()
  iter = 1
  # Paso 1: 
  L = b - a
  while (is.null(solucion)){
    # Paso 2:
    Lk = (fibonacci(n - k + 2) / fibonacci(n + 2)) * L
    x1 <- a + Lk
    x2 <- b - Lk

    # Paso 3: 
    if (exists("temp")){
      # Find x1
      idx <- which(x1==temp$xs)
      if (length(idx)>0){
        fx1 <- temp[idx,2]
      }else{
        fx1 <- eval(x1)
      }
      # Find x2
      idx <- which(x2==temp$xs)
      if (length(idx)>0){
        fx2 <- temp[idx,2]
      }else{
        fx2 <- eval(x2)
      }            
    }else{
      fx1 <- eval(x1)
      fx2 <- eval(x2)
    }
    temp <- data.frame(xs=c(x1,x2),fxs=c(fx1,fx2))

    #  Rule
    if (fx1 > fx2){
      a = x1
    }else if (fx1 < fx2){
      b = x2
    }else{
      a = x1
      b = x2
    }
    
    # Paso 4
    if (k == n){
      cat("Iteración:",iter,"\ta:", a,"\tb:",b,"\n")
      solucion <- c(a,b)
    }else{
      cat("Iteración:",iter,"\ta:", a,"\tb:",b,"\n")
      iter = iter + 1
      k = k + 1
    }
  } # End while
  solucion
}

acotamiento_fibonacci(n=3)
