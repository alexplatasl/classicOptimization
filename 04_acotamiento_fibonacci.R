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

acotamiento_fibonacci <- function(a = 0, b = 5, n = 3, k = 2, eval = function(x){x^2 + 54/x}, trace = FALSE){
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
      if (trace) cat("Iteración:",iter,"\ta:", a,"\tb:",b,"\n")
      solucion <- c(a,b)
    }else{
      if (trace) cat("Iteración:",iter,"\ta:", a,"\tb:",b,"\n")
      iter = iter + 1
      k = k + 1
    }
  } # End while
  solucion
}

# acotamiento_fibonacci(n=3)

# # Ejercicio: Direcciones conjugadas de powell
# acotamiento_fibonacci(a=-3, n=25, eval = function(a){ (a^2-7)^2 + (a+9)^2})
# 
# (x1^2 + x2 - 11)^2 + (x1 + x2^2 - 7)^2
# 
# acotamiento_fibonacci(a=-3, n=25, eval = function(a){ (2.08^2 + (4+a) -11)^2 + (2.08 + (4+a)^2 -7)^2})
# acotamiento_fibonacci(a=-3, n=25, eval = function(a){ ((a+2.08)^2+2.408-11)^2 + ((a+2.08)+2.408^2-7)^2 })
# acotamiento_fibonacci(a=-3, n=25, eval = function(a){ ((2.08 + a*1)^2+(2.408+a*0)-11)^2 + 
#     ((2.08+a*1)+(2.408+a*0)^2-7)^2 })
# acotamiento_fibonacci(a=-3, n=25, eval = evaluar)

# c(2.88,2.408) - c(2.08, 4)

#---------------------------------------------------------------------------------------------
# Comparación de exactitud de métodos
# n = seq(5,20,5)
# Ln_half = (0.5^(n/2))*5
# Ln_fib = (2 / unlist(lapply(n+1, fibonacci))) * Lo
# Ln_gold = ((0.681)^(n-1))*Lo
# Lo = 5
# Rn_half = Ln_half / Lo
# Rn_fib = Ln_fib / Lo
# Rn_gold = Ln_gold / Lo
