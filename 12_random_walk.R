# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 22/OCTUBRE/2019
# # Multivariable optimization algortihms
# ### Direct search method
# #### Caminata aleatoria

randomWalk <- function(x1=c(1,1), lambda = 0.5, eps = 0.00001, N = 10000, eval = function(xs){(xs[1]^2 + xs[2] - 11)^2   + (xs[1] + xs[2]^2 - 7)^2}){
  solucion <- c()
  k <- 1
  camina <- FALSE
  # Paso 2: evaluar
  f1 <- eval(x1)

  while (lambda > eps ) {
    # Paso 3: 
    i = 1
    camina = FALSE
    
    while (i<=N & camina == FALSE){
      # Paso 4:
      rnd <- runif(length(x1), -1, 1)
      while (sqrt(sum(rnd^2))>1){
        rnd <- runif(length(x1), -1, 1)
      }
      u = 1 / sqrt(sum(rnd^2))*rnd
      
      # Paso 5: 
      x = x1 + lambda*u
      x = abs(x)

      f = eval(x)
      k = k  + 1      

      # Paso 6: 
      if (f < f1){
        x1 = x
        f1 = f
        camina = TRUE
      }

      # Paso 7:
      i = i + 1

    } # end while i <= N
    
    # Paso 8: 
    lambda = lambda/2

  } # end while lambda > eps

  solucion <- x1
  cat("Evaluaciones: \t", k, "Solucion:\t",solucion,"\tvalor: \t",f1,  "\n")
  solucion
}

randomWalk(lambda = 1.64, N = 200)
