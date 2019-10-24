# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 15/OCTUBRE/2019
# # Multivariable optimization algortihms
# ### Direct search method
# #### Box’s Evolutionary Optimization Method

hiper <- function(punto.inicial, distancias){
  points <- list()
  DIST <- unique(c(distancias*-1, distancias))
  DIST <- expand.grid(DIST,DIST)
  for (i in 1:nrow(DIST)){
    points[[i]] <- punto.inicial + unname(unlist(DIST[i,]))
  }
  points
  points[[i+1]] <- punto.inicial
  do.call(rbind, points)
}

hiper(c(3.5,1.5),c(1,1)/2)

norma <- function(vector){
  sqrt(sum(vector^2))
}
norma(c(2,2))


evop <- function(x0=c(1,1), delta = c(2,2),  eps = 0.001, eval = function(xs){(xs[1]^2 + xs[2] - 11)^2   + (xs[1] + xs[2]^2 - 7)^2}){
  solucion <- c()
  k = 1
  # Paso 1:
  xb <- x0

  while (is.null(solucion)) {

  # Paso 2:
  if (norma(delta) < eps){
    solucion <- x0
  }else{
    hipercubo <- hiper(xb,delta/2)
  }
  
  # Paso 3:
  evaluaciones <- apply(hipercubo, 1, eval)
  xb <- hipercubo[which.min(evaluaciones),]
  
  # Paso 4:
  if (xb == x0){
    delta = delta /2
  }else{
    x0 = xb
  }
  cat("Iteracion: \t", k, "Solucion:\t",x0,"\n")
  k = k + 1
  } # end while    
  solucion
}

evop(x0=c(1,1), delta = c(2,2),  eps = 0.001)
