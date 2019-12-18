# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 22/OCTUBRE/2019
# # Multivariable optimization algortihms
# ### Direct search method
# #### Método Simplex de Nelder Mead


simplex_method <- function(simplex = list(x1=c(0,0),x2=c(2,0),x3=c(1,1)), gamma = 1.5, beta = 0.5, 
                           eps = 0.001, 
                           eval = function(xs){(xs[1]^2 + xs[2] - 11)^2   + (xs[1] + xs[2]^2 - 7)^2}) {

  # Variables auxiliares
  solucion <- c()
  xls <- list()
  k = 1
  
  while (is.null(solucion)) {
    # Paso 2:
    f_simplex <- sapply(simplex, eval, simplify = F)
    xh <- simplex[[which.max(f_simplex)]]
    xl <- simplex[[which.min(f_simplex)]]
    xg <- simplex[[setdiff(c(1,2,3),c(which.max(f_simplex),which.min(f_simplex)))]]

    xls[[k]] <- c(xl, eval(xl),k)

    # Calcula el centroide y plotea
    xc <- (xl+xg)/2

    # Paso 3:
    # Calcula el punto reflejado
    xr <- abs(2*xc - xh)
    
    xnew <- xr                       # Default
    if (eval(xr) < eval(xl)){
      xnew = abs((1+gamma)*xc - gamma*xh) # Expansión
    }else if (eval(xr) >= eval(xh)){
      xnew = abs((1-beta) * xc + beta*xh) # Contracción 1
    }else if (eval(xg) < eval(xr) & eval(xr) < eval(xh)){
      xnew = abs((1+beta)*xc - beta*xh)   # Contracción 2
    }
    
    # Reemplazos
    fxnew = eval(xnew)
    simplex[[which.max(f_simplex)]] <- xnew

    # Reporta mejor solución
    cat("Iteración:\t", k,"\tMejor solución:\t",paste0("(",paste(round(simplex[[which.max(f_simplex)]],4), collapse = ", "),")"),"\tValor:\t",fxnew,"\n")
    k = k + 1

    Q = sqrt(sum((sapply(simplex, eval) - eval(xc))^2 / 3))

    # Paso 4:
    if (Q <= eps){
      solucion = simplex[[which.min(f_simplex)]]
    }
  }
  solucion <- data.frame(do.call(rbind, xls))
  
  p <- ggplot2::ggplot(solucion) +
    ggplot2::geom_point(ggplot2::aes(x=X1, y= X2, colour = X4, size = 1 / log(X4+1))) +
    ggplot2::geom_path(ggplot2::aes(x=X1, y= X2, colour = X4), 
                       arrow = ggplot2::arrow(angle = 15, ends = "last", 
                                              length = ggplot2::unit(0.10, "inches"), type = "closed")) +
    ggplot2::scale_colour_viridis_c(option="D",direction = 1, begin = 0, end = 0.95) +
    ggplot2::labs(colour = "", group = "", shape = "", fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  print(p)
  solucion
}

solucion <- simplex_method(gamma = 1.00, beta = 0.10, eps = 0.00001)



