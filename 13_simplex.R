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
    xr <- 2*xc - xh
    
    xnew <- xr                       # Default
    if (eval(xr) < eval(xl)){
      xnew = (1+gamma)*xc - gamma*xh # Expansión
    }else if (eval(xr) >= eval(xh)){
      xnew = (1-beta) * xc + beta*xh # Contracción 1
    }else if (eval(xg) < eval(xr) & eval(xr) < eval(xh)){
      xnew = (1+beta)*xc - beta*xh   # Contracción 2
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
  
  # Plotea recorrido de las xl
  XS <- expand.grid(seq(0,4,by=0.1),seq(0,3,by=0.1))
  XS$fx <- apply(XS, 1, eval)
  names(XS)[1:2] <- c("X1","X2")
  p <- ggplot2::ggplot(XS, ggplot2::aes(X1, X2, z = fx))+
    ggplot2::geom_contour(bins = 15)

  p <- p + ggplot2::ggplot(solucion) +
    ggplot2::geom_point(ggplot2::aes(x=X1, y= X2, colour = X4),size = 2) +
    ggplot2::geom_path(ggplot2::aes(x=X1, y= X2, colour = X4)) +
    ggplot2::scale_colour_viridis_c(option="D",direction = 1, begin = 0, end = 0.95) +
    ggplot2::labs(colour = "", group = "", shape = "", fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  print(p)
  solucion
}

solucion <- simplex_method(gamma = 1.5, beta = 0.5, eps = 0.001)


eval = function(xs){(xs[1]^2 + xs[2] - 11)^2   + (xs[1] + xs[2]^2 - 7)^2}

XS <- expand.grid(seq(0,4,by=0.1),seq(0,3,by=0.1))
XS$fx <- apply(XS, 1, eval)
ggplot2::ggplot(XS, ggplot2::aes(Var1, Var2, z = fx))+
  ggplot2::geom_contour(bins = 15)
