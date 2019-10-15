# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 9/OCTUBRE/2019
# Métodos Basados en Gradiente
  # Búsqueda cúbica
options("scipen"=100, "digits"=15)

busqueda_cubica <- function(x0 = 1, step  = 0.5, epsilon1 = 0.001, epsilon2 = 0.001, eval=function(x){x^2+54/x}){
  # AUXILIARES:
  # Almacenar la solución
  solucion <- c()
  # Almacenar la iteración
  iter <- 1
  # Almacenar el valor candidato x1
  x_1 <- c()

  # PASO 1:
  k = 0
  delta_x <- ifelse(x0 > 0.001, 0.01*x0, 0.0001)
  d1_x0 <- (eval(x0+delta_x) - eval(x0-delta_x)) / (2*delta_x)
  if (d1_x0 > 0){
    step = -step
  }

  while (is.null(x_1)) {
    # PASO 2:
    assign(paste0("x",k+1), get(paste0("x",k))+ 2^k*step)
  
    # PASO 3:
    delta_xk <- ifelse(get(paste0("x",k+1)) > 0.001, 0.01*get(paste0("x",k+1)), 0.0001)
    assign(paste0("d1_x",k+1),
         (eval(get(paste0("x",k+1))+delta_xk) - eval(get(paste0("x",k+1))-delta_xk)) / (2*delta_xk))
  
    if ( get(paste0("d1_x",k+1)) * get(paste0("d1_x",k)) <= 0 ){
      x_1 <- get(paste0("x",k))
      x_2 <- get(paste0("x",k+1))
      
      f1 <- eval(x_1)
      f2 <- eval(x_2)
      
      d_f1 <- get(paste0("d1_x",k))
      d_f2 <- get(paste0("d1_x",k+1))
    }else{
      k = k + 1
    }
  }

  while (is.null(solucion)) {
    # PASO 4:
    z <- ((3*(f1-f2))/(x_2 - x_1)) + d_f1 + d_f2
    w <- ifelse(x_1<x_2,  (z^2 - d_f1*d_f2)^(1/2), -(z^2 - d_f1*d_f2)^(1/2))
    mu <- (d_f2+w-z)/(d_f2-d_f1+2*w)
    x_bar <- ifelse(mu<0,x_2, 
                    ifelse(mu>=0 & mu<=1,x_2-mu*(x_2-x_1),
                           ifelse(mu>1,x_1))
                    )
    # PASO 5:
    if (eval(x_bar) < eval(x_1)){
      # GOTO PASO 6
    }else{
      while ( eval(x_bar) > eval(x_1) ){
        #cat("xbar: ",eval(x_bar),"x1:", eval(x_1),"\n")
        x_bar = x_bar - (0.5 * (x_bar - x_1))
      }
    }
      
    # PASO 6:
    delta_xbar <- ifelse(x_bar>0.001, 0.01*x_bar, 0.0001)
    d1_xbar <- (eval(x_bar+delta_xbar) - eval(x_bar-delta_xbar)) / (2*delta_xbar)
  
    if (abs(d1_xbar) <= epsilon1 & abs((x_bar-x_1)/x_bar) <= epsilon2 ){
      solucion = x_bar
      cat("Terminado: ",iter, "\tx_bar:\t",x_bar,"\n")
    } else if((d1_xbar*d_f1) < 0){
      x_2 <- x_bar
      f2 <- eval(x_2)
      d_f2 <- d1_xbar
      cat("Iteración: ",iter, "\tx_bar:\t",x_bar,"\n")
      iter <- iter + 1
    }else{
      x_1 <- x_bar
      f1 <- eval(x_1)
      d_f1 <- d1_xbar
      cat("Iteración: ",iter, "\tx_bar:\t",x_bar,"\n")
      iter <- iter + 1
    }
  }    
  solucion
}

busqueda_cubica(x0=1, step = 0.5, epsilon2 = 0.001)
