# OPTIMIZACIÓN CLÁSICA
# ALEJANDRO PLATAS LÓPEZ
# 24/SEPTIEMBRE/2019
# Métodos de Estimación-Puntual
  # Método de Estimación Cuadrática Sucesiva

X_ <- function(F1,F2,F3,X1,X2,X3){
  a1 <- (F2 - F1) / (X2 - X1)
  a2 <- (1 / (X3 - X2))*(((F3-F1)/(X3-X1))-a1)
  while (a2 < 0 ){
    random.point <- sample(1:3,1)
    assign(paste0("X",random.point), runif(1,0,5))
    assign(paste0("F",random.point), eval(get(paste0("X",random.point))))
    a2 <- (1 / (X3 - X2))*(((F3-F1)/(X3-X1))-a1)
  }
  ((X1+X2)/2)-(a1/(2*a2))
}

# Ejemplo:
#X_(55,31,27,1,2,3)

metodo_powell <- function(x1=1,delta=1,TOL1=0.001,TOL2=0.1,eval=function(x){x^2+54/x}){
  # x1: Es un punto inicial
  # delta: Es el tamaño de paso
  # TOL1: Es la tolerancia para comparar el valor de las funciones
  # TOL2: Es la tolerancia para comparar los valores de x
  # eval: Es la función a optimizar

  # Almacenar solución en:
  solucion <- c()

  # Contador de iteraciones:
  iter <- 1

  # Paso 1: Se calcula x2
  x2  <- x1 + delta

  # Paso 2: Se evaluan los puntos x1 y x2
  fx1 <- eval(x1)
  fx2 <- eval(x2)  
  
  # Paso 3: Se comparan los puntos evaluados en 2, se genera x3 y se evalua
  if (fx1 > fx2){
    x3 <- x1 + (2*delta)
  }else{
    x3 <- x1 - delta
  }
  fx3 <- eval(x3)
  
  while (is.null(solucion)){
    # Paso 4: Determinar Fmin y su respectivo Xmin
    fmin <- min(c(fx1,fx2,fx3))
    xmin <- c(x1,x2,x3)[which.min(c(fx1,fx2,fx3))]

    # Paso 5: Calcular x testada con los xi puntos hasta el momento
    x_ <- X_(fx1,fx2,fx3,x1,x2,x3)
    fx_ <- eval(x_)

    # Paso 6: Revisar tolerancias
    if (abs(fmin - fx_) <= TOL1 & abs(xmin - x_) <= TOL2){
      cat("==========\tTerminado\t==========\n")
      solucion <- c(x1,x2,x3,x_)[which.min(c(fx1,fx2,fx3,fx_))]
    }

    # Paso 7: Eliminación del peor punto y re-etiquetado
    best <- c(x1,x2,x3,x_)[which.min(c(fx1,fx2,fx3,fx_))]
    best.points <- c(x1,x2,x3,x_)[order(c(fx1,fx2,fx3,fx_))][1:3]
    if (best==sort(best.points)[1] | best==sort(best.points)[3]){
      # Si el mejor punto está en un extremo:
      x3 <- sort(best.points)[3]; fx3 <- eval(x3)
      x2 <- sort(best.points)[2]; fx2 <- eval(x2)
      x1 <- sort(best.points)[1]; fx1 <- eval(x1)
    }else{
      # El mejor punto puede encerrarse entre otros dos puntos:
      x3 <- max(best.points); fx3 <- eval(x3)
      x2 <- best            ; fx2 <- eval(x2)
      x1 <- min(best.points); fx1 <- eval(x1)
    }
    # Imprime el estatus actual del algoritmo
    cat("Iteración: ",iter, "\tBest point:\t",best,"\n")
    iter <- iter + 1
    }
  solucion
}

metodo_powell(x1= 10, TOL1 = 0.00001, TOL2 = 0.00001)
