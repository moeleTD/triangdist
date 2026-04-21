#-----------Me lo ha chivado la IA-----------

#' @title Density function for the triangular distribution
#' @param x Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of density values.

#-----------Me lo ha chivado la IA-----------
#' @export
dtriang <- function(x, min, max, mode) {
  if (any(min > max) || any(mode < min) || any(mode > max)) {#Añadimos any para que el código sea válido para vectres.
    stop("Error. Reminder: min <= mode <= max")
  }
  
  n <- length(x)
  res <- numeric(n)
  
  min  <- rep_len(min, n)
  max  <- rep_len(max, n)
  mode <- rep_len(mode, n)
  
  #Esto último es SUPER clave para poder trabajar con vectores

  subida <- x >= min & x < mode
  pico   <- x == mode
  bajada <- x > mode & x <= max
  
  res[subida] <- (2 * (x[subida] - min[subida])) / 
    ((max[subida] - min[subida]) * (mode[subida] - min[subida]))
  
  res[pico] <- 2 / (max[pico] - min[pico])
  
  res[bajada] <- (2 * (max[bajada] - x[bajada])) / 
    ((max[bajada] - min[bajada]) * (max[bajada] - mode[bajada]))
  
  return(res)
}

#-----------Me lo ha chivado la IA-----------

#' @title Distribution function for the triangular distribution
#' @param q Vector of quantiles.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of cumulative probabilities.

#-----------Me lo ha chivado la IA-----------
#' @export
ptriang <- function(q, min, max, mode) {
  if (any(min > max) || any(mode < min) || any(mode > max)) {
    stop("Error. Reminder: min <= mode <= max") [cite: 50]
  }
  
  n <- length(q)
  min  <- rep_len(min, n)
  max  <- rep_len(max, n)
  mode <- rep_len(mode, n)
  
  res <- rep(1, n)
  
  izq <- q < min
  subida    <- q >= min & q < mode
  bajada    <- q >= mode & q <= max
  
  res[izq] <- 0
  
  res[subida] <- (q[subida] - min[subida])^2 / 
    ((max[subida] - min[subida]) * (mode[subida] - min[subida]))
  
  res[bajada] <- 1 - (max[bajada] - q[bajada])^2 / 
    ((max[bajada] - min[bajada]) * (max[bajada] - mode[bajada]))
  
  return(res)
}

#-----------Me lo ha chivado la IA-----------

#' @title Quantile function for the triangular distribution
#' @param p Vector of probabilities.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of quantiles.

#-----------Me lo ha chivado la IA-----------
#' @export
qtriang <- function(p, min, max, mode) {

  if (any(p < 0 | p > 1)) stop("Error: p must be between 0 and 1")
  if (any(min > max) || any(mode < min | mode > max)) {
    stop("Error: min <= mode <= max")
  }
  
  n <- length(p)
  min  <- rep_len(min, n)
  max  <- rep_len(max, n)
  mode <- rep_len(mode, n)
  
  res <- numeric(n)
  
  p_corte <- (mode - min) / (max - min)
  
  izq <- p < p_corte
  der <- p >= p_corte
  
  res[izq] <- min[izq] + sqrt(p[izq] * (max[izq] - min[izq]) * (mode[izq] - min[izq]))
  res[der] <- max[der] - sqrt((1 - p[der]) * (max[der] - min[der]) * (max[der] - mode[der]))
  
  return(res)
  }
  
#-----------Me lo ha chivado la IA-----------

#' @title Random generation for the triangular distribution
#' @param n Number of observations.
#' @param min Lower limit (a).
#' @param max Upper limit (b).
#' @param mode Mode (c).
#' @return A numeric vector of random deviates.

#-----------Me lo ha chivado la IA-----------
#' @export
rtriang <- function(n, min, max, mode){
  res <- qtriang(runif(n), min, max, mode)
  return(res)
}