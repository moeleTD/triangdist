
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