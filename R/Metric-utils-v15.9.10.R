
#' Definición de la clase abstracta
#' @param sim:
#' @param obs:
#' @export
get.MSE <-function(sim, obs, ...) UseMethod("get.MSE")

#' Método por default utilizado por la clase get.MSE
#'
#' @param sim: Valor simulado por el modelo. Corresponde a la plantilla del índice ARI.
#' @param obs: Valor observado en los datos reales, u obtenidos mediante un modelo.
#' @return MSE: se retorna el error cuadrático medio calculado por el método.
#' @export
get.MSE.default <- function(sim, obs, ...)
{
  valid.classes <- c("integer", "numeric", "ts")
  if(!all(inherits(sim, valid.classes), inherits(obs, valid.classes)))
    stop("Invalid argument type: 'sim' & 'obs' have to be of class ",
         valid.classes)

  mse <- mean((sim - obs)^2, ...)

  return(mse)
}

#' Método utilizado por la clase get.MSE en caso que se utilice una matriz como estructura de datos.
#'
#' @param obs: Valor observado en los datos reales, u obtenidos mediante un modelo.
#' @return MSE: se retorna el error cuadrático medio calculado por el método.
#' @export

get.MSE.matrix <- function(sim, obs, ...)
{
  # Check that 'sim' and 'obs' have the same dimensions
  if(!all.equal(dim(sim), dim(obs)))
    stop(paste0("Invalid argument: dim(sim) != dim(obs) ",
         "(", "[", paste(dim(sim), collapse = " "), "]", " != ",
         "[", paste(dim(obs), collapse = " "), "]", ")"))

  mse <- colMeans((sim - obs)^2, ...)

  return(mse)
}

#' Método utilizado por la clase get.MSE en caso que se utilice un data frame como estructura de datos.
#'
#' @param obs: Valor observado en los datos reales, u obtenidos mediante un modelo.
#' @return MSE: se retorna el error cuadrático medio calculado por el método.
#' @export
get.MSE.data.frame <- function(sim, obs, ...)
{
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  get.MSE.matrix(sim = sim, obs = obs, ...)

}
