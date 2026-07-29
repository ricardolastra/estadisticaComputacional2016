#REGLA DEL TRAPECIO
limits <- c(0, 1)
eps <- .01
fun <- function(x) {
  x^2
}
trapecio <- function(limits, fun, eps=1e-6){
  #limits es un vector de tamaño 2
  #eps es el tamaño de cada subintervalo
  x_i <- seq(
    from = limits[1],
    to = limits[2],
    by = eps
  )
  f_i <- sapply(x_i, fun)
  eps/2*sum(f_i[-length(x_i)] + f_i[-1])
}
limits_list <- list(c(0, 1), c(-1, 0))
trapecio <- function(limits_list, fun, eps=1e-6){
  #
}