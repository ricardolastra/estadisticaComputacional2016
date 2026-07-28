#install.packages("ggplot2") 
#METODO DE LA FUNCION INVERSA

Finv <- function(u, lambda) {return(-log(1-u)/lambda)}

set.seed(20160817)
nsim <- 1000
lambda <- .2

U <- runif(nsim)

X <- Finv(U, lambda)
# Find(U[991], lambda)

hist(X, breaks=50)

#Usando la funcion rexp propia de R
X2 <- rexp(nsim, rate=lambda)
hist(X2, breaks=50)

library(plotly)
#Solo un ejemplo, diagrama de caja:
#p <- plot_ly(midwest, x = percollege, color = state, type = "box")

p <- plot_ly(x = X, type = "histogram", opacity = .3) %>% add_trace(x = X2, type = "histogram", opacity = .3)

p
