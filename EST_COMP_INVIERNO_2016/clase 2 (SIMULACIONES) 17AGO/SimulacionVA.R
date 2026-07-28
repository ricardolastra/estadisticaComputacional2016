Finv <- function(u, lambda) {return(-log(1-u)/lambda)}

set.seed(20160817)
nsim <- 1000
lambda <- .2

U <- runif(nsim)

X <- Finv(U, lambda)
# Finv(U[991], lambda)

hist(X)
hist(X, breaks=50)

X2 <- rexp(nsim, rate=lambda)
hist(X2, breaks=50)


library(plotly)
plot_ly(x=X, type="histogram", opacity=.3) %>% 
  add_trace(x=X2, type="histogram", opacity=.3)
