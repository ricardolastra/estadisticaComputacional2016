
n_sim <- 1000
n_personas <- 23
#1
experimentos <- replicate(n_sim, {
  bdays <- sample.int(365, n_personas, replace=TRUE)
  exito <- (anyDuplicated(bdays)>0)
  exito
})
prob_exito <- sum(experimentos)/n_sim

#2
personas_necesarias <- replicate(n_sim, {
  i <- 1
  bdays <- sample.int(365, 1)
  while(anyDuplicated(bdays)==0){
    bdays <- c(bdays, sample.int(365, 1))
    i <- i + 1
  }
  i
})
mean(personas_necesarias)

prob_exito <- mean(experimentos)
prob_exito
