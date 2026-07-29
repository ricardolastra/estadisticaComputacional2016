GCL <-function(nsim, semilla=160167, incremento=1, multiplicador=245661, M=2^32){
  seq <- semilla
  for(i in 1:nsim){
    new <- (seq[i]*multiplicador + incremento)%%M
    seq <- c(seq, new)#c es concatenar
  }
  return(seq/M)
}
x <- GCL(10000)
hist(x)
anterior <- x[-length(x)] # x[1:(length(x)-1)]
siguiente <- x[-1] # x[2:length(x)]
plot(anterior, siguiente) 