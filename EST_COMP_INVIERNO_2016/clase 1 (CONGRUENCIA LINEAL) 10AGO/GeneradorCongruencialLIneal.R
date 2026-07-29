GCL <-function(nsim, semilla=21849, incremento=1, multiplicador=2456651, M=2^32){
  seq <- semilla
  for(i in 1:nsim){
    new <- (seq[i]*multiplicador + incremento)%%M
    seq <- c(seq, new)
  }
  return(seq/M)
}
x <- GCL(10000)
hist(x)
anterior <- x[-length(x)] # x[1:(length(x)-1)]
siguiente <- x[-1]
plot(anterior, siguiente) # x[2:length(x)]
