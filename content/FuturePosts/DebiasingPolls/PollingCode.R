n = 10000
a = .1
V = sample(c(rep(1,a*n),rep(0,(1-a)*n)),n)
k = 100
p = runif(n)+20*runif(n)*(1-V)
p = k*p/sum(p)



nT = 1000
R = matrix(rbinom(n*nT,1,rep(p,nT)),nrow=n)

MSE = Vectorize(function(alpha){
	gamma = sum(p^(alpha))*p^(-alpha)/k
	W = matrix(rep(gamma*V,nT),nrow=n)
	Z = apply(R*W,2,mean)
	return(mean((Z-a)^2))
})

MAD = Vectorize(function(alpha){
	gamma = sum(p^(alpha))*p^(-alpha)/k
	W = matrix(rep(gamma*V,nT),nrow=n)
	Z = apply(R*W,2,mean)
	return(mean(abs(Z-a)))
})

M = Vectorize(function(alpha){
	gamma = sum(p^(alpha))*p^(-alpha)/k
	W = matrix(rep(gamma*V,nT),nrow=n)
	Z = apply(R*W,2,mean)
	return(mean(Z))
})

SD = Vectorize(function(alpha){
	gamma = sum(p^(alpha))*p^(-alpha)/k
	W = matrix(rep(gamma*V,nT),nrow=n)
	Z = apply(R*W,2,mean)
	return(sd(Z))
})

plot(MSE,xlim=c(0,2))
plot(SD,xlim=c(0,2))

plot(M,xlim=c(0,2))
abline(h=a,v=1)