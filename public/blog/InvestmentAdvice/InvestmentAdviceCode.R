mu = 1.2
sigma = .3

EReturn = function(alpha,beta){
	mu1 = 1+alpha*(mu-1)
	mu2 = 1+beta*(mu-1)
	return(mu1*mu2)
}



SDReturn = function(alpha,beta){
	mu1 = 1+alpha*(mu-1)
	mu2 = 1+beta*(mu-1)
	return(sqrt((alpha^2*sigma^2+mu1^2)*(beta^2*sigma^2+mu2^2)-mu1^2*mu2^2))	
}


N = 20
alpha = runif(N)
beta = runif(N)
plot(NULL,xlim=c(1,mu^2),ylim=c(0,sqrt((sigma^2+mu^2)^2-mu^4)))
for(i in 1:N){
	points(EReturn(alpha[i],beta[i]),SDReturn(alpha[i],beta[i]),pch=as.character(i))
}



#######Shows that sometimes, Pareto improvements are possible by going to extremes (rather than centrist)
for(j in 1:T){
	mu = 1 + rexp(1)
	sigma = runif(1,0,mu)
	for(i in 1:N){
		alpha = runif(1)
		beta = runif(1)
		x = EReturn(alpha,beta)
		gamma = (sqrt(x)-1)/(mu-1)
		if(gamma > 1) print("Error 1")
		if(abs(EReturn(gamma,gamma)-x)>exp(-8)) print("Error 2")
		if(SDReturn(alpha,beta) < SDReturn(gamma,gamma)) print(c(mu,sigma,alpha,beta))
	}
}







