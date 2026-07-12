n = 10
N = 10^5

P = rep(1:19)/20
l = rep(0,length(P))
e = rep(0,length(P))
h = rep(0,length(P))
for(i in 1:19){
	p = P[i]
	k = 1 #Exponential; mean = 1 when a = 0
	a = 1/log(2) #Shifted Power Law; Probability that it exceeds 1 is 2^{-a}, mean is 1/(a-1)
	countL = 0
	countE = 0
	countP = 0
	for(j in 1:N){
		x = rbinom(1,n,p)
		U = runif(1)
		yL = sqrt(-log(U)) #Light Tailed; F(x) =  1- exp(-x^2)
		yE = -log(U) #Exponential; F(x) =  1- exp(-x)
		yP = U^(-1/a) - 1  #Power Law; F(x) = 1- (1+x)^{-a}
		if(x < yL) countL = countL+1
		if(x < yE) countE = countE+1
		if(x < yP) countP = countP+1

	}
	l[i] = countL/N
	e[i] = countE/N
	h[i] = countP/N
}
x = 1 - P + P*exp(-k)
plot(function(x){return(x^n)},xlim=c(min(x),max(x)),ylim=c(0,max(x)^n),ylab=expression(P(S[n] <= Y)),xlab=expression(P(X[i]<=Y)),las=1)
lines(x,l,col='green')
lines(x,e,col='blue')
lines(x,h,col='red')



k = 1 #Exponential; mean = 1 when a = 0
a = 1/log(2) #Shifted Power Law; Probability that it exceeds 1 is 2^{-a}, mean is 1/(a-1)
countL = 0
countE = 0
countP = 0
for(j in 1:N){
	x = rbinom(1,k,p)
	U = runif(1)
	yL = sqrt(-log(U)) #Light Tailed; F(x) =  1- exp(-x^2)
	yE = -log(U) #Exponential
	yP = U^(-1/a) - 1  #Power Law
	if(x < yL) countL = countL+1
	if(x < yE) countE = countE+1
	if(x < yP) countP = countP+1

}

(1-p+p*exp(-k))^n

countL/N
countE/N
countP/N


