#Given vectors X,Y such that X is increasing and Y = g(X), returns a discretized function approximating g
approximate = function(X,Y){
	return(Vectorize(function(x){
		i = max(c(which(X < x),1))
 		return(Y[i]) #Could also do an average of Y[i] and Y[i+1], or linear interpolation	
	}))
}

#Given current wealth w, return from safe and risky assets, and value function V, computes expected utility from any given alpha
#Assumes that p is a probability vector of the same length as r, and s and w are scalars
expectedUtility = function(w,s,r,p,V){
 	return(Vectorize(function(alpha){
 		 return(sum(V((alpha*r+(1-alpha)*s)*w)*p))
 	}))	
}

s = 1  #return from safe asset
r = .6 + c(0:10)/10  #possible returns from risky asset
p = rep(1/length(r),length(r)) #p[i] is probability of return r[i]

a = 0.02; b= 650;
N = 250
W = b+6*rep(-N:N)/(a*N)   #Different levels of wealth, in a range relevant to U


#U for plot 1
U = Vectorize(function(x){return(1/(1+exp(-a*(x-b))))})   #Utility of wealth function

#U for plot 2
U = Vectorize(function(x){return(sqrt(x/b)/2)})

#Target of 650
#U = Vectorize(function(x){if(x>=650) return(1); return(0)})

T = 2

V = matrix(0,nrow = T+1,ncol=length(W)) #V[t+1,i] gives value of having W[i] dollars with t periods remaining
Alpha = matrix(0,nrow = T+1,ncol=length(W)) #Alpha[t+1,i] gives optimal portfolio wit W[i] dollars and t periods remaining
V[1,] = U(W)
for(t in 1:T){
	for(i in 1:length(W)){
		EU = expectedUtility(W[i],s,r,p,approximate(W,V[t,]))(c(0:N)/N)  #Taking this approach because the optimize function works poorly for locally flat functions
		V[t+1,i] = max(EU)
		Alpha[t+1,i] = which(EU==max(EU))[1]/N
		#soln = optimize(expectedUtility(W[i],s,r,p,approximate(W,V[t,])),interval=c(0,1),maximum=TRUE)
		#Alpha[t+1,i] = soln$maximum  #What is optimal alpha, given t remaining periods and wealth level W[i]?
		#V[t+1,i] = soln$objective #What is expected value, given t remaining periods and wealth level W[i]?
	}
}
#filled.contour(V,x = c(0:T),y=W,xlab='Time Remaining',ylab='Wealth')


#Two period model -- compare alpha in first period to average alpha in second period
AlphaSecondPdAvg = rep(0,length(W))  #AlphaSecondPdAvg[i] gives average alpha in second period, when investor has W[i] with two periods to go and behaves optimally
alphafn = approximate(W,Alpha[2,])  #alphafn(w) says what alpha to choose given wealth of w and one period remaining
for(i in 1:length(W)){
	AlphaSecondPdAvg[i] = sum(alphafn((Alpha[3,i]*r+(1-Alpha[3,i])*s)*W[i])*p)  #Alpha[3,i] is optimal alpha, given wealth W[i] with two periods remaining
}

plot(NULL,xlab='Initial Wealth',xlim=c(W[1],W[length(W)]),ylab='Fraction Invested in Stocks',ylim=c(0,1))
lines(W,predict(loess(Alpha[3,]~W,span=0.1)),col='black')
lines(W,predict(loess(AlphaSecondPdAvg~W,span=0.1)),col='red')
legend(x=W[N*1.3],y=0.9,legend = c('First Period','Second Period'),col=c('black','red'),lty=c(1,1))
#lines(W,Alpha[3,],col='black')
#lines(W,AlphaSecondPdAvg,col='red')

#s = 1.1
#r = c(0.8,1.2,1.65)
#p = rep(1/3,3)


#points(W,Alpha[3,])
#points(W,AlphaSecondPdAvg,pch='x')

#plot(NULL,xlab='Initial Wealth',xlim=c(W[1],W[length(W)]),ylab='Fraction Invested in Stocks',ylim=c(0,1))
#lines(W,predict(loess(Alpha[3,]~W,span=0.1)),col='black')
#lines(W,predict(loess(AlphaSecondPdAvg~W,span=0.1)),col='red')
#legend(x=W[N*0.7],y=0.9,legend = c('First Period','Second Period'),col=c('black','red'),lty=c(1,1))


	
	



