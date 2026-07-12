n = 100
k = c(rep(1,n),rep(2,n))
m = 200

T = 10000
counts = c(0,0)
for(i in 1:T){
	#l = rexp(length(k))
	l = runif(length(k))
	M = rbind(k,l*k)
	M=M[,order(M[2,])]
	t = min(which(cumsum(M[1,])>=m-1))
	counts[1] = counts[1]+length(which(M[1,1:t]==1))
	counts[2] = counts[2]+length(which(M[1,1:t]==2))
	#tabulate(M[1,1:t])
	#data.frame(table(M[1,1:t]))
}
counts/T




#k is group size
p = 0.2
n_tickets = 2

#Probability of winning for a group of size k, given that an individual wins with prob p
#Approximation based on large markets (in finite markets, outcomes are not independent)
successProb = function(k){
	return(pbinom(ceiling(k/n_tickets)-1,k,p,lower.tail=FALSE)) #Need to win ceiling(k/n_tickets) times, so calc prob of at least this many
}


