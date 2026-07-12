V = 85000 #total number of visas
A = 96000 #number of A applicants
B = 94000 #number of B applicants
AV = 20000 #number of visas reserved for A applicants


#One lottery, give reserved visa if possible
four = Vectorize(function(AV){return(max(AV,V*A/(A+B)))})

#Process general first
three = Vectorize(function(AV){min(A,(V-AV)*A/(A+B) + AV)}) 

#Two lotteries, process A applicants first
two = Vectorize(function(AV){return(min(A,AV+(A-AV)*(V-AV)/(A+B-AV)))})

#Each counts number of A applicants to receive a visa
plot(two,xlim=c(0,V),las=1,xlab='Set Aside Visas',ylab='',ylim=c(40000,85000),main='Successful A Applicants')
plot(three,xlim=c(0,V),add=TRUE,lty='dashed')
plot(four,xlim=c(0,V),add=TRUE,lty='dotted')

legend(x=5000,y=80000,legend=c('System 2','System 3','System 4'),lty=c('solid','dashed','dotted'))