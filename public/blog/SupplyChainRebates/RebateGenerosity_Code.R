 library(fields)
 n = 300
 Generosity = matrix(0,n,n)
 f = function(M,C){return((1 - C)/(1-M*C))}
for(i in 1:n) for(j in 1:n) Generosity[i,j] = f(i/(n+1),j/(n+1))
cex = 2
image.plot(t(Generosity),x = c(0:(n-1))/(n-1), y = c(0:(n-1))/(n-1),breaks = c(0:32)/32,col=heat.colors(32),cex.lab=1.2,legend.shrink=1,xlab='Competition Level',las=1,ylab='Margin')
title(main='Rebate Generosity',cex.main=2,line=1)
