ab = .2
theta = 0.4

f = Vectorize(function(x){return((x > -.2)&&(x<0.8))})
g = Vectorize(function(x){return((x > 0)&&(x<1))})
cMin = -.2
vMax = 1 #vMin = 0
cMax = 0.5 #more generally, could be theta*vMax

mu = function(v,cc){return(f(cc)*g(v))}



Ud = function(v,cc){return(max(ab*v-cc,0))}
Wd = integrate(Vectorize(function(v){return(integrate(Vectorize(function(cc){return(Ud(v,cc)*mu(v,cc))}),cMin,cMax)$value)}),0,vMax)$value



vStar = uniroot(
	function(vS){
		muH = integrate(Vectorize(function(v){return(integrate(Vectorize(function(cc){return(mu(v,cc))}),cMin,theta*v)$value)}),vS,vMax)$value
		muL = integrate(Vectorize(function(v){return(integrate(Vectorize(function(cc){return(mu(v,cc))}),cMin,0)$value)}),0,vS)$value
		return(ab*(muL+muH)-theta*muH)
	},interval=c(0,vMax))$root


Udv = Vectorize(function(v,cc){if(v < vStar) return(max(-cc,0)); return(max(theta*v-cc,0))})
Wdv = integrate(Vectorize(function(v){return(integrate(Vectorize(function(cc){return(Udv(v,cc)*mu(v,cc))}),cMin,cMax)$value)}),0,vMax)$value





aspect.ratio = .5
plot(NULL,xlim=c(cMin,cMax),ylim=c(0,vMax),xlab='c',ylab='v',las=1,asp=aspect.ratio)
polygon(x = c(cMin,cMin,theta*vMax,theta*vStar,0,0),y=c(0,vMax,vMax,vStar,vStar,0),density=0,angle=0,col='blue')
polygon(x = c(cMin,cMin,ab*vMax,0),y=c(0,vMax,vMax,0),density=0,angle=90,col='red',lty='dashed')














aspect.ratio = .5
plot(NULL,xlim=c(cMin,cMax),ylim=c(0,vMax),xlab='c',ylab='v',las=1,asp=aspect.ratio)
polygon(x = c(cMin,cMin,theta*vMax,theta*vStar),y=c(vStar,vMax,vMax,vStar),density=10,angle=90,col='blue')
polygon(x = c(cMin,cMin,ab*vMax,ab*vStar),y=c(vStar,vMax,vMax,vStar),density=10,angle=0,col='blue')
#polygon(x = c(cMin,cMin,theta*vMax,theta*vStar),y=c(vStar,vMax,vMax,vStar),density=10,angle=180/pi*atan(aspect.ratio/theta),col='blue')
#polygon(x = c(cMin,cMin,theta*vMax,theta*vStar),y=c(vStar,vMax,vMax,vStar),density=10,angle=180/pi*atan(aspect.ratio/theta)+90,col='blue')

polygon(x = c(cMin,cMin,0,0),y=c(0,vStar,vStar,0),density=10,angle=90,col='green')
polygon(x = c(cMin,cMin,0,0),y=c(0,vStar,vStar,0),density=10,angle=0,col='green')
polygon(x = c(0,0,ab*vStar),y=c(0,vStar,vStar),density=10,angle=0,col='red')


segments(x0=0,y0 = 0,x1 = theta*vmax,y1 = vmax,lty='dashed')
#segments(x0=0,y0 = 0,x1 = ab*vmax,y1 = vmax,lty='dashed')