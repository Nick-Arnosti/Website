cc = .3

fbar = function(x){return(1-punif(x))}

#Need to change upper bound for other distributions
qNews = uniroot(function(x){return(fbar(x)-cc)},interval=c(0,1))$root

qICO = uniroot(function(q){return((fbar(q)-cc)*integrate(fbar,lower=0,upper=q)$value-fbar(q)*cc*q)},interval=c(exp(-6),qNews))$root
alphaICO = cc*qICO/integrate(fbar,lower=0,upper=qICO)$value

avgProfitICO = integrate(Vectorize(function(x){return(fbar(x/(1-alphaICO)))}),lower=0,upper=(1-alphaICO)*qICO)$value

profitFunctionICO = Vectorize(function(d){
	return((1-alphaICO)*min(d,qICO))
})
investorProfitFunctionICO = Vectorize(function(d){
	return(alphaICO*min(d,qICO)-cc*qICO)
})


alphaSTO = function(q){
	return(cc*q/integrate(fbar,lower=cc*q,upper=q)$value)
}

qSTO = uniroot(Vectorize(function(q){return(fbar(q)*(1-alphaSTO(q))*(1-cc)-(1-alphaSTO(q))*cc*(fbar(cc*q)-fbar(q))-cc*(1-fbar(cc*q)))}),interval=c(exp(-6),qNews))$root

profitFunctionSTO = Vectorize(function(d){
	return(min(d,qSTO)-alphaSTO(qSTO)*max(min(d,qSTO)-cc*qSTO,0))
})
investorProfitFunctionSTO = Vectorize(function(d){
	return(min(d,qSTO)-cc*qSTO-profitFunctionSTO(d))
})