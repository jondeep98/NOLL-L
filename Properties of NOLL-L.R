
# CDF
pNOLL=function(x,a,b){
u=1+exp(-x);
return(u^(-a)/(u^(-a)+exp(-b*x)*u^(-b))) 
}


#PDF
dNOLL=function(x,a,b){
u=1+exp(-x);
return((exp(-b*x)*u^(-a-b)*(a+(b-a)*u^(-1)))/(u^(-a)+exp(-b*x)*u^(-b))^2)
}


#quantile
qNOLL=function(p, a, b){
  u1 = function(x, a, b) (1+exp(-x))^(-a)*(1-p)-p*exp(-b*x)*(1+exp(-x))^(-b)
  return(uniroot(u1,  c(-150, 150),  tol = 0.0000000001,  a=a,  b=b)$root) }

#pseudo-random number generator 
rNOLL =function(n, a, b) {
  x=numeric(n)
  for (i in 1:n) x[i]=qNOLL(runif(1, 0, 1), a, b)
  return(sort(x)) }

x1=rNOLL(1000,6,0.5)


hist(x1)









