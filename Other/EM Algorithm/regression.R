# x1 = 1:50
# x2 = 1:50
# 
# y1 = x1*2+5+rnorm(length(x1),sd = 7)
# y2 = x2*4+5+rnorm(length(x2),sd = 5)
# 
# x = c(x1,x2)
# y = c(y1,y2)


##### Functions #####
### calculate likehood 
calcLike = function(x,y,par,sigma)
{
  eps = y - x*par[2] - par[1] 
  dnorm(eps,0,sigma)  
}

getCategory = function(y)
{
  apply(y,1, function(x)
  {
    ct = which(x ==max(x))+1
    if(length(ct)>1) return(1)
    ct
  }) 
}




require(robustbase)
require(animation)
data(starsCYG)
#x = starsCYG$log.Te
#y = starsCYG$log.light

k = 200
x1 = arima.sim(list(ar = 0.6),n = k, mean = 2)
x2 = arima.sim(list(ar = -0.6),n = k, mean = -1)
y1 = x1[-1]; y2 = x2[-1]
x1 = x1[-k]; x2 = x2[-k]

x = c(x1,x2)
y = c(y1,y2)

plot(x,y, pch = 19, cex = 1.5)

# number of models
nmodels = 2
init_sample = 40

#### Variables for models ####
# Parameters of models

share   = rep(1/nmodels,nmodels)
params = matrix(nrow = 2, ncol = nmodels)
sigmas = 1:nmodels



n = length(y)

for(i in 1:nmodels)
{
  smp = sample(1:n,init_sample, replace = TRUE)
  fit = summary(lm(y[smp]~x[smp]))
  params[,i] = fit$coeff[,1]
  sigmas[i] = fit$sigma
  
}

# Add initial lines to a plot:
# params[1,] = params[1,]+c(-5,4)
params[2,2] = -params[2,1]
apply(params,2, function(x) abline(x[1],x[2], lwd = 3))


ani.options('interval' = 0.5)
#saveVideo({
for(i in 1:30)
{
  weights = sapply(1:nmodels, function(i) share[i]*calcLike(x,y,params[,i],sigmas[i]))
  weights = weights/rowSums(weights)
  share = colSums(weights)/sum(weights)
  
  for(i in 1:nmodels)
  {
    fit = summary(lm(y~x, weights = weights[,i]))
    params[,i] = fit$coeff[,1]
    sigmas[i] = fit$sigma
  }
  
  category = getCategory(weights)
  plot(x,y,col = category, pch = 19, cex = 1.5)
  apply(params,2, function(x) abline(x[1],x[2], lwd = 3))
}#}, other.opts = "-b 3000k", outdir = getwd())





