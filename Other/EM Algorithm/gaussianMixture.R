require(MASS)
require(car)
nn = 100

x1 = mvrnorm(nn, c(0,3), diag(2))
x2 = mvrnorm(nn, c(-2,-1), cbind(c(2,1),c(1,2)))
x = rbind(x1,x2)


x1 = arima.sim(list(ar = 0.6),n = nn+1, mean = 0.2)
x2 = arima.sim(list(ar = -0.6),n = nn+1, mean = -1)
y1 = x1[-1]; y2 = x2[-1]
x1 = x1[-(nn+1)]; x2 = x2[-(nn+1)]

x = c(x1,x2)
y = c(y1,y2)
x = cbind(x,y)

cols = c(rep(1,nn), rep(2,nn))

plot(x, col = cols, pch  = 19)

means = matrix(ncol = 2, nrow = 2)
means[1,] =  quantile(x[,1],c(0.25,0.5))
means[2,] =  quantile(x[,2],c(0.25,0.5))


covs = list(cov(x), cov(x))

denw = c(0.5,0.5)
weights = rep(0.5,2*nn) 
weights = cbind(weights,weights)

### compute likehood
getLE = function(x,mean,cov)
{
  d = ncol(cov)
  cont = 1/sqrt((2*pi)^d*det(cov))
  
  tmx = sweep(x,2,mean,"-")
  
  mah = mahalanobis(x,mean,cov)
  eterm = -0.5*(mah)
  
  cont*exp(eterm)
}

colWMean = function(x,w) sapply(1:ncol(x), function(i) weighted.mean(x[,i],w))

calcCov = function(x,center,w)
{
  tmx = sweep(x,2,center,"-")*w
  
  t(tmx)%*%tmx/sum(w)
}


addEllipses = function(center, cov, radius = seq(0.5,2,0.5), col)
{
  for(i in 1:length(radius))
    ellipse(center,cov, radius = radius[i], col = col)
}


sapply(1:2, function(i) addEllipses(means[,i],covs[[i]],col = i+2))


for(i in 1:20)
{
  ll1 = getLE(x,means[,1], covs[[1]]) * denw[1]
  ll2 = getLE(x,means[,2], covs[[2]]) * denw[2]
  
  weights[,1] = ll1/(ll1+ll2) 
  weights[,2] = ll2/(ll1+ll2)
  
  denw = colSums(weights)/sum(weights)
  
  plot(x, col = cols, pch  = 19)
  
  means[,1] = colWMean(x,weights[,1])
  means[,2] = colWMean(x,weights[,2])
  
  covs[[1]] = calcCov(x,means[,1], weights[,1])
  covs[[2]] = calcCov(x,means[,2], weights[,2])
  
  sapply(1:2, function(i) addEllipses(means[,i],covs[[i]],col = i+2))
  
}



