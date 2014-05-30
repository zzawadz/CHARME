#' @title Vert naive CHARME estimator based
#' @export
charmeSimpleEst = function(x, nmodels = 2, niter = 10, trace = TRUE)
{
  
  n = length(x)
  xx = x[-n]
  yy = x[-1]
  
  data = cbind(xx,yy)
  ndata = nrow(data)
  
  ## Variables for estimation:
  mod_size = rep(1/nmodels, nmodels)
  weights  = matrix(1/nmodels, ncol = nmodels, nrow = ndata)
  means    = initMeans(data, nmodels)
  cov_mats = initCov(data, nmodels)
  
  ## Other utils vars
  nmod = 1:nmodels
  
  ## estimation:
  for(i in 1:niter)
  {
    weights = sapply(nmod, function(i) mod_size[i] * getLE(data,means[,i],cov_mats[[i]]))
    weights  = weights/rowSums(weights)
    mod_size = colSums(weights)/sum(weights) 
    
    means    = sapply(nmod, function(i) colWMean(data,weights[,i]))
    cov_mats = sapply(nmod, function(i) calcCov(data,means[,i],weights[,i]), simplify = FALSE)
  }
  

  
  regime = getModelNum(weights); regime = c(regime[1],regime)
  models = sapply(nmod, function(i) fitGarch(x[regime == i]))
  trans_mat = estTrans(regime)
  spec = charmeSpec(models, trans_mat)
  
  
  obj = new("CHARME",x, regime = regime, spec = spec)
  return(obj)
}

########### Utils func only for charmeSimpleEst  ############
estTrans = function(regime)
{
  reg = paste(regime, collapse="")
  uqr = unique(regime)
  
  regimes = sapply(paste0(uqr,"+"), gregexpr,reg, simplify = FALSE)
  len = sapply(regimes, function(x) attr(x[[1]],"match.length"), simplify = FALSE)
  prob = 1/sapply(len, mean)
  
  createTransMatrix(2,prob)
}



###
fitGarch = function(x)
{
  fit = garchFit(data = x, formula = ~ arma(1,0) + garch(1,1), trace = FALSE)
  fit = fit@fit$par
  names(fit)[c(2,4,5)] = c("ar","alpha","beta")
  garchSpec(fit)
}

# Return matrix with init means for models
initMeans = function(data, nmodels, rand = TRUE)
{
  q = cumsum(rep(1/(nmodels+1),nmodels))
  means = apply(data,2,quantile,q)
  
  if(rand) means = apply(means,2,sample)
  return(t(means))
}

initCov = function(data, nmodels)
{
  cov = cov(data)
  list = list()
  for(i in 1:nmodels) list[[i]] = cov
  list
}

### Compute likehood
getLE = function(x,mean,cov)
{
  d = ncol(cov)
  cont = 1/sqrt((2*pi)^d*det(cov))
  
  tmx = sweep(x,2,mean,"-")
  
  mah = mahalanobis(x,mean,cov)
  eterm = -0.5*(mah)
  
  cont*exp(eterm)
}

### Weighted mean by column
colWMean = function(x,w) sapply(1:ncol(x), function(i) weighted.mean(x[,i],w))

### weighted covariance function
calcCov = function(x,center,w)
{
  tmx = sweep(x,2,center,"-")*w
  
  t(tmx)%*%tmx/sum(w)
}

### return model number
getModelNum = function(y)
{
  apply(y,1, function(x)
  {
    ct = which(x ==max(x))
    if(length(ct)>1) return(1)
    ct
  }) 
}