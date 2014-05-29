#' @title Symulowanie obserwacji z procesu CHARME
#' @export
#' 
#' @param spec CHARMESPEC object created by charmeSpec
#' @param n numbers of simulated values
#'

charmeSim = function(spec = charmeSpec(),  n = 500, n_start = 100, start_regime = 1)
{
  models = spec@models
  x = sapply(models,function(sp) as.numeric(garchSim(sp,n = n, n.start = n_start)))
  
  mat = spec@trans
  
  nrem = 1:ncol(mat)
  regime = 1:(n+1)
  regime[1] = start_regime
  data = 1:n
  
  for(i in 2:(n+1))
  {
    regime[i] = sample(nrem,1,prob=mat[regime[i-1],])
    data[i-1] = x[i-1,regime[i]]
  }
  
  obj = new("CHARME",data, regime = regime, spec = spec)
  return(obj)
}