plotCHARME = function(x, chtype = "l", type = "l", alpha = 0.5, rcol = NULL,...) 
{
  plot(as.numeric(x), type = type,...)
  plotRegime(x@regime, colors =  rcol, ylim = extendrange(x)*2, alpha = alpha)
  lines(x,type = type,...)
  
}

setMethod("plot", signature = "CHARME", plotCHARME)




plotRegime = function(x , colors = NULL, alpha = 0.5,ylim = c(-10,10))
{
  if(is.null(colors)) colors = 1:length(unique(x))+1
  x  = factor(x)
  uq = levels(x)
  xx = as.numeric(factor(x))
  uqxx = 1:length(uq)
  xx = paste(xx, collapse="")
  
  
  
  
  regimes = sapply(paste0(uqxx,"+"), gregexpr,xx)
  
  sapply(1:length(regimes), function(i)
  {
    regime = regimes[[i]]
    len = attr(regime,"match.length")
    rect(xleft=regime,xright=regime+len, ybottom=ylim[1], ytop=ylim[2],col = addAlphaToColor(colors[i],alpha), border = NA)
  })
  legend("topleft", uq, fill = colors)
}