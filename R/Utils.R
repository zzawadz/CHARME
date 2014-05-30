#' @title Create and check validity of models list in CHARME 
#' @export
createModelsList = function(...)
{
  list = list(...)
  checkModelsList(list)
  list
}

#' @title Utility function for creating transition matrix
#' @export
createTransMatrix = function(dim = 2, prob = 0.01)
{
  
  if(any(prob < 0)) stop("Prob must be non negative")
  if(any(prob > 1)) stop("Prob must be less than 1")
  if(dim < 1) stop("dim must be greater than 0")
  
  sprob = prob/(dim-1)
  mat = matrix(sprob, ncol = dim, nrow = dim)  
  diag(mat) = 1 - rowSums(mat) + diag(mat)
  mat
}


############ Default models spec ###########
defaultModelsList = function()
{
  spec1 = garchSpec(model = list(omega=0.10, mu= -2, ar = 0.7, alpha = 0.1, beta = 0.75))
  spec2 = garchSpec(model = list(omega=0.10, mu= 2, ar = -0.5, alpha = 0.1, beta = 0.75))
  
  createModelsList(spec1, spec2)
}

########## addAlphaToColor ##################
addAlphaToColor <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, function(x) 
    rgb(x[1], x[2], x[3], alpha=alpha))  
}


