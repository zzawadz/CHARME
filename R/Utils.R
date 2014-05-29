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
  
  if(prob < 0) stop("Prob must be non negative")
  if(prob > 1) stop("Prob must be less than 1")
  if(dim < 1) stop("dim must be greater than 0")
  
  sprob = prob/(dim-1)
  mat = matrix(sprob, ncol = dim, nrow = dim)  
  diag(mat) = 1 - rowSums(mat) + diag(mat)
  mat
}


############ Default models spec ###########
defaultModelsList = function()
{
  spec1 = garchSpec(model = list(omega=0.10, mu= 0, ar = 0.7, alpha = 0.1, beta = 0.75), cond.dist = "std")
  spec2 = garchSpec(model = list(omega=0.10, mu= 0, ar = 0.1, alpha = 0.1, beta = 0.75), cond.dist = "std")
  
  createModelsList(spec1, spec2)
}

