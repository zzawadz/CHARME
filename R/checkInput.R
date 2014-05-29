#' @title Check for charmeSpec
#' 
checkInput = function(list, trans_mat)
{
  checkModelsList(list)
  checkTransMatrix(trans_mat)
  checkMLandTrDim(list, trans_mat)
}


#' @title Check that a matrix is a proper trasition matrix
#' 
checkTransMatrix = function(x)
{
  if(any(x<0))
  {
    stop("Transition matrix must have not negative values!")
  }
  
  if( any(round(rowSums(x), 10) != 1))
  {
    stop("Transition matrix is not valid! Sums of rows must be equal 1.")
  }
  
  if(ncol(x) != nrow(x))
  {
    stop("Transition matrix must be square!")
  }
  
}

#' @title Check validity of the models list
checkModelsList = function(list)
{
  res = sapply(list, function(spec) class(spec) != "fGARCHSPEC")
  if(any(res)) stop("There is a problem with list of models
One or more objects in models list is not a vaild fGARCHSPEC")
  
}

#' @title Check dimension of trans_mat and length of models list 
checkMLandTrDim = function(list, mat)
{
  ll = length(list)
  dd = ncol(mat)
  if(dd != ll) stop("List of models must be equal to
                    transition matrix dimensions")
}

