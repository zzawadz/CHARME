#' @title Create CHARMESpec object
#' @export
#' 
#' @examples
#' charmeSpec()

charmeSpec = function(models, trans_mat)
{
  if(missing(models)) models = CHARME:::defaultModelsList()
  if(missing(trans_mat)) trans_mat = createTransMatrix(dim = 2, prob = 0.01)
    
    # Check function input:
    checkInput(models,trans_mat)
  
  new("CHARMESPEC", models = models, trans = trans_mat)
}