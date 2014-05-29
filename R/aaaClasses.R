#' @title CHARMESPEC
#' @export
setClass("CHARMESPEC", 
         slots = list(models = "list",
                            trans  = "matrix"))

#'  @title CHARME Object
#'  @export
#'  
setClass("CHARME", 
         representation=c(regime = "numeric",
                          spec   = "list",
                          mat    = "matrix"), contains="numeric")