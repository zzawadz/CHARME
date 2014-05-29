#' @title CHARMESPEC
#' @export
setClass("CHARMESPEC", 
         slots = list(models = "list",
                            trans  = "matrix"))

#'  @title CHARME Object
#'  @export
#'  
setClass("CHARME", 
         representation=c(spec = "CHARMESPEC",
                          regime = "numeric"), contains="numeric")