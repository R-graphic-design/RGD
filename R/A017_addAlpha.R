#' addAlpha
#'
#' Takes a colour and adds transparency
#' @param col a colour
#' @param alpha a transparency from 0 to 1
#' @return a colour
#' @export
#' @examples
#' pruneStart(1:10,4)

addAlpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
}