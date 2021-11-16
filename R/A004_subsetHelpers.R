#' Subset helper functions
#'
#' @description
#' Functions that return x`[i`] but with alternate behaviour when i is greater than the length of x.
#' @param x a vector to be subsetted.
#' @param i a vector of indexes
#' @details
#' loopSubset will cycle back through the elements of x when i is too big. overflowSubset will simply give the last element of x instead.
#' @return A vector containing elements of x, chosen by the index i.
#' @examples
#' loopSubset(1:10,1:20)
#' overflowSubset(1:10,1:20)
#' @name subsetHelpers
NULL
#> NULL

#' @rdname subsetHelpers
#' @export
loopSubset=function(x,i){
return(x[loopIndex(i,length(x))])
}

#' @rdname subsetHelpers
#' @export
overflowSubset=function(x,i){
return(ifelse(i>=length(x),x[length(x)],x))
}