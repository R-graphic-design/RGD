#' saveSeed
#'
#' @description
#' Functions to save and restore seeds for random number generators
#' @param oldSeed a seed to be restored, or NULL if the seed hadn't been set
#' @details
#' @return saveSeed returns the current seed or NULL if not set. restoreSeed doesn't return anything.
#' @seealso
#' @examples
#' print(1+1)
#' @name randomSeeds
NULL
#> NULL


#' @rdname randomSeeds
#' @export
saveSeed=function(){
	oldseed<-NULL
    if (exists(".Random.seed", .GlobalEnv)){oldseed <- .GlobalEnv$.Random.seed}
	return(oldseed)
}

#' @rdname randomSeeds
#' @export
restoreSeed=function(oldseed){
    if (!is.null(oldseed)) 
        .GlobalEnv$.Random.seed <- oldseed
    else
        rm(".Random.seed", envir = .GlobalEnv)
}