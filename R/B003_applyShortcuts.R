#' applyShortcuts
#'
#' Takes a list with names that use shortcuts and replaces them with names recogised by R's graphics systems.
#' @param inputList a parameter list with names
#' @param useGrid determines whether grid is used or not
#' @export
#' @examples
#' print(1+1)
applyShortcuts=function(inputList,useGrid=FALSE){
	if(useGrid){
		answer=grepRename(inputList,"\\.colour$","col")
		answer=grepRename(answer,"\\.color$","col")
		answer=grepRename(answer,"\\.col$","col")
		answer=grepRename(answer,"\\.border$","col")
		#answer=grepRename(answer,"\\.symbol$","pch")
		answer=grepRename(answer,"\\.fill$","fill")
		#answer=grepRename(answer,"\\.font$","fontface")
		#answer=grepRename(answer,"\\.font$","family")
		#answer=grepRename(answer,"\\.fontSize$","ps")
		#answer=grepRename(answer,"\\.fontStyle$","font")
	}else{
		answer=grepRename(inputList,"\\.colour$","col")
		answer=grepRename(answer,"\\.color$","col")
		answer=grepRename(answer,"\\.col$","col")
		answer=grepRename(answer,"\\.symbol$","pch")
		answer=grepRename(answer,"\\.border$","border")
		answer=grepRename(answer,"\\.fill$","col")
		#answer=grepRename(answer,"\\.font$","fontface")
		answer=grepRename(answer,"\\.font$","family")

		answer=grepRename(answer,"\\.fontSize$","ps")
		answer=grepRename(answer,"\\.fontStyle$","font")
	}
	return(answer)
}