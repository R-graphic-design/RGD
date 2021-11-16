#' grepRename
#'
#' A function to rename the names in a named list using grep patterns.
#' @param inputList a named list
#' @param inputPattern the items to be renamed
#' @param replaceString the replacement
#' @return Returns the original list with any matches renamed.
#' @export
#' @examples
#' testInputList=list(col="red",.colour="blue",points_.colour="green",lines_col="yellow",lines_.colour="pink")
#' testOutputList=grepRename(testInputList,"\\.colour$","col")
#' expectedOutputList=list(col="blue",points_col="green",lines_col="pink")
grepRename=function(inputList,inputPattern,replaceString){
	debugFLAG=FALSE
	
	answer=inputList
	#what happens to duplicates....bad things at the moment..
	if(sum(grepl(pattern=inputPattern,x=names(answer),perl=TRUE)>0)){
		matched=grep(pattern=inputPattern,x=names(answer), value=TRUE,perl=TRUE)
		if(debugFLAG){print(matched)}
		for(i in 1:length(matched)){
			replacement=sub(pattern=inputPattern,replacement=replaceString,x=matched[i])
			if(debugFLAG){cat("### replacement",": ",replacement,"\n")}
			if(replacement %in% names(answer)){
				answer=answer[-which(names(answer)==replacement)]
				if(debugFLAG){cat("remove column",": ","\n")
				print(answer)}
			}
			names(answer)=sub(pattern=paste("^",matched[i],"$",sep=""),replacement=replacement,x=names(answer))
			if(debugFLAG){cat("rename column",": ","\n")
			print(answer)}
		}
	}
	return(answer)
}
