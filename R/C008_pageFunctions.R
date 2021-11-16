#' pagefunctionsfunctions
#'
#' @description
#' An function for running all the functions in a page
#' @export
#' @examples
#' print(1+1)
#' @name pagefunctions
runPageFunctions=function(input,classes=list(),getData=FALSE,runTimes=0,frameNumber,plotting=FALSE){
if(getData){input=getRequiredDataPage(input)}
#print("running runPageFunctions")
#print(class(input))
	answer=input
	if(length(answer@sections)>0){
		for(i in 1:length(answer@sections)){
			answer@sections[[i]]=runSectionFunctions(answer@sections[[i]],answer@classes,frameNumber=frameNumber,plotting=plotting)
		}
	}
	return(answer)
}