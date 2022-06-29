#' matrixToColours
#'
#' Takes a matrix converts into rgb colours. e.g. 1=red, 2=blue, etc or ranges for each colour
#' @param image a matrix
#' @param col a list of colours or vector that can be coerced into a list using as.list. Each colour should be an rgb vector or character. Lists can contain a mixture of both formats.
#' @param breaks optional. For ranges that correspond to colours
#' @return Returns an RGB array
#' @export
#' @examples
#' matrixToColours(matrix(1:9,3))

matrixToColours=function(image,col=c("white","black","red","blue","orange","yellow","green","purple","brown","gray"),breaks=NULL){
 colR=c()
 colG=c()
 colB=c()
 col=as.list(col)
 for(i in 1:length(col)){
	 if(is.character(col[[i]])){col[[i]]=col2rgb(col[[i]])}
	 colR[i]=col[[i]][1]
	 colG[i]=col[[i]][2]
	 colB[i]=col[[i]][3]
 }
 answer=sapply(list(image,image,image), identity, simplify="array")
 if(!is.null(breaks)){
	answer[,,1]=1
	for(i in seq_along(breaks)){
		answer[,,1]=answer[,,1]+(answer[,,2]>breaks[i])
	}
	answer[,,2]=answer[,,1]
	answer[,,3]=answer[,,1]
 }
 for(i in 1:nrow(image)){
	 answer[i,,1]=colR[answer[i,,1]]
	 answer[i,,2]=colG[answer[i,,2]]
	 answer[i,,3]=colB[answer[i,,3]]
 }
 #answer[is.na(answer[,,])]=255
 return(list(image=answer/255))
}