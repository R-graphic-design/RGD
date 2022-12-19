#' antiAlias
#'
#' @description
#' A simple anti alias function
#' @param image an array with three colour channels
#' @return Returns an array that is a quarter the size of the original
#' @details 
#' Takes an array and splits each channel into non-overlapping 2x2 cells and returns a smaller array with the average of the old 2x2 cells in the new cells. 
#' @export
#' @examples
#' print(1+1)

antiAlias22=function(image){
	dimAnswer=dim(image)
	dimAnswer[1:2]=dimAnswer[1:2]/2
	answer=array(0,dimAnswer)
	for(i in 1:dimAnswer[1]){
		for(j in 1:dimAnswer[2]){
			answer[i,j,1]=mean(image[(2*i)-0:1,(2*j)-0:1,1])
			answer[i,j,2]=mean(image[(2*i)-0:1,(2*j)-0:1,2])
			answer[i,j,3]=mean(image[(2*i)-0:1,(2*j)-0:1,3])
		}
	}
	return(list(image=answer))
}
