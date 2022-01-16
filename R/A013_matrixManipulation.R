#' matrixManipulation
#'
#' @description
#' Functions to find the neighbouring values of matrix cells. Used by Cyclic Cellular Automata.
#' @param input a matrix.
#' @param xshift,yshift amount to shift the matrix
#' @param type "N","VN" or "M". 
#' @param range numeric. The size of the neighbourhood
#' @return shiftMatrix returns a matrix and neighbourhood returns a array.
#' @name matrixManipulation

#' @rdname matrixManipulation
#' @export
shiftMatrix=function(input,xshift=0,yshift=0){
	answer=input
	if(xshift<0){answer=cbind(input[,(abs(xshift)+1):ncol(input)],input[,1:abs(xshift)])}
	if(xshift>0){answer=cbind(input[,(ncol(input)-xshift+1):ncol(input)],input[,1:(ncol(input)-xshift)])}
	if(yshift<0){answer=rbind(answer[(abs(yshift)+1):nrow(input),],answer[1:abs(yshift),])}
	if(yshift>0){answer=rbind(answer[(nrow(input)-yshift+1):nrow(input),],answer[1:(nrow(input)-yshift),])}
	return(answer)
}
#' @rdname matrixManipulation
#' @export
neighbourhood=function(input,type="VN",range=1){
	nhbd=list()
	if(type%in%c("N","VN")){
		for(i in -range:range){
			for(j in (-range+abs(i)):(range-abs(i))){
				if(i!=0 | j!=0){nhbd[[length(nhbd)+1]]=shiftMatrix(input,i,j)}
			}
		}
	}
	if(type=="M"){
		for(i in -range:range){
			for(j in -range:range){
				if(i!=0 | j!=0){nhbd[[length(nhbd)+1]]=shiftMatrix(input,i,j)}
			}
		}
	}
	return(sapply(nhbd, identity, simplify="array"))
}
