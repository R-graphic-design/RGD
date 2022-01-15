#' imageColourScheme
#'
#' Takes a component and calculates a colour scheme for matrixToColours
#' @param input a component
#' @details 
#' imageColourScheme looks for arguments for colourScheme inside input@p
#' @return Returns the input after setting input@p$matrixToColours_col 
#' @export
#' @examples
#' imageColourScheme(component())@p

imageColourScheme=function(input){
	if(is.null(input@p$matrixToColours_col)){
		colourSchemePar=list()
		for(i in names(as.list(args("colourScheme")))){
			if(i %in% names(input@p)){
				colourSchemePar[i]=input@p[i]
			}
		}
		if(!("numberOfColours"%in%names(colourSchemePar))){
			colourSchemePar$numberOfColours=length(unique(as.vector(input@p$image)))
		}
		input@p$matrixToColours_col=do.call("colourScheme",colourSchemePar)
	}
	if(class(input@p$matrixToColours_col)!="list"){
		input@p$matrixToColours_col=as.list(input@p$matrixToColours_col)
	}
return(input)
}