#' imageColourScheme
#'
#' Takes a component and calculates a colour scheme for matrixToColours
#' @param input a component
#' @details 
#' imageColourScheme looks for arguments for colourScheme inside input@data
#' @return Returns the input after setting input@data$matrixToColours_col 
#' @export
#' @examples
#' imageColourScheme(component())@data

imageColourScheme=function(input){
	if(is.null(input@data$matrixToColours_col)){
		colourSchemePar=list()
		for(i in names(as.list(args("colourScheme")))){
			if(i %in% names(input@data)){
				colourSchemePar[i]=input@data[i]
			}
		}
		if(!("numberOfColours"%in%names(colourSchemePar))){
			colourSchemePar$numberOfColours=length(unique(as.vector(input@data$image)))
		}
		input@data$matrixToColours_col=do.call("colourScheme",colourSchemePar)
	}
	if(class(input@data$matrixToColours_col)!="list"){
		input@data$matrixToColours_col=as.list(input@data$matrixToColours_col)
	}
return(input)
}