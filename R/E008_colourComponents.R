#' colourComponents
#'
#' Takes a layer and uses the componentColours entry in the data slot to colour the components.
#' @param layer input layer
#' @return Returns the original layer with modified colours for the components
#' @export
#' @examples
#' print(1+1)

colourComponents=function(layer){
	for(i in 1:length(layer@components)){
		layer@components[[i]]@data$.col=layer@data$componentColours[i]
	}
	return(layer)
}