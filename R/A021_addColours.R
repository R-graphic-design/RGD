#' addColours
#'
#' @description
#' A wrapper for colourScheme that can be added to components/layers etc using code like +action.data("addColours").
#' @param numberOfColours The number of colours returned. Defaults to 10.
#' @param scheme The palette or function to use. Used when colours is NULL. Defaults to "BW" for black and white. See details for more information.
#' @param colours A starting set of colours that will be interpolated.
#' @param bias used by colorRampPalette
#' @param space used by colorRampPalette
#' @param alpha used by colorRampPalette
#' @param interpolate used by colorRampPalette
#' @param reverse Reverses the final colour order. Defaults to FALSE
#' @param discrete Defaults to TRUE
#' @param randomise logical. If TRUE randomises the order of the answer. Defaults to FALSE
#' @param addColoursName the name of the parameter in the returned list.
#' @return Returns a named list containing the vector of colours of the given length.
#' @details 
#' The possible values of scheme are;
#'
#' * "BW" and "BWrev" for greyscale 
#'
#' * "rainbow" and "rainbowrev" which uses rainbow()
#'
#' * "heat" and "heatrev" which uses heat.colors()
#'
#' * "terrain" and "terrainrev" which uses terrain.colors()
#'
#' * "topo" and "toporev" which uses topo.colors()
#'
#' * "cm" and "cmrev" which uses cm.colors()
#'
#' * "BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
#'	"RdYlGn","Spectral","Accent","Dark2","Paired","Pastel1","Pastel2","Set1",
#'	"Set2","Set3","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
#'	"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
#'	"YlOrBr" and "YlOrRd" from the RColorBrewer package
#'
#' * "plasma", "plasmarev", "viridis", "viridisrev", "magma", "magmarev", "inferno" and "infernorev" from the viridis package.
#'
#' *"BottleRocket1","BottleRocket2","Rushmore1","Rushmore",      
#' "Royal1","Royal2","Zissou1","Darjeeling1",   
#' "Darjeeling2","Chevalier1","FantasticFox1","Moonrise1",     
#' "Moonrise2","Moonrise3","Cavalcanti1","GrandBudapest1",
#' "GrandBudapest2","IsleofDogs1" and "IsleofDogs2" from the wesanderson package.
#'
#' The above terms that end in rev overrides the reverse parameter to force reversing the colour scheme.
#' @export
#' @examples
#' addColours()
addColours=function(numberOfColours=10,scheme="BW",colours=NULL,bias=1,space=c("rgb","Lab"),alpha=FALSE,
interpolate=c("linear","spline"),reverse=FALSE,discrete=TRUE,randomise=FALSE,addColoursName=".col"){
	answer=list()
	answer[[addColoursName]]=colourScheme(numberOfColours=numberOfColours,scheme=scheme,colours=colours,
					bias=bias,space=space,alpha=alpha,interpolate=interpolate,reverse=reverse,discrete=discrete,
					randomise=randomise)
	return(answer)
}

  