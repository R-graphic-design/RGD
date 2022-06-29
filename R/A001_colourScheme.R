#' colourScheme
#'
#' @description
#' Creates a single function that can create colour schemes using many different packages with the option of interpolation.
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
#' @return Returns a vector of colours of the given length.
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
#' colourScheme()
#' colourScheme(25,"rainbow")


colourScheme=function(numberOfColours=10,scheme="BW",colours=NULL,bias=1,space=c("rgb","Lab"),alpha=FALSE,
interpolate=c("linear","spline"),reverse=FALSE,discrete=TRUE,randomise=FALSE){
	tempColours=colours
	if(is.null(colours)){
		tempColours=c()
		if(scheme %in% c("BW","BWrev")){
			for(i in 1:numberOfColours){
				tempRatio=i/(numberOfColours+1)
				tempColours=c(rgb(tempRatio,tempRatio,tempRatio),tempColours)
			}
		}
		if(scheme %in% c("rainbow","rainbowrev")){
			tempColours=rainbow(numberOfColours)
		}
		if(scheme %in% c("heat","heatrev")){
			tempColours=heat.colors(numberOfColours)
		}
		if(scheme %in% c("terrain","terrainrev")){
			tempColours=terrain.colors(numberOfColours)
		}
		if(scheme %in% c("topo","toporev")){
			tempColours=topo.colors(numberOfColours)
		}
		if(scheme %in% c("cm","cmrev")){
			tempColours=cm.colors(numberOfColours)
		}
	}
	if(length(tempColours)==1){
		tempColours=rep(tempColours,numberOfColours)
	}
	if(scheme=="interpolate"){
		tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)
	}
	if(scheme %in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
	"RdYlGn","Spectral","Accent","Dark2","Paired","Pastel1","Pastel2","Set1",
	"Set2","Set3","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
	"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
	"YlOrBr","YlOrRd")){
		require("RColorBrewer")
		if(numberOfColours>RColorBrewerMaxColours(scheme)|!discrete){
			tempColours=brewer.pal(n = RColorBrewerMaxColours(scheme), name = scheme)
			tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)		
		}else{
			tempColours=brewer.pal(n = numberOfColours, name = scheme)
		}
	}
	if(scheme %in% c("plasma","plasmarev")){
		require("viridis")
		tempColours=plasma(numberOfColours)
	}
	if(scheme %in% c("viridis","viridisrev")){
		require("viridis")
		tempColours=viridis(numberOfColours)
	}
	if(scheme %in% c("magma","magmarev")){
		require("viridis")
		tempColours=magma(numberOfColours)
	}
	if(scheme %in% c("inferno","infernorev")){
		require("viridis")
		tempColours=inferno(numberOfColours)
	}
	if(scheme %in% c("BottleRocket1","BottleRocket2","Rushmore1","Rushmore",      
 "Royal1","Royal2","Zissou1","Darjeeling1",   
 "Darjeeling2","Chevalier1","FantasticFox1","Moonrise1",     
"Moonrise2","Moonrise3","Cavalcanti1","GrandBudapest1",
"GrandBudapest2","IsleofDogs1","IsleofDogs2")){
		require("wesanderson")
		if(numberOfColours>wesandersonMaxColours(scheme)|!discrete){
			tempColours=wes_palette(n = wesandersonMaxColours(scheme), name = scheme, type="continuous")
			tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)		
			}else{
			tempColours=wes_palette(n = numberOfColours, name = scheme, type="discrete")
		}
	}
	if(scheme %in% c("BWrev","rainbowrev","heatrev","toporev","cmrev","terrainrev","plasmarev","infernorev","viridisrev","magmarev")|reverse){
		tempColours=rev(tempColours)
	}
	if(randomise){tempColours=sample(tempColours,length(tempColours))}
	return(tempColours)
}


#' RColorBrewerMaxColours
#'
#' Returns the maximum number of colours for a given scheme in RColorBrewer
#' @param scheme The scheme in question
#' @return The maximum number of colours for a given scheme in RColorBrewer
#' @examples
#' RColorBrewerMaxColours=function(scheme="RdPu")

RColorBrewerMaxColours=function(scheme="RdPu"){
#Sequential Palettes
if(scheme%in% c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
	"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
	"YlOrBr","YlOrRd")){return(9)}
#Diverging Palettes
if(scheme%in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
	"RdYlGn","Spectral")){return(11)}
#Qualitative Palettes
if(scheme%in% c("Accent","Dark2","Pastel2","Set2")){return(8)}
if(scheme%in% c("Pastel1","Set1")){return(9)}
if(scheme%in% c("Paired","Set3")){return(12)}
}

#' wesandersonMaxColours
#'
#' Returns the maximum number of colours for a given scheme in the wes anderson package
#' @param scheme The scheme in question
#' @return The maximum number of colours for a given scheme in wes anderson
#' @examples
#' wesandersonMaxColours=function(scheme="GrandBudapest1")


wesandersonMaxColours=function(scheme="GrandBudapest1"){
if(scheme%in%c("BottleRocket1")){return(7)}
if(scheme%in%c("IsleofDogs1")){return(6)}
if(scheme%in%c("BottleRocket2","Rushmore1","Rushmore2","Royal2","Zissou1","Darjeeling1",
	"Darjeeling2","FantasticFox1","Moonrise3","Cavalcanti1","IsleofDogs2")){return(5)}
if(scheme%in%c("Royal1","Chevalier1","Moonrise1","Moonrise2","GrandBudapest1","GrandBudapest2")){return(4)}
}

#The Color Universal Design from the University of Tokyo proposes the following palettes:
#palette using grey
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#palette using black
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  