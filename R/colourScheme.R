#' colourScheme
#'
#' Creates a single function that can create colour schemes using many different packages with the option of interpolation.
#' @param numberOfColours The number of colours returned. Defaults to 10.
#' @param style The palette or function to use. Used when colours is NULL. Defaults to "BW" for black and white. 
#' @param colours A starting set of colours that will be interpolated.
#' @param bias used by colorRampPalette
#' @param space used by colorRampPalette
#' @param alpha used by colorRampPalette
#' @param interpolate used by colorRampPalette
#' @param reverse Reverses the final colour order. Defaults to FALSE
#' @param discrete Defaults to TRUE
#' @return Returns a vector of colours of the given length.
#' @export
#' @examples
#' colourScheme()
#' colourScheme(25,"rainbow")


colourScheme=function(numberOfColours=10,style="BW",colours=NULL,bias=1,space=c("rgb","Lab"),alpha=FALSE,
interpolate=c("linear","spline"),reverse=FALSE,discrete=TRUE){
	tempColours=colours
	if(is.null(colours)){
		tempColours=c()
		if(style %in% c("BW","BWrev")){
			for(i in 1:numberOfColours){
				tempRatio=i/(numberOfColours+1)
				tempColours=c(rgb(tempRatio,tempRatio,tempRatio),tempColours)
			}
		}
		if(style %in% c("rainbow","rainbowrev")){
			tempColours=rainbow(numberOfColours)
		}
		if(style %in% c("heat","heatrev")){
			tempColours=heat.colors(numberOfColours)
		}
		if(style %in% c("terrain","terrainrev")){
			tempColours=terrain.colors(numberOfColours)
		}
		if(style %in% c("topo","toporev")){
			tempColours=topo.colors(numberOfColours)
		}
		if(style %in% c("cm","cmrev")){
			tempColours=cm.colors(numberOfColours)
		}
	}
	if(length(tempColours)==1){
		tempColours=rep(tempColours,numberOfColours)
	}
	if(style=="interpolate"){
		tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)
	}
	if(style %in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
	"RdYlGn","Spectral","Accent","Dark2","Paired","Pastel1","Pastel2","Set1",
	"Set2","Set3","Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
	"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
	"YlOrBr","YlOrRd")){
		require("RColorBrewer")
		if(numberOfColours>RColorBrewerMaxColours(style)|!discrete){
			tempColours=brewer.pal(n = RColorBrewerMaxColours(style), name = style)
			tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)		
		}else{
			tempColours=brewer.pal(n = numberOfColours, name = style)
		}
	}
	if(style %in% c("plasma","plasmarev")){
		require("viridis")
		tempColours=plasma(numberOfColours)
	}
	if(style %in% c("viridis","viridisrev")){
		require("viridis")
		tempColours=viridis(numberOfColours)
	}
	if(style %in% c("magma","magmarev")){
		require("viridis")
		tempColours=magma(numberOfColours)
	}
	if(style %in% c("inferno","infernorev")){
		require("viridis")
		tempColours=inferno(numberOfColours)
	}
	if(style %in% c("BottleRocket1","BottleRocket2","Rushmore1","Rushmore",      
 "Royal1","Royal2","Zissou1","Darjeeling1",   
 "Darjeeling2","Chevalier1","FantasticFox1","Moonrise1",     
"Moonrise2","Moonrise3","Cavalcanti1","GrandBudapest1",
"GrandBudapest2","IsleofDogs1","IsleofDogs2")){
		require("wesanderson")
		if(numberOfColours>wesandersonMaxColours(style)|!discrete){
			tempColours=wes_palette(n = wesandersonMaxColours(style), name = style, type="continuous")
			tempColours=colorRampPalette(colors=c(tempColours),bias=bias,alpha=alpha,space=space,interpolate=interpolate)(numberOfColours)		
			}else{
			tempColours=wes_palette(n = numberOfColours, name = style, type="discrete")
		}
	}
	if(style %in% c("BWrev","rainbowrev","heatrev","toporev","cmrev","terrainrev","plasmarev","infernorev","viridisrev","magmarev")|reverse){
		tempColours=rev(tempColours)
	}
	return(tempColours)
}


#' RColorBrewerMaxColours
#'
#' Returns the maximum number of colours for a given style in RColorBrewer
#' @param style The style in question
#' @return The maximum number of colours for a given style in RColorBrewer
#' @examples
#' RColorBrewerMaxColours=function(style="RdPu")

RColorBrewerMaxColours=function(style="RdPu"){
#Sequential Palettes
if(style%in% c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges",
	"OrRd","PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu",
	"YlOrBr","YlOrRd")){return(9)}
#Diverging Palettes
if(style%in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu",
	"RdYlGn","Spectral")){return(11)}
#Qualitative Palettes
if(style%in% c("Accent","Dark2","Pastel2","Set2")){return(8)}
if(style%in% c("Pastel1","Set1")){return(9)}
if(style%in% c("Paired","Set3")){return(12)}
}

#' wesandersonMaxColours
#'
#' Returns the maximum number of colours for a given style in the wes anderson package
#' @param style The style in question
#' @return The maximum number of colours for a given style in wes anderson
#' @examples
#' wesandersonMaxColours=function(style="GrandBudapest1")


wesandersonMaxColours=function(style="GrandBudapest1"){
if(style%in%c("BottleRocket1")){return(7)}
if(style%in%c("IsleofDogs1")){return(6)}
if(style%in%c("BottleRocket2","Rushmore1","Rushmore2","Royal2","Zissou1","Darjeeling1",
	"Darjeeling2","FantasticFox1","Moonrise3","Cavalcanti1","IsleofDogs2")){return(5)}
if(style%in%c("Royal1","Chevalier1","Moonrise1","Moonrise2","GrandBudapest1","GrandBudapest2")){return(4)}
}

#The Color Universal Design from the University of Tokyo proposes the following palettes:
#palette using grey
#cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#palette using black
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  