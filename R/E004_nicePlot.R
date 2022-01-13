#' nicePlot
#'
#' A wrapper for easyPlot with defaults set up to make png files sized for instagram
#' @param art a component, list of components....
#' @param finalWidth,finalHeight sizes given in inches.
#' @param name,folder,bg,format,border,useGrid,res,innerMargin,borderColours,borderSize,... all passed to easyPlot
#' @bgCode a shortcut for border colours. small number. see instagramColours
#' @return No return value, easy plot is called.
#' @export

nicePlot=function(art,...,finalWidth=3.6,finalHeight=3.6,
name="artwork",folder=getwd(),bg="WhiteSmoke",format="png",
auto=FALSE,bgCode=NULL,border=c(0.07,0.03,0),useGrid=FALSE,
res=300,innerMargin=0.04,borderColours=c("white","black"),borderSize=60){
	fileName=name
	if(auto){
		fileName=newFileName(folder=folder,name=fileName,format=format)
	}
	if(!is.null(bgCode)){
		if(bgCode==0){bg=sample(instagramColours(),1)}else{bg=instagramColours()[bgCode]}
	}
	deviceSettings=list(res=150,antialias="cleartype")
	if(class(art)%in%c("component","list","layer","section")){
		deviceSettings$res=300
	}else{
		deviceSettings=art@deviceSettings
	}
	easyPlot(art,bg=c(bg,borderColours),name=fileName,folder=folder,useGrid=useGrid,innerMargin=innerMargin,#rect_lwd=ifelse(useGrid,4,3),
	format=format,
	width=finalWidth-borderSize/deviceSettings$res,
	height=finalHeight-borderSize/deviceSettings$res,
	border=border,
	borderLine=c(NA,NA,NA),deviceSettings=deviceSettings,...)
}