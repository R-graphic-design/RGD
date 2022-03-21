#' defaultArtSettings
#'
#' Returns a list of default settings and parameters for easyPlot.
#' @return Returns a list including artClasses, artdata, and artstyle.
#' @export
#' @examples
#' print(1+1)
defaultArtSettings=function(){return(list(artstyle(cex=1),
					artClasses(artBorderBackground="rect",artBorderForeground="polygon",#artBorderForeground="rect",
					xAxisLabels="text",xAxisLine="lines",xAxisTicks="segments",
					yAxisLabels="text",yAxisLine="lines",yAxisTicks="segments",
					graphLine="lines",graphArrow="arrows",graphVertex="polygon",graphLabel="text",graphLineLabel="text",
					celticKnotFill="polygon",celticKnotBorder="lines"),
					artdata(rect_lwd=2,points_col="black",points_lwd=1,
					artBorderForeground_.fill=NA,artBorderForeground_.border="black",artBorderBackground_.border=NA,artBorderBackground_.fill=NA,
					graphVertex_.col="white",
					celticKnotFill_.col="white",celticKnotBorder_lwd=1.5,
					artBorderForeground_lend=2)))}