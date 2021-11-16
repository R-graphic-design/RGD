#' Valid Parameters
#' @description
#' * validParameters calculates valid parameters for any function, or for par and gpar.
#' 
#' * validAdjustParameters returns a list of parameters that can modify co-ordinate parameters
#'
#' * coordinateNamesX returns a list of parameters considered to be x co-ordinates
#'
#' * coordinateNamesY returns a list of parameters considered to be x co-ordinates
#' @param fun a function
#' @param simplify logical
#' @param style logical
#' @param useGrid logical
#' @return a vector of parameter names
#' @examples
#' print(1+1)
#' @name validPars
NULL
#> NULL

#' @rdname validPars
#' @export
validParameters=function(fun="",simplify=TRUE,style=FALSE,useGrid=FALSE){
	answer=c()
	if(style){
		if(useGrid){
			return(c("lwd","col","fill","alpha","lty","lex","lineend","linejoin","linemitre","fontsize","cex","fontfamily","fontface","lineheight","font"))
		}else{
			return(c("adj","ann","ask","bg","bty",
				"cex","cex.axis","cex.lab","cex.main","cex.sub",
				"cin","col","col.axis","col.lab","col.main","col.sub",
				"cra","crt","csi","cxy","din","err","family","fg","fig","fin",
				"font","font.axis","font.lab","font.main","font.sub","lab","las",
				"lend","lheight","ljoin","lmitre","lty","lwd","mai","mar","mex",
				"mfcol","mfrow","mfg","mgp","mkh","new","oma","omd","omi",
				"page","pch","pin","plt","ps","pty","smo","srt","tck","tcl","usr",
				"xaxp","xaxs","xaxt","xlog","xpd","yaxp","yaxs","yaxt","ylbias","ylog",
				validAdjustParameters()))
		}
	}else{
		answer=c(names(as.list(args(fun))),validAdjustParameters())
		if(simplify){
			if(fun%in%c("points","lines","text")){answer=c(answer,"y")}
			if(fun%in%c("text")){answer=c(answer,"labels","type")}
			}
		if("..."%in%answer){
			if(useGrid){
				answer=c(answer,c("col","fill","alpha","lty","lwd","lex","lineend",
					"linejoin","linemitre","fontsize","cex","fontfamily",
					"fontface","lineheight","font"))
			}else{
				answer=c(answer,c("adj","ann","bg","bty",
					"cex","cex.axis","cex.lab","cex.main","cex.sub",
					"cin","col","col.axis","col.lab","col.main","col.sub",
					"cra","crt","csi","cxy","din","err","family","fg",
					"font","font.axis","font.lab","font.main","font.sub","lab","las",
					"lend","ljoin","lmitre","lty","lwd","mgp","mkh",
					"page","pch","smo","srt","tck","tcl",
					"xaxp","xaxs","xaxt","xpd","yaxp","yaxs","yaxt"))
			}
		}
	}
	return(answer)
}

#' @rdname validPars
#' @export
validAdjustParameters=function(){return(c("xnative","ynative","xinches","yinches","xnpc","ynpc",
	"xinchesxright","xinchesxleft","xinchesx0","xinchesx1",
	"yinchesytop","yinchesybottom","yinchesy0","yinchesy1",
	"xvarinches","yvarinches"))}


#' @rdname validPars
#' @export
coordinateNamesX=function(){
answer=c("x","x0","x1","xleft","xright","xnative","xinches","xnpc",
	"xinchesxright","xinchesxleft","xinchesx0","xinchesx1")
return(answer)
}

#' @rdname validPars
#' @export
coordinateNamesY=function(){
answer=c("y","y0","y1","yleft","yright","ynative","yinches","ynpc",
	"yinchesyright","yinchesyleft","yinchesy0","yinchesy1")
return(answer)
}