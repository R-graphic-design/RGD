#' An S4 class to represent a set of integers
#'
#' @description
#' An S4 class to describe a set of integers using conditions. Here are some examples;
#'
#' * Any integer from 5 to 10 or from 50 to 100
#'
#' * Any integer under 7
#' 
#' * Any integer in the vector c(12,11,17,14)
#'
#' It can also represent the union of such sets. For example any integer that meets any one of the above conditions.
#'
#' The function integerCheck checks whether a given integer belongs to a given integerSet.
#' @slot integers A vector of integers. 
#' @slot from stuff
#' @slot to see from
#' @slot over lala
#' @slot under Like over, but for the < operator
#' @param integers,from,to,over,under parameters for the constructor new("integerSet") go directly into the relevant slots
#' @param inputNum a single to number to be compared against inputSet
#' @param inputSet an integerSet object
#' @details 
#' integerCheck(number,set) will return TRUE if
#'
#' * number belongs to set@integers
#'
#' * number>set@over
#'
#' * number<set@under
#'
#' * sum(number>set@from)>sum(number>set@to)
#'
#' If none of these conditions are met, integerCheck will return FALSE. The only exception to all of this is if set@integers contains -Inf. In this case, integerCheck will always FALSE.
#' @export
#' @examples
#' #An integer set
#' autoPass=new("integerSet")
#' integerCheck(4,autoPass)  
#' autoFail=new("integerSet",moments=-Inf)
#' integerCheck(4,autoFail)
#' @name integerSet
integerSet <- setClass("integerSet",
  slots = c(integers="numeric",from="numeric",to="numeric",over="numeric",under="numeric"))

#' @rdname integerSet
#' @method initialise integerSet
#' @export
setMethod("initialize","integerSet",function(.Object,integers=numeric(),from=numeric(),to=numeric(),over=numeric(),under=numeric()){
	.Object@integers<-integers
	.Object@from<-from
	.Object@to<-to
	.Object@over<-over
	.Object@under<-under
	.Object
})


#' @rdname integerSet
#' @export
integerSet=function(...){new("integerSet",...)}

#' @rdname integerSet
#' @export
frames=function(...){new("integerSet",...)}

#' @rdname integerSet
#' @export
integerCheck=function(inputNum,inputSet){
	if(length(inputSet@integers)>0&&-Inf%in%inputSet@integers){return(FALSE)}
	if(length(inputSet@integers)>0&&inputNum%in%inputSet@integers){return(TRUE)}
	numberOfIntervals=length(inputSet@from)+(length(inputSet@over)>0)+(length(inputSet@under)>0)
	if(numberOfIntervals==0&&length(inputSet@integers)==0){return(TRUE)}
	answer=FALSE
	if(length(inputSet@over)>0&&inputNum>=inputSet@over){answer=TRUE}
	if(length(inputSet@under)>0&&inputNum<=inputSet@under){answer=TRUE}
	if(length(inputSet@from)>0&&sum(inputNum>=inputSet@from)>sum(inputNum>=inputSet@to)){answer=TRUE}
	return(answer)
}

