#' An S4 class to represent a piece of string art
#'
#' @description
#' An S4 class to describe a piece of string art.
#' @slot x,y point co-ordinates
#' @slot names point names
#' @slot groups a named list of name vectors
#' @slot distances a list of adjacencies for each line group
#' @slot lineGroups a list of point groups to join with lines
#' @slot loop logical. If true the ends of a group are considered adjacent to the start of the group.
#' @details
#' @family stringArt
#' @export
#' @examples
#' print(1+1)
#' @name stringArt
setClass("stringArt",slots=c(names="character",x="numeric",y="numeric",groups="list",lineGroups="list",distances="ANY",loop="logical"))

#' @rdname stringArt
#' @method initialise stringArt
#' @export
setMethod("initialize","stringArt",function(.Object,x,y,names,groups,lineGroups,distances,loop){
	.Object@x<-x
	.Object@y<-y
	.Object@names<-names
	.Object@groups<-groups
	.Object@distances<-distances
	.Object@lineGroups<-lineGroups
	.Object@loop<-loop
	.Object
})