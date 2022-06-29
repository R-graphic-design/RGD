#' makeMesh
#' @description
#' Functions to produces meshes from a list of vertices
#' runMesh performs calculations. makeMesh makes a component or list of components.
#' @param x,y,z co-ordinates for vertices
#' @param faces list of faces. Each face is numerical vector of indices that specify a sequence of vertices from x,y and z.
#' @examples
#' @family makeMesh
#' makeMesh()
#' @name makeMesh 
NULL
#> NULL

#' @rdname makeMesh
#' @export
runMesh=function(x,y,z,faces){
	answer=list(x=c(),y=c(),z=c())
	for(i in seq_along(faces)){
		answer$x=c(answer$x,x[faces[[i]]])
		answer$y=c(answer$y,y[faces[[i]]])
		answer$z=c(answer$z,z[faces[[i]]])
		if(i<length(faces)){
			answer$x=c(answer$x,NA)
			answer$y=c(answer$y,NA)
			answer$z=c(answer$z,NA)
		}
	}
	return(answer)
}

#' @rdname makeMesh
#' @export
makeMesh=function(type="polygon",...){
	return(component(type=type,...)+action.data(fun="runMesh",...))
}