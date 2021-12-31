.gdinr_global_env <- new.env(parent=emptyenv())

setHiddenVar<-function(name,value){
  assign(name, value, envir=.gdinr_global_env)
  return(invisible(NULL))
}

getHiddenVar<-function(name){
  if(exists(name,envir=.gdinr_global_env)){
    return(get(name,.gdinr_global_env))
  }else{
	return(NULL)
  }
}
 