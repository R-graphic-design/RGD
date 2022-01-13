#' newFileName
#'
#' Generates a file name in a specific format, which doesn't conflict with any files already in the folder.
#' @param folder the address of the folder
#' @param name the non-numerical part of the file name
#' @param format the type of file used. (png, txt, etc.) 
#' @return Returns a new file name
#' @export

newFileName=function(folder,name,format){
	fileList=list.files(getwd(),pattern=paste(name,"[0123456789]+.png$",sep="_"))
	print(fileList)
	attempts=0
	while(attempts<1000){
		if(paste(name,"_",attempts+1,".",format,sep="")%in%fileList){
			attempts=attempts+1
		}else{
			return(paste(name,attempts+1,sep="_"))
		}
	}
}