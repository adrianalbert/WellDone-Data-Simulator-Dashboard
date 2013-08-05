#########################################
# df2JSON.r 
# 
# Convert R data frame to JSON
# 
# Adrian Albert
# Adapted from http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/
# Last modified: July 2013. 
#########################################

df2JSON <- function(dtf, tofile = NULL){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric'){
      quote <- '"';
    }    
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  if (!is.null(tofile)) {
    fileConn<-file(tofile)
    writeLines(res, fileConn)
    close(fileConn)
  }
  
  return(res)
}
