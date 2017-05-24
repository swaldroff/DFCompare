

dfCompare <- function(dfOld,dfNew,key) {

  dfJoined <- merge(dfOld,dfNew,key)
  dfDeletes <- dfOld[is.na(match(dfOld[,key],dfNew[,key])),]
  dfAdds <- dfNew[is.na(match(dfNew[,key],dfOld[,key])),]


  dfJoined <- sapply(dfJoined,as.character)

  #Function for checking if a field was changed between the dataframes
  isChanged <- function(x,idx,colCount)
  {

    j <- x[3:idx-1]
    k <- x[idx:colCount]

    identical(unname(j),unname(k))
  }


  idx <- grep(".y",colnames(dfJoined))[1]
  colCount <- ncol(dfJoined)
  ident <- apply(dfJoined,1,function(x) isChanged(x,idx,colCount))

  dfJoined <- as.data.frame(dfJoined)

  dfJoined <- cbind(dfJoined,ident)

  dfChanged <- dfJoined[dfJoined$ident == FALSE,!grepl(".x",colnames(dfJoined))]

  #Clean up objects
  rm(dfJoined,ident)
  gc()

  return(list(DFDeletes = dfDeletes, DFAdds = dfAdds, DFChanges = dfChanged))
}


