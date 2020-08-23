create_results_df <- function(column_names, column_types, nRows){
  if(length(column_names) != length(column_types)){
    warning("You have passed different size of colnames and coltypes. Cannot continue")
    return(NULL)
  }
  df <- as.data.frame(matrix(nrow = nRows, ncol = length(column_names)))
  colnames(df) <- column_names
  for(i in seq_len(length(column_types))){
    df[,i] <- create_column(nRows, column_types[i])
  }
  return(df)
}

create_column <- function(nRows, type){
  if(type == "string"){
    return(rep("", nRows))
  }
  if(type == "numeric"){
    return(rep(NA, nRows))
  }
  else{
    warning("The type", type, "you have passed cannot be used.")
    return(NULL)
  }
}