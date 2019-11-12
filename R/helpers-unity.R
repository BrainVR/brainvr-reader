#turns vector columns in string "(x, y, z)" into three columns(position_x, position_y, position_z) and returns the table
vector3_to_columns <- function(df_position){
  if(!requireNamespace("stringr", quietly = T)){
    print("Cannot continue withouth stringr package. Please install it")
    return(F)
  }
  xyz <- c("x", "y", "z")
  #TODO - remvoe the data.table
  values_split <- strsplit(substring(df_position[, get("Position")], 2, nchar(df_position[, get("Position")]) - 1), ",")
  #turns the Vector3 into lists of 3 values
  i <- 1
  for (letter in xyz){
    #TODO - make this underscore and lowecase
    new_name <- paste("position", letter, sep = "_")
    #TODO - remove data.table
    df_position[, (new_name) := as.numeric(sapply(values_split, "[", i))]
    i <- i + 1
  }
  return(df_position)
}

#pure helpers for my particular unity logging 
position_to_vector <- function(list){
  listNames <- names(list)
  for(name in listNames){
    ls <- list[[name]]
    numberOfItems <- length(ls)
    df <- data.frame(position_x = numeric(numberOfItems), 
                     position_y = numeric(numberOfItems),
                     position_z = numeric(numberOfItems))
    for (i in 1:length(ls)){
      stringVector <- ls[i]
      df[i, ] <- unity_vector_to_numeric(stringVector)
    }
    list[[name]] <- df
  }
  return(list)
}