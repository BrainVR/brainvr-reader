#turns vector columns in string "(x, y, z)" into three columns(position_x, position_y, position_z) and returns the table
vector3_to_columns <- function(df_position){
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

text_to_vector3 <- function(text){
  values_split <- strsplit(substring(text, 2, nchar(text) - 1), ",")
  if(length(values_split[[1]]) > 2) return(sapply(values_split[[1]], as.numeric,
                                                  warning = F, USE.NAMES = F))
  return(NULL)
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
      df[i, ] <- text_to_vector3(stringVector)
    }
    list[[name]] <- df
  }
  return(list)
}