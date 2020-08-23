#turns vector columns in string "(x, y, z)" into three columns(position_x, position_y, position_z) and returns the table
vector3_to_columns <- function(df_position){
  if(!requireNamespace("stringr", quietly = TRUE)){
    print("Cannot continue withouth stringr package. Please install it")
    return(FALSE)
  }
  xyz <- c("x", "y", "z")
  #TODO - remove the data.table
  values_split <- strsplit(substring(df_position[, get("Position")], 2,
                                     nchar(df_position[, get("Position")]) - 1),
                           ",")
  #turns the Vector3 into lists of 3 values
  i <- 1
  for (letter in xyz){
    #TODO - make this underscore and lowercase
    new_name <- paste("position", letter, sep = "_")
    #TODO - remove data.table
    df_position[, (new_name) := as.numeric(sapply(values_split, "[", i))]
    i <- i + 1
  }
  return(df_position)
}

#pure helpers for my particular unity logging 
position_to_vector <- function(positions){
  res <- positions
  all_names <- names(positions)
  for(name in all_names){
    temp <- positions[[name]]
    n_items <- length(positions)
    df <- data.frame(position_x = numeric(n_items), 
                     position_y = numeric(n_items),
                     position_z = numeric(n_items))
    for (i in seq_len(length(temp))){
      df[i, ] <- unity_vector_to_numeric(temp[i])
    }
    res[[name]] <- df
  }
  return(res)
}