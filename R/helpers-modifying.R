#pure helpers for my particular unity logging 
# translates list of positions. 
# RETURNS  null is some translations fail. That is on PURPOSE.
translate_positions_list <- function(ls_positions, offset){
  if(!is_valid_offset(offset)) return(NULL)
  
  ls_translated <- list()
  listNames <- names(ls_positions)
  for(name in listNames){
    df <- ls_positions[[name]]
    translated <- translate_positions_df(df, offset)
    if(is.null(translated)) {
      print(paste0("Couldn't translate data.frame ", name))
      return(NULL)
    } else {
      ls_translated[[name]] <- translated
    }
  }
  return(ls_translated)
}

translate_positions_df <- function(df, offset){
  if(!is_valid_positions_df(df)) return(NULL)
  if(!is_valid_offset(offset)) return(NULL)
  df$Position.x <- df$Position.x + offset[1]
  df$Position.y <- df$Position.y + offset[2]
  df$Position.z <- df$Position.z + offset[3]
  return(df)
}

# VALIDATIONS ----

is_valid_positions_df <- function(df){
  position_table_colnames <- c("Position.x", "Position.y", "Position.z")
  return(all(position_table_colnames %in% colnames(df)))
}

is_valid_offset <- function(vec){
  if(length(vec) != 3){
    print("is_valid_offset::Offset doesn't have length of 3")
    return(FALSE)
  }
  return(TRUE)
}