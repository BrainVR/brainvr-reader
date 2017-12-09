#pure helpers for my particular unity logging 
# translates list of positions. 
# RETURNS  null is some translations fail. That is on PURPOSE.
mirror_positions_list <- function(ls_positions){
  ls_mirrored <- list()
  listNames <- names(ls_positions)
  for(name in listNames){
    df <- ls_positions[[name]]
    mirrored <- mirror_positions_df(df)
    if(is.null(mirrored)) {
      print(paste0("Couldn't mirror data.frame ", name))
      return(NULL)
    } else {
      ls_mirrored[[name]] <- mirrored
    }
  }
  return(ls_mirrored)
}

mirror_positions_df <- function(df){
  if(!is_valid_positions_df(df)) return(NULL)
  df$Position.x <- -df$Position.x
  df$Position.z <- -df$Position.z
  ## flips all rotations in dt_player 
  ## all player logs should be data.table, otehrwise this breaks
  df_colnames <- colnames(df)
  rotation_cols <- df_colnames[grep("Rotation", df_colnames)]
  for (column in rotation_cols){
    df[, (column):= angle_to_360(df[, (get(column) - 180)])]
  }
  return(df)
}

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