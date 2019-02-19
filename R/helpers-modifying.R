# MIRRORING ----
mirror_positions_list <- function(ls_positions){
  ls_mirrored <- apply_transformation(ls_positions, mirror_positions_df)
  return(ls_mirrored)
}

# TODO - move to navr
mirror_positions_df <- function(df){
  if(!is_valid_positions_df(df)) return(NULL)
  df$position_x <- -df$position_x
  df$position_z <- -df$position_z
  ## flips all rotations in dt_player 
  ## all player logs should be data.table, otehrwise this breaks
  df_colnames <- colnames(df)
  rotation_cols <- df_colnames[grep("Rotation", df_colnames)]
  for (column in rotation_cols){
    df[, (column):= navr::angle_to_360(df[, (get(column) - 180)])]
  }
  df <- add_angle_differences(df)
  return(df)
}

# TRANSLATING -----
translate_positions_list <- function(ls_positions, offset){
  if(!is_valid_offset(offset)) return(NULL)
  ls_translated <- apply_transformation(ls_positions, translate_positions_df, offset)
  return(ls_translated)
}
# TODO - move to navr
translate_positions_df <- function(df, offset){
  if(!is_valid_positions_df(df)) return(NULL)
  if(!is_valid_offset(offset)) return(NULL)
  df$position_x <- df$position_x + offset[1]
  df$position_y <- df$position_y + offset[2]
  df$position_z <- df$position_z + offset[3]
  return(df)
}

# RESIZING ----
resize_positions_list <- function(ls_positions, multiplier){
  ls_resized <- apply_transformation(ls_positions, resize_positions_df, multiplier)
  return(ls_resized)
}
# TODO - move to navr
resize_positions_df <- function(df, multiplier){
  if(!is_valid_positions_df(df)) return(NULL)
  df$position_x <- df$position_x * multiplier
  df$position_y <- df$position_y * multiplier
  df$position_z <- df$position_z * multiplier
  # the calulated distances need to be recalculated
  # BUT this is tricky, because we only want to do that for player log, not for other tables
  if(!is.null(df$timestamp)) df <- add_distance_moved(df)
  return(df)
}

# DRY HELPERS ----
# procedure = string with name, only for reporting
# list function

#' Title
#'
#' @param obj Brainvr Object
#' @param procedure string name of the procedure
#' @param df_function function to apply to data frames
#' @param list_function function to apply to lists
#' @param value value used to transform - usually numeric vector, defines scale, offset etc.
#'
#' @return
#'
#' @examples
#' @noRd
transform_object <- function(obj, procedure, df_function, list_function, value){
  UseMethod('transform_object')
}
transform_object.brainvr <- function(obj, procedure, df_function, list_function, value){
  if(missing(value)){
     transformed_player <- df_function(obj$data$player_log)
  } else {
    transformed_player <- df_function(obj$data$player_log, value)
  }
  if(is.null(transformed_player)){
    print(paste0("Couldn't ", procedure," positions in player log. Have you preprocessed it correctly? Quitting."))
    return(obj)
  }
  if(missing(value)){
    transformed_positions <- list_function(obj$data$experiment_log$positions)
  } else {
    transformed_positions <- list_function(obj$data$experiment_log$positions, value)
  }
  if(is.null(transformed_positions)){
    print(paste0("Couldn't ", procedure, " positions in expeirment log. Have you preprocessed it correctly? Quitting."))
    return(obj)
  }
  obj$data$player_log <- transformed_player
  obj$data$experiment_log$positions <- transformed_positions
  return(obj)
}

apply_transformation <- function(ls_positions, fun, value){
  ls_transformed <- list()
  listNames <- names(ls_positions)
  for(name in listNames){
    df <- ls_positions[[name]]
    #mirroring doesn't pass value
    if(missing(value)) {
      transformed <- fun(df)
    } else {
      transformed <- fun(df, value)
    }
    if(is.null(transformed)) {
      print(paste0("Couldn't transform data.frame ", name))
      return(NULL)
    } else {
      ls_transformed[[name]] <- transformed
    }
  }
  return(ls_transformed)
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
