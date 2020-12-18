#' Preprocesses player log and returns it
#' @param navr_object loaded NavrObject
#' @return preprocessed player log
#' @export
preprocess_player_log <- function(navr_object) {
  navr_object <- navr::prepare_navr(navr_object)
  return(navr_object)
}

#' Saves preprocessed player ot hte given folder. Receives either specific name,
#' or gets the name from already present player logs. If there are multiple
#' player logs in the folder,takes the name from the first available
#' @param directory Where shoudl i save the file. Should end with a slash!
#' @param player_log table to be saved
#' @param orig_filename what is the name of the original file
#' @return
#'
#' @export
save_preprocessed_player <- function(directory, exp_timestamp = NULL, log,
                                     orig_filename = NULL, precision = 4) {
  if (is.null(orig_filename)) {
    ptr <- create_log_search_pattern("player", exp_timestamp)
    logs <- list.files(directory, pattern = ptr, full.names = TRUE)
    # Allow for overwrite
    if (length(logs) != 1) stop("More player logs in the saving directory. 
                               Have you deleted the preprocessed one?")
    filename <- logs[1]
    preprocessed_filename <- gsub(".txt", "_preprocessed.txt", filename)
  } else {
    preprocessed_filename <- paste0(directory, orig_filename)
    preprocessed_filename <- gsub(".txt", "_preprocessed.txt", filename)
  }
  print(paste0("Saving processed player log as", preprocessed_filename))
  write.table(format(log, digits = precision, nsmall = precision, trim = TRUE),
    preprocessed_filename,
    sep = ";",
    dec = ".", quote = FALSE, row.names = FALSE
  )
}

#' Converts output of unity logging a vector 3 to a vector of three
#'
#' @description Unity logs vector as (x, y, z). This function simply
#' splits it into three parts and returns a numeric(2 or 3)
#'
#' @param text Vector of"(x, y, z)" characters. If single value is passed, 
#' the result is returned as a vector. If multiple are passed, it is returned 
#' as a matrix with each row being single converted value
#'
#' @return matrix of vector of numbers
#' @export
#'
#' @examples
unity_vector_to_numeric <- function(text) {
  single_convert <- function(x){
    txt <- gsub("[()]", "", x)
    values_split <- strsplit(txt, ",")
    if (length(values_split[[1]]) > 2) {
      return(sapply(values_split[[1]], as.numeric,
        warning = FALSE,
        USE.NAMES = FALSE
      ))
    }
    return(NULL)
  }
  out <- sapply(text, single_convert, USE.NAMES = FALSE, simplify = TRUE)
  out <- t(out)
  if(nrow(out) == 1) out <- as.vector(out)
  return(out)
}
