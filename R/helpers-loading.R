SEPARATOR_START <- "\\*\\*\\*"
SEPARATOR_END <- "\\-\\-\\-"

create_log_search_pattern <- function(log_name, exp_timestamp) {
  ptr <- paste0("_", log_name, "_")
  if (!is.null(exp_timestamp)) {
    ptr <- paste0(ptr, ".*", exp_timestamp)
  }
  return(ptr)
}

create_header_separator <- function(string) {
  res <- list()
  res$start <- paste0(SEPARATOR_START, string, SEPARATOR_START)
  res$end <- paste0(SEPARATOR_END, string, SEPARATOR_END)
  return(res)
}

#' Loads the header portion from the log. Overall simply a wrapper around a 
#' \code{\link{load_header_section}} function which parses the header from a 
#' text
#'
#' @param filepath path to the file
#'
#' @return list of parsed settings
#'
#' @examples
#' @noRd
load_headers <- function(filepath) {
  txt <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  result <- load_header_section(txt)
  return(result)
}

#' Parses brainvr framework headers
#'
#' @param txt 
#'
#' @return
#'
#' @examples
#' @noRd
load_header_section <- function(txt) {
  res <- list()
  ptr <- create_header_separator("(.*)")$start
  i_subsections <- which(grepl(ptr, txt))
  if (length(i_subsections) > 0) {
    for (i in i_subsections) {
      section_name <- gsub(SEPARATOR_START, "", txt[i])
      # TODO issue in case we have nested values of the same name
      # Let's just say it won't happen
      name <- tolower(gsub("\\s+", "_", section_name))
      if(!header_section_was_serialised(res, name)){
        section_text <- get_text_between(txt, section_name)
        section <- load_header_section(section_text)
        res[[name]] <- section
      }
    }
  } else {
    res <- json_to_list(txt)
  }
  return(res)
}

get_bottom_header_index <- function(filepath) {
  txt <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  ptr <- create_header_separator("(.*)")$end
  i_end <- tail(which(grepl(ptr, txt)), 1)
  return(i_end)
}

header_section_was_serialised <- function(res, section_name) {
  all_names <- names(unlist(res))
  # removes the past parameter name
  ptr <- "\\.(?:.(?!\\.))+$" # negative search from the last .
  all_names <- gsub(ptr, "", all_names, perl = TRUE)
  return(any(grepl(section_name, all_names)))
}


### TODO
### Can massively speed it up if only reads part of the text or do it line by line
get_indicies_between <- function(text, string) {
  ls <- list()
  ls$start <- which(grepl(create_header_separator(string)$start, text))
  ls$end <- which(grepl(create_header_separator(string)$end, text))
  return(ls)
}

get_json_between <- function(text, string) {
  ls <- json_to_list(get_text_between(text, string))
  return(ls)
}

get_text_between <- function(text, string) {
  indices <- get_indicies_between(text, string)
  if (length(indices$start) != 1 || length(indices$end) != 1) {
    return(NULL)
  }
  text <- text[(indices$start + 1):(indices$end - 1)]
  return(text)
}

experiment_name_from_filename <- function(filename) {
  ptr <- "_test_(.*)_"
  if (!requireNamespace("stringr", quietly = T)) {
    stop("Needs stringr to continue")
  }
  capture_groups <- stringr::str_match(filename, ptr)
  return(capture_groups[, 2])
}

## Helper for escaping characters in quest names
escape_regex <- function(string) {
  return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string))
}

unlist_to_data_frame <- function(ls) {
  listNames <- names(ls)
  for (name in listNames) {
    row <- unlist(ls[[name]])
  }
}
