SEPARATOR_START <- "\\*\\*\\*"
SEPARATOR_END <- "\\-\\-\\-"

create_log_search_pattern <- function(log_name, exp_timestamp) {
  ptr <- paste0("_", log_name, "_")
  if (!is.null(exp_timestamp)) {
    ptr <- paste0(ptr, ".*", exp_timestamp)
  }
  return(ptr)
}

create_separator <- function(string) {
  ls <- list()
  ls$beginning <- paste0(SEPARATOR_START, string, SEPARATOR_START)
  ls$end <- paste(SEPARATOR_END, string, SEPARATOR_END, sep = "")
  return(ls)
}

load_headers <- function(filepath) {
  txt <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  ptr <- paste0(SEPARATOR_START, "(.*)", SEPARATOR_START)
  i_start <- which(grepl(ptr, txt))
  result <- list()
  for (i in i_start) {
    section_name <- gsub(SEPARATOR_START, "", txt[i])
    # TODO issue in case we have nested values of the same name
    if (section_was_serialised(result, section_name)) next
    section <- load_header_section(txt, section_name)
    section_name <- gsub(" ", "_", section_name)
    section_name <- tolower(section_name)
    result[[section_name]] <- section
  }
  return(result)
}

load_header_section <- function(text, section_name) {
  ls <- list()
  section_text <- get_text_between(text, section_name)
  ptr <- paste0(SEPARATOR_START, "(.*)", SEPARATOR_START)
  i_subsections <- which(grepl(ptr, section_text))
  if (length(i_subsections) > 0) {
    for (i in i_subsections) {
      section_name <- gsub(SEPARATOR_START, "", section_text[i])
      # TODO issue in case we have nested values of the same name
      if (section_was_serialised(ls, section_name)) next
      ls[[section_name]] <- load_header_section(section_text, section_name)
    }
  } else {
    ls <- get_json_between(text, section_name)
  }
  return(ls)
}

section_was_serialised <- function(ls, section_name) {
  all_names <- names(unlist(ls))
  # removes the past parameter name
  ptr <- "\\.(?:.(?!\\.))+$" # negative search from the last .
  all_names <- gsub(ptr, "", all_names, perl = T)
  return(any(grepl(section_name, all_names)))
}

get_bottom_header_index <- function(filepath) {
  txt <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  ptr <- paste0(SEPARATOR_END, "(.*)", SEPARATOR_END)
  i_end <- which(grepl(ptr, txt))
  return(tail(i_end, 1))
}

### TODO
### Can massively speed it up if only reads part of the text or do it line by line
get_indicies_between <- function(text, string) {
  ls <- list()
  ls$beginning <- which(grepl(create_separator(string)$beginning, text))
  ls$end <- which(grepl(create_separator(string)$end, text))
  return(ls)
}

get_json_between <- function(text, string) {
  ls <- json_to_list(get_text_between(text, string))
  return(ls)
}

get_text_between <- function(text, string) {
  indices <- get_indicies_between(text, string)
  if (length(indices$beginning) != 1 || length(indices$end) != 1) {
    return(NULL)
  }
  text <- text[(indices$beginning + 1):(indices$end - 1)]
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
