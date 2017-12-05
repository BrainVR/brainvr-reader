get_indicies_between = function(text, string){
  ls = list()
  ls$beginning = which(grepl(create_separator(string)$beginning, text))
  ls$end = which(grepl(create_separator(string)$end, text))
  return(ls)
}