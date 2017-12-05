replace_strings <- function(vec, strings, replacements){
  if(length(strings) != length(replacements)){
    cat("Strings and replllacements need to thave hte same lenght")
    return(NULL)
  }
  for (i in 1:length(strings)){
    vec[vec == strings[i]] <- replacements[i]
  }
  return(vec)
}