##Helper for escaping characters in quest names
escapeRegex = function(string){
  return(gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string))
}