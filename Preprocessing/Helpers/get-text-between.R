get_text_between = function(text, string){
  indices = get_indicies_between(text, string)
  if (length(indices$beginning) != 1 || length(indices$end) != 1) return (NULL)
  text = text[(indices$beginning + 1):(indices$end - 1)]
  return(text)
}