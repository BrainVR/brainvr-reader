json_to_list = function(text){
  return(TextToJSON(text))
}
TextToJSON = function(text){
  #JSON checking
  ls = fromJSON(text)
  return(ls)
}