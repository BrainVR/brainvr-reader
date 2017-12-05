text_to_vector3 = function(text){
  splitted = strsplit(substring(text,2,nchar(text)-1),",")
  if(length(splitted[[1]]) > 2) return(sapply(splitted[[1]], as.numeric, 
                                              warning = F, USE.NAMES = F))
  return(NULL)
}