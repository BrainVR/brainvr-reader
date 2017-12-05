create_separator = function(string){
  ls = list()
  ls$beginning = paste("\\*\\*\\*\\",string, "\\*\\*\\*", sep="")
  ls$end = paste("\\-\\-\\-",string, "\\-\\-\\-", sep="")
  return(ls)
}