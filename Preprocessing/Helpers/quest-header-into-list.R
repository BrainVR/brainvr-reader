#the header files ar writen in this format
## PROPERTY NAME: *VALUE*
## PROPERTY NAME 2: *VALUE 2*
#this function sorts that out into the list/dictionary of keys and values
into_list = function(text = ""){
  ls = list()
  #fir each line
  for (info in text) {
    #finds the PROEPRTY NAME
    split = str_split(info, pattern = ":", n = 2)
    #extract the value from the str_split list (this is a weird line but
    #strsplit creates a list of lists so we need to do this
    code = split[[1]][1]
    #extracting the VALUE from the second part of the list
    value = str_extract_all(split[[1]][2], "\\*(.*?)\\*")[[1]][1]
    #removing the *
    value = substring(value, 2, nchar(value) - 1)
    #saving into the list 
    ls[[code]] = value
  }
  return(ls)
}