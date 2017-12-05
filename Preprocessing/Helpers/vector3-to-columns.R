#turns vector columns in string "(x, y, z)" into three columns(Position.x, Position.y, Position.z) and returns the table
vector3_to_columns = function(tab, column_name){
  xyz = c("x", "y", "z")
  splitted = strsplit(substring(tab[, get(column_name)], 2, nchar(tab[, get(column_name)]) - 1), ",")
  #turns the Vector3 into lists of 3 values
  i = 1
  for (letter in xyz){
    new_name = paste(column_name, letter, sep = ".")
    tab[, (new_name) := as.numeric(sapply(splitted, "[", i))]
    i = i + 1
  }
  return(tab)
}