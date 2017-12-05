add_special_paths = function(position_table, ls){
  list_names = names(ls)
  position_table = position_table[, special:= "normal"]
  list_names = names(ls)
  for (i in 1:length(ls)){
    position_table = position_table[is_between(Time,ls[[i]]$start,ls[[i]]$finish), special:= list_names[i] ]
  }
  return(position_table)
}