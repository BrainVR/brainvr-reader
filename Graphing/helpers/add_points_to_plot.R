add_points_to_plot = function(plot, ls){
  list_names = names(ls)
  data_table = data.frame(point.x = numeric(0), point.y = numeric(0), point.name = character(), stringsAsFactors = F)
  for (i in 1:length(ls)){
    data_table[i, 1] = ls[[i]][1]
    data_table[i, 2] = ls[[i]][2]
    data_table[i, 3] = list_names[i]
  }
  plot = plot + geom_point(data = data_table, aes(point.x, point.y),size = 4, color = "blue") + geom_text(data = data_table, aes(point.x, point.y,label=point.name))
  return(plot)
}