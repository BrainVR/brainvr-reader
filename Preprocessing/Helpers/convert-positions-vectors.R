#pure helpers for my particular unity logging 
position_to_vector = function(list){
  listNames = names(list)
  for(name in listNames){
    ls = list[[name]]
    numberOfItems = length(ls)
    df = data.frame(Position.x = numeric(numberOfItems), 
                    Position.y = numeric(numberOfItems),
                    Position.z = numeric(numberOfItems))
    for (i in 1:length(ls)){
      stringVector = ls[i]
      df[i, ] = text_to_vector3(stringVector)
    }
    list[[name]] = df
  }
  return(list)
}