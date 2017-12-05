angle_from_positions = function(pos_from, pos_to){
  
  #' Zero vector is the vector given by the unity for calculation rotation of oabjects. 
  #' In 3D unity (different in UNREAL!) are exes X horizontal left/right, Y vertical up/down, and Z plane horizontal up/down
  #' The zero vector is vector when GameObject only changes its Z position, therefore if we calculate position as [X Z], 
  #' the normalised zero vector shoudl be 0 on X and 1 on Z - [0, 1]
  
  ZERO_VECTOR =  c(0, 1)
  target_vector = pos_to - pos_from
  
  if(length(pos_from) != 2 ){print("ERROR:angle_from_positions, DESCRIPTION:input does not have two 2d position")}
  if(length(pos_to) != 2){print("ERROR:angle_from_positions, DESCRIPTION:input does not have two 2d position")}
  
  # ATAN takes Y and X, but we want to scale it against Z axis, therefore Y in carthesian, so the input is reversed
  theta = atan2(target_vector[1], target_vector[2])
  angle = radian_to_angle(theta)
  return(angle)
}

radian_to_angle = function(radian){
  angle = radian/pi * 180
  if(angle < 0) angle = 360 + angle
  return(angle)
}