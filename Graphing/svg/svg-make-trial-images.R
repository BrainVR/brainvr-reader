svg_make_trial_images = function(dt_position, test, indices = c(), col_len = 5){
  indices = if (length(indices) == 0) get_trial_event_indices(test, "Finished") else indices
  ls = list()
  SIZE = c(250, 250)
  transform = list(translate = c(SIZE/2), scale = c(4,4))
  for (i in 1:length(indices)){
    ls[[i]] = svgR(wh = SIZE, svg_make_trial_image(dt_position, test, i, transform = transform))
  }
  images = '<div class = "trails">'
  images = paste(images, '<div class = "column">', sep = '\n', collapse = '')
  for (i in 1:length(ls)){
    if (i %% col_len == 0){
      images = paste(images, '</div>', '\n', '<div class = "column">', sep = '\n', collapse = '')
    }
    div = '<div class = "trail">'
    div = paste(div, ls[[i]], sep = '\n', collapse = "")
    div = paste(div, '</div>', sep = '\n', collapse = "")
    images = paste(images, div, sep = '\n', collapse = "")
  }
  images=  paste(images, '</div>', sep = '\n', collapse = "")
  cat(images, file = 'test.html')
}