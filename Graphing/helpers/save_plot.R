save_plot = function(plt, name){
  mypath = paste("../images/", name, sep = "")
  file = png(mypath, width = 1200, height = 800, units = "px")
  plot(plt)
  dev.off()
}
