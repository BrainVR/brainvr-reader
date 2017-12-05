#libraries in correct order
library('R6')
library('data.table')
library('plyr')
library('dplyr')
library('stringr')
library('png')
library('grid')
library('ggplot2')
library('jsonlite')
library('svgR')

load_brainvr_readr <- function(dir){
  source(paste(dir,"R/Helpers/source-folder.R", sep = "/"))
  source_folder(paste(dir,"R/Helpers/", sep = "/"))
  source_folder(paste(dir,"R/Exporting/", sep = "/"))
  source_folder(paste(dir,"R/Analysis/", sep = "/"))
  source_folder(paste(dir,"R/Classes/", sep = "/"))
  source_folder(paste(dir,"R/Preprocessing/", sep = "/"))
  source_folder(paste(dir,"R/Graphing/", sep = "/"))
  source_folder(paste(dir,"R/DataManipulation/", sep = "/"))
  source_folder(paste(dir,"R/Graphing/", sep = "/"))
  source_folder(paste(dir,"R/Results/", sep = "/"))
}
