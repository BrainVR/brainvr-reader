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

load_brainvr_reader <- function(dir){
  source(paste(dir,"Helpers/source-folder.R", sep = "/"))
  source_folder(paste(dir,"Helpers/", sep = "/"))
  source_folder(paste(dir,"Exporting/", sep = "/"))
  source_folder(paste(dir,"Analysis/", sep = "/"))
  source_folder(paste(dir,"Classes/", sep = "/"))
  source_folder(paste(dir,"Preprocessing/", sep = "/"))
  source_folder(paste(dir,"Graphing/", sep = "/"))
  source_folder(paste(dir,"DataManipulation/", sep = "/"))
  source_folder(paste(dir,"Graphing/", sep = "/"))
  source_folder(paste(dir,"Results/", sep = "/"))
}
