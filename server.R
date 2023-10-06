#Packages to be installed and loaded############################
library(shiny)
library(tidyverse)#includes dplyr, ggplot2, tidyr, tibble, purrr, stringr, and other packages that are useful
library(fst)#fst files are smaller and load faster than csv...we will save main database in this format
library(data.table)#used for fread, which is faster way to load .csv file. Also performs faster sorts, filters, merges than dplyr if large files are involved

par(bg = "white") #fixes problem where figures that are downloaded have grey background. This makes them white

# creelData <- fread(creeldata.csv)#this will eventually read in the main creel database


function(input, output, session) {
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Creel Planning Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Data Validation Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##Analysis Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##User's guide Tab code##########
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  
}