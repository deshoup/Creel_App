#Packages to be installed and loaded############################
library(shiny)
library(tidyverse)
library(data.table)#used for fread, which is faster way to load .csv file. Also performs faster sorts, filters, merges than dplyr if large files are involved

par(bg = "white") #fixes problem where figures that are downloaded have grey background. This makes them white

function(input, output, session) {
  
}