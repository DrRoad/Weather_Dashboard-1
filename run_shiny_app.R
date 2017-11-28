# Run Shiny App

library(shiny)

args = commandArgs(trailingOnly=TRUE)
port = as.numeric(args[1])
appdir = args[2]

options(warn=-1)
runApp(appdir, port=port)
