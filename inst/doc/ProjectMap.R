## ----setup, include = FALSE----------------------------------------------
library(knitr)
library(rmarkdown)
library(xtable)

opts_chunk$set(eval = T, echo = F, message = F, warning = F, results = "asis", 
               fig.cap = "", fig.align = "center", out.width = "95%", out.height = "95%")
options(xtable.comment = FALSE)

## ---- echo = T, eval = F-------------------------------------------------
#  devtools::install_github("opendoor-labs/projectmap")

## ---- echo = T, eval = F-------------------------------------------------
#  library(projectmap)
#  link_to_proj()

## ---- echo = T, eval = F-------------------------------------------------
#  set_proj_models(
#    Example = T,
#    Model1 = F
#  )

## ---- echo = T, eval = F-------------------------------------------------
#  if(run_proj_model("Example")){
#    source_file("Example File.R", inFolder = NULL)
#  }
#  if(run_proj_model("Model1")){
#    source_file("Model1.R", inFolder = "Codes")
#  }

## ---- echo = T, eval = F-------------------------------------------------
#  rm(list = ls())
#  #Load projectmap
#  library(projectmap)
#  #Link this file as part of the project
#  link_to_proj()
#  #Load other required packages
#  library(ggplot2)
#  library(data.table)

## ---- echo = T, eval = F-------------------------------------------------
#  library(projectmap)
#  #Branch or clone
#  branch("Project Master.R") #or clone()
#  #Make changes to your branched files
#  #Merge changes
#  mege_branch("Project Master.R")
#  #Review the changes
#  #Push the changes
#  push_merge("Project Master.R")

## ---- echo = T, eval = F-------------------------------------------------
#  library(projectmap)
#  branch("Project Master.R")

## ---- echo = T, eval = F-------------------------------------------------
#  clone()

## ---- echo = T, eval = F-------------------------------------------------
#  merge_branch("Project Master.R")

## ---- echo = T, eval = F-------------------------------------------------
#  push_merge("Project Master.R")

