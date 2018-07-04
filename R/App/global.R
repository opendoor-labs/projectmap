###############################################################################
                 #Global Script
                 #
                 #Script used by both server.R and ui.R
                 #
                 #Authors: Author Name (author.name@email.com)
                 ###############################################################################
                 #Clear the workspace
                 rm(list = ls())
                 
                 #Load required packages
                 
                 #Read in the data
                 #This will assign a read in of a csv to its file name
                 #Make sure all file names are unique
                 #Will replace all spaces in object name to "_"
                 files = c(list.files(pattern = "\.RData"), list.files(pattern = "\.csv"))
                 for(i in files){
                 if(file_ext(i) == "RData"){
                 env = new.env()
                 load(i, envir = env)
                 if(exists(names(env), envir = .GlobalEnv)){
                 stop("An object with name", names(env), "already exists. Change the object name to be something unique.")
                 }
                 assign(names(env), get(names(env), env), .GlobalEnv)
                 rm(env)
                 }else if(file_ext(i) == "csv"){
                 name = gsub(" ", "_", basename(gsub(paste0(".", file_ext(i)), "", i)))
                 if(exists(name)){
                 stop("An object with name", name, "already exists. Change the file name to be something unique.")
                 }
                 assign(name, fread(i, showProgress = F), .GlobalEnv)
                 rm(name)
                 }
                 }
                 rm(i, files)
                 
                 plot_ht = 500
                 sidebar_wd = 300
                 
                 #Opendoor color palette
                 od.colors = c(blue = #1c85e8, navy = #1d2c4c, iris = #ab80de, turquoise = #68e1c7, citrine = #ffc259, ruby = #ff7f82, lightgrey = #babcbc, bluegrey = #7b9cb1, coolgrey = #506d7e, warmgrey = #525975, lightgreytint = #f0f0f0)
