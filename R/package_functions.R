#' Redefined library, require, and install.packages functions to only look in the project library
#' This overwrites the base library function to only look in the user's project library to load a package
#'
#' for package version control
#' @param lib.loc Path to users package library
#' @param ... First, package name, then other named arguments passed to the base library function
#' @return Normally library returns (invisibly) the list of attached packages, but TRUE or FALSE if logical.return is TRUE.
#' When called as library() it returns an object of class "libraryIQR", and for library(help=), one of class "packageInfo".
#' @examples
#' library(projectmap)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
library = function(..., lib.loc = proj.env$libPath){
  base::library(..., lib.loc = lib.loc)
}
#' This overwrites the base require function to only look in the user's project library to load a package
#' for package version control
#'
#' @param lib.loc Path to users package library
#' @param ... First, package name, then other named arguments passed to the base require function
#' @return Normally library returns (invisibly) the list of attached packages, but TRUE or FALSE if logical.return is TRUE.
#' When called as library() it returns an object of class "libraryIQR", and for library(help=), one of class "packageInfo".
#' @examples
#' require(projectmap)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
require = function(..., lib.loc = proj.env$libPath){
  base::require(..., lib.loc = lib.loc)
}
#' This overwrites the base install.packages function to only install the function in the user's project library
#' for package version control
#'
#' @param lib Path to users package library
#' @param ... First, package name, then other named arguments passed to the base install.packages function
#' @return Normally library returns (invisibly) the list of attached packages, but TRUE or FALSE if logical.return is TRUE.
#' When called as library() it returns an object of class "libraryIQR", and for library(help=), one of class "packageInfo".
#' @examples
#' install.packages("packageName")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
install.packges = function(..., lib = proj.env$libPath){
  utils::install.packages(..., lib = lib)
}

#' Lock all the project variables
#'
#' @return No return value
#' @description Lock all project environment variablers
#' @examples
#' lock_proj()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
lock_proj = function(){
  for(i in names(proj.env)){
    lockBinding(i, proj.env)
  }
}

#' Unlock all the project variables
#'
#' @return No return value
#' @description Unlock all project environment variablers
#' @examples
#' unlock_proj()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
unlock_proj = function(){
  for(i in names(proj.env)){
    if(bindingIsLocked(i, proj.env)){
      unlockBinding(i, proj.env)
    }
  }
}

#' Set the project models to be executed from the "Project Master.R" file
#'
#' @param ... The names and boolean values assigned to each model (i.e. set_proj_models(Model1 = T, model2 = F, ...))
#' @return No return value
#' @description Assign a name and boolean (T, F) indicator to the project models you want to run when you source "Project Master.R".
#' @examples
#' link_to_proj()
#' set_proj_models(
#'    Model1 = T,
#'    model2 = F
#' )
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
set_proj_models = function(...){
  unlock_proj()
  #Assign the models to a named list
  proj.env$models = list(...)

  #Read in the Project Master.R file and parse out all the execute_proj_model statuements
  #to find those that are set to T or F and count the number of source_file statements
  lines = readLines(paste0(proj.env$root.dir, "/Project Master.R"))
  lines = unname(sapply(lines, function(x){
    if(grepl("#", x)){
      loc = gregexpr("#", x)[[1]][1]
      return(trimws(gsub("#", "", substr(x, 1, loc))))
    }else{
      return(trimws(x))
    }
  }))
  lines = gsub(" ", "", paste(lines, collapse = ""))
  loc1 = gregexpr("if\\(execute_proj_model\\(", lines)[[1]]
  loc2 = sapply(loc1, function(x) {
    gregexpr("\\}", substr(lines, x + 1, nchar(lines)))[[1]][1] +
      x
  })
  blocks = lapply(1:length(loc1), function(x) {
    block = substr(lines, loc1[x], loc2[x])
    model = gsub("\"", "", gsub("\\{", "", gsub("\\)", "", gsub("if\\(execute_proj_model\\(", "",
                                                                substr(block, 1, gregexpr("\\{", block)[[1]])))))
    models = names(which(unlist(proj.env$models)))
    if (any(models == model)) {
      return(block)
    }
    else {
      return(NULL)
    }
  })
  blocks = blocks[which(sapply(blocks, function(x) {
    !is.null(x)
  }))]
  proj.env$numFiles = sum(sapply(1:length(blocks), function(x) {
    length(gregexpr("source_file\\(", blocks[x])[[1]])
  }))

  #Set the master progress bar, counter, trace.message, and startSourceLog to their default values
  proj.env$pb = utils::txtProgressBar(min = 0,
                                      max = ifelse(proj.env$numFiles == 0, 1, proj.env$numFiles),
                                      initial = NA, char = "=", style = 3)
  proj.env$pbCounter = 0
  proj.env$trace.message = list()
  proj.env$startSourceLog = F
  lock_proj()
}

#' Return boolean of project model to be executed from the Project Master.R file.
#'
#' @param ... The names and boolean values assigned to each model (i.e. set_proj_models(Model1 = T, model2 = F, ...))
#' @return Boolean (T,F) indicator
#' @description Returns a boolean (T, F) indicator used as a flag telling "Project Master.R" whether to run a group of R scripts.
#' @examples
#' if(execute_proj_model("Model1")){
#'   source_file("Model1.R", inFolder = "Codes")
#' }
#' )
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
execute_proj_model = function(model){
  return(proj.env$models[[model]])
}

#' Reset the project environment
#'
#' @param build Boolean (T, F) indicator of whether to rebuild the cabinet or not. Default is F.
#' @param newroot Boolean (T, F) indicator of whether to reset root.dir
#' @return No return value
#' @description Resets the project environment to its default values.
#' @examples
#' reset_proj_env()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
reset_proj_env = function(build = F, newroot = F){
  unlock_proj()

  proj.env$startSourceLog = T
  proj.env$current.dir = NULL
  if(!is.null(proj.env$numFiles)){
    proj.env$pb = utils::txtProgressBar(min = 0, max = ifelse(proj.env$numFiles == 0, 1, proj.env$numFiles), initial = NA, char = "=", style = 3)
  }
  proj.env$pbCounter = 0
  proj.env$trace.message = ""
  proj.env$file = NULL
  if(build == T){
    build_cabinet()
  }
  if(newroot == T){
    proj.env$root.dir = NULL
  }

  lock_proj()
}

# #' Exit a project packrat mode
# #'
# #' @return No return value
# #' @description Wrapper for packrat's disable() function
# #' @examples
# #' exit_proj()
# #' @author Alex Hubbard (hubbard.alex@gmail.com)
# #' @export
# exit_proj = function(){
#   packrat::disable()
# }

#' Creates a project environment variable
#'
#' @description An environment variable
#' @examples
#' names(proj.env)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
proj.env = new.env()
proj.env$project.name = "Project"
proj.env$R.dev.version = "3.5.0"
proj.env$required.packages = c("rstudioapi", "R.utils", "utils", "stats", "pacman", "rJava", "xlsx", "tools", "devtools",
                               "ggplot2", "data.table", "parallel", "doSNOW", "foreach", "grDevices", "rmarkdown", "projectmap")
if(get("R.dev.version", envir = proj.env) != paste(R.Version()$major, R.Version()$minor, sep = ".")){
  warning.message = paste0("projectmap was built under R version ", get("R.dev.version", envir = proj.env), ". Your current R version is ", paste(R.Version()$major, R.Version()$minor, sep = "."), ".")
}

#Lock the project
lock_proj()

#' Parse out packages to load
#'
#' @param files A vector of character strings giving the full file path to the R scripts
#' @return A vector of character strings representing package names
#' @description Parse out packages from library and require function calls in R scripts
#' @examples
#' get_packages("Project Master.R")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
get_packages = function(files, parallel = T){
  if(parallel == T){
    cl = parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    `%fun%` = foreach::`%dopar%`
  }else{
    `%fun%` = foreach::`%do%`
  }
  packages = foreach::foreach(i = files, .combine = "c", .export = c("proj.env", "remove_file")) %fun% {
    if(file.exists(i)){
      lines = suppressWarnings(readLines(i))
      if(length(lines) > 0){
        libraries = trimws(unique(lines[which(sapply(gregexpr("library\\(", lines), function(x){x[1] != -1}))]))
        requires = trimws(lines[which(sapply(gregexpr("require\\(", lines), function(x){x[1] != -1}))])
        if(length(libraries) > 0){
          for(j in 1:length(libraries)){
            temp = trimws(strsplit(gsub("library\\(|\\)", "", libraries[j]), ",")[[1]])
            temp = temp[!grepl("=", temp)]
            libraries[j] = temp
          }
          rm(temp, j)
        }
        if(length(requires) > 0){
          for(j in 1:length(requires)){
            temp = trimws(strsplit(gsub("require\\(|\\)", "", requires[j]), ",")[[1]])
            temp = temp[!grepl("=", temp)]
            requires[j] = temp
          }
          rm(temp, loc, j)
        }
        return(unique(c(libraries, requires)))
      }else{
        return(NULL)
      }
      rm(lines)
    }else{
      remove_file(i)
      return(NULL)
    }
  }
  if(parallel == T){
    parallel::stopCluster(cl)
    rm(cl)
  }
  return(packages)
}

#' Get the project root directory
#'
#' @param app Boolean (T, F) indicator to tell the function that it is being executed from within the app directory
#' @return No return value
#' @description Updates the project environment with the root and current directories
#' @examples
#' get_proj_root()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
get_proj_root = function(app = F){
  unlock_proj()

  frames = unique(sys.parents())
  frames = seq(min(frames), max(frames), 1)
  found_wd = F
  proj.env$current.dir = proj.env$file
  for(i in rev(frames)){
    proj.env$current.dir = c(proj.env$current.dir, tryCatch(dirname(parent.frame(i)$ofile),
                                          error = function(err){NULL}))
  }
  if(Sys.getenv("RSTUDIO") == "1"){
    proj.env$current.dir = unique(c(proj.env$current.dir,
                           tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),
                                    error = function(err){NULL}),
                           tryCatch(dirname(rstudioapi::getSourceEditorContext()$path),
                                    error = function(err){NULL}),
                           tryCatch(dirname(rstudioapi::getConsoleEditorContext()$path),
                                    error = function(err){NULL})))
  }
  proj.env$current.dir = unique(c(proj.env$current.dir, getwd()))
  if(!all(is.null(proj.env$current.dir))){
    for(i in proj.env$current.dir){
      proj.env$root.dir = i
      for(j in 1:(length(gregexpr("/", i)[[1]]) + 1)){
        if(file.exists(paste0(proj.env$root.dir, "/Project Master.R"))){
          found_wd = T
          proj.env$current.dir = i
          break
        }else{
          proj.env$root.dir = dirname(proj.env$root.dir)
        }
      }
      if(found_wd == T){
        if(proj.env$root.dir == "."){
          proj.env$root.dir = getwd()
        }
        break
      }
    }
  }
  if(found_wd == F){
    unlock_proj()
    if(Sys.getenv("RSTUDIO") == "1"){
      proj.env$root.dir = rstudioapi::selectDirectory(caption = "Select Project Directory", label = "Select", path = NULL)
    }else{
      proj.env$root.dir = getwd()
    }
  }
  if(app == T){
    unlock_proj()
    proj.env$root.dir = dirname(proj.env$current.dir)
  }
  lock_proj()
}

#' Set the path to the project library
#'
#' @return No return value
#' @description Sets the path to the project library
#' @examples
#' set_proj_lib()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
set_proj_lib = function(){
  unlock_proj()

  if(is.null(proj.env$libPath.orig)){
    proj.env$libPath.orig = .libPaths()
  }
  proj.env$libPath = paste0(proj.env$root.dir, "/Library")
  .libPaths(new = proj.env$libPath)
  message("Project package library path set to ", .libPaths()[1], ".\n")

  lock_proj()
}

#' Exit a project
#'
#' @param reset_lib Boolean (T, F) indicator to reset the library path to its original state
#' @return No return value
#' @description Exits a project by detaching the projectmap package and resetting the library path
#' @examples
#' exit_proj()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
exit_proj = function(reset_lib = T){
  orig.lib = proj.env$libPath.orig
  proj.lib = proj.env$libPath
  suppressMessages(pacman::p_unload(projectmap))
  if(reset_lib == T){
    .libPaths(new = orig.lib)
    message("projectmap package detached. Library path reset to ", .libPaths()[1], ".\n")
  }else{
    message("projectmap package detached. Library path remains ", .libPaths()[1], ".\n")
  }
}

#' Link a script to the project
#'
#' @param init Boolean (T, F) indicator of wheter to reset the project environment.
#' @param app Boolean (T, F) indicator to tell the function that it is being executed from within the app directory
#' @return No return value
#' @description Link an R (or Rmd) script to the project environment so that it will be integrated with the
#' "Project Master.R" script created at the set up of the project.
#' @details This is the most important function in the package. It first looks for the project root by
#' looking for the folder path to "Project Master.R". If it does not find this file, it will prompt the
#' user to specify a path. Once the user specifies the path, the "Project Master.R" file will be
#' automatically created in that folder along with "Example File.R". It then stores this folder as
#' "root.dir" in the package environment.
#'
#' Once the root directory is set, it will build the folder structure if it does not exist. This
#' structure consists of folders "Codes", "Functions", "Input", "Output", "Documentation", and "Logs".
#' You should store all your R scripts in "Codes" except R scripts containing functions to be used
#' by other R scripts. Those should be stored in "Functions". All input data used R scripts should
#' be store in "Input". All output data and images should be stored in "Output". All documenation
#' files created by Rmd scripts should be stored in "Documentation". "Logs" will contain output
#' information created after sourcing "Project Master.R".
#'
#' Next, the file cabinet is built and saved in the "Functions" folder. If the cabinet already exists,
#' it will load it into the project environment.
#'
#' Finally, the function searches all R scripts for "library" and "require" keywords. It will check that
#' all packages in these keywords are installed, and if they are not, install them. If any package
#' versions differ from previous runs, it will prompt to update for the correct package version. If any
#' of the bigrquery, bigQueryR, googledrive, or googlesheets packages are used in any of the R scripts,
#' it will open a web browser and ask the user to verify access. Authenication tokens will be stored in
#' the project folder to prevent this from happening on future runs. The authentication token may be
#' updated periodically.
#'
#' If the project root is already stored in the package environment, the above will not be run and will
#' only set the "current.file" in the package environment that stores the path of the current R script.
#' @examples
#' link_to_proj(init = F)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
link_to_proj = function(init = F, app = F){
  if(Sys.getenv("RSTUDIO") != "1"){
    warning("Should be using RStudio.")
  }

  if(!exists("root.dir", proj.env)){
    if(init == T){
      reset_proj_env()
      history = list.files(pattern = "\\.Rhistory", recursive = F, all.files = T)
      proj.history = list.files(patter = "\\.Rproj\\.user", recursive = F, all.files = T)
      if(length(history) > 0){
        invisible(file.remove(history))
      }
      if(length(proj.history) > 0){
        invisible(unlink(proj.history, recursive = T))
      }
      rm(history, proj.history)
    }
    unlock_proj()

    #Finds the enclosing folder of the "Master.R" file and sets it as the working directory
    get_proj_root()
    setwd(proj.env$root.dir)
    message("Project root directory set to ", getwd(), ".\n")
    message("Directory of current script is ", proj.env$current.dir, ".\n")
    set_proj_lib()

    #Create the folder structure
    folders = c("./Codes", "./Functions", "./Input", "./Output", "./Documentation", "./Logs", "./Library", "./App")
    if(!basename(proj.env$root.dir) %in% gsub("\\./", "", folders)){
      if(!file.exists("Example.R")){
        write(x = exampleFile, file = "Example File.R")
      }
      if(!file.exists("Project Master.R")){
        write(x = masterFile, file = "Project Master.R")
      }
      for(i in folders){
        if(!dir.exists(i)){
          dir.create(i)
          if(i == "./App"){
            if(!file.exists("./App/global.R")){
              write(x = globalR, file = "./App/global.R")
            }
            if(!file.exists("./App/ui.R")){
              write(x = uiR, file = "./App/ui.R")
            }
            if(!file.exists("./App/server.r")){
              write(x = serverR, file = "./App/server.R")
            }
          }
        }
      }
    }
    suppressWarnings(rm(folders, i))

    #Build the file cabinet
    if(!file.exists("./Functions/cabinet.RData") | init == T){
      #If the file cabinet does not exist, create it
      message("Building project file cabinet...")
      build_cabinet()
      message(paste0(paste(rep("\b", nchar("Building project file cabinet... ")), collapse = ""), "Building project file cabinet...Done."))
    }else{
      #If the file cabinet already exists, load it
      message("Loading file cabinet...")
      load("./Functions/cabinet.RData", envir = proj.env)
      message(paste0(paste(rep("\b", nchar("Loading file cabinet... ")), collapse = ""), "Loading file cabinet...Done."))
    }

    #Find the R files to parse for required packages
    message("Checking required packages...")
    unlock_proj()
    proj.env$required.packages = unique(c(proj.env$required.packages, get_packages("Project Master.R", parallel = F)))
    rfiles = proj.env$cabinet[grepl("\\.R", proj.env$cabinet) & !grepl("Project Master.R", proj.env$cabinet)]
    rfiles = rfiles[unique(c(which(substr(rfiles, nchar(rfiles) - 1, nchar(rfiles)) == ".R"),
                             which(substr(rfiles, nchar(rfiles) - 3, nchar(rfiles)) == ".Rmd")))]
    rfiles = rfiles[!basename(rfiles) %in% c(paste0(proj.env$project.name, "Master.R"), paste(proj.env$project.name, "Mapping.R"))]
    packages = proj.env$required.packages
    if(length(rfiles) > 0){
      packages = unique(c(packages, get_packages(rfiles, parallel = T)))
    }
    rm(rfiles)
    message(paste0(paste(rep("\b", nchar("Checking required packages... ")), collapse = ""), "Checking required packages...Done."))

    if(!is.null(packages)){
      packages = packages[!packages %in% c("projectmap", installed.packages(lib.loc = proj.env$libPath))]
      packages = packages[!packages %in% rownames(installed.packages(priority = "base"))]
      if(length(packages) > 0){
        message("Installing packages...")
        for(i in packages){
          pacman::p_install(i, character.only = T, quiet = T, verbose = F, dependencies = T, lib = proj.env$libPath)
        }
      }
      if("projectmap" %in% installed.packages(lib.loc = proj.env$libPath) & length(packages) > 0){
        message("Done.")
      }
    }
    if(!"projectmap" %in% installed.packages(lib.loc = proj.env$libPath)){
      pacman::p_install_gh("opendoor-labs/projectmap", quiet = T, verbose = F, dependencies = T, reload = F, lib = proj.env$libPath)
      message("Done.")
    }

    #Link to Google BiqQuery and Google Drive if necessary
    if("bigrquery" %in% packages){#installed.packages(lib.loc = proj.env$libPath)){
      if(!".httr-oauth" %in% packages){#list.files(path = proj.env$root.dir, all.files = T, recursive = F) & "bigrquery" %in% packages){
        invisible(bigrquery::bq_projects())
      }
    }
    if("bigQueryR" %in% packages){#installed.packages(lib.loc = proj.env$libPath)){
      if(!"bq.oauth" %in% list.files(path = proj.env$root.dir, all.files = T, recursive = F)){
        invisible(bigQueryR::bqr_auth())
      }
    }
    if("googledrive" %in% packages){#installed.packages(lib.loc = proj.env$libPath)){
      if(!".httr-oauth" %in% list.files(path = proj.env$root.dir, all.files = T, recursive = F)){
        invisible(googldedrive::drive_auth())
      }
    }
    if("googlesheets" %in% packages){#installed.packages(lib.loc = proj.env$libPath)){
      if(!".httr-oauth" %in% list.files(path = proj.env$root.dir, all.files = T, recursive = F)){
        invisible(googlesheets::gs_auth())
      }
    }
    rm(packages)

    #Create the location of the master log and define the progress bar variables
    unlock_proj()
    proj.env$logLocation = paste("./Logs", paste(proj.env$project.name, "Master Log", Sys.Date()), sep = "/")
    proj.env$startSourceLog = F

    if(basename(proj.env$current.dir) == "App"){
      unlock_proj()
      proj.env$root.dir = proj.env$current.dir
      setwd(proj.env$current.dir)
      message("\nProject root directory reset to ", getwd(), ".\n")
    }

    lock_proj()
    message("\nProject environment set.\n")
  }else{
    unlock_proj()
    get_proj_root()
    setwd(proj.env$root.dir)
    message("Project root directory set to ", getwd(), ".\n")
    message("Directory of current script is ", proj.env$current.dir, ".\n")
    #packrat::packrat_mode(on = T, auto.snapshot = F, clean.search.path = F)
    set_proj_lib()
    if(basename(proj.env$current.dir) == "App"){
      unlock_proj()
      proj.env$root.dir = proj.env$current.dir
      setwd(proj.env$current.dir)
      message("Project root directory reset to ", getwd(), ".\n")
    }
    lock_proj()
    message("\nProject environment set.\n")
  }
}

#' Builds the file cabinet
#'
#' @return No return value
#' @description Searches through all folders in the project directory for all files to store in
#' the cabinet. All paths are stored relative to the projects working directory and stored in a package environment variable called cabinet.
#' @examples
#' build_cabinet()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
build_cabinet = function(){
  unlock_proj()

  cabinet = unlist(lapply(c("./Codes", "./Functions", "./Input",
                            "./Output", "./Documentation", "./Logs"), function(x) {
                              unique(list.files(path = x, recursive = T, full.names = T, include.dirs = F))
                            }))
  cabinet = unique(c(cabinet, list.files(path = ".", recursive = F, full.names = T, include.dirs = F)))
  dirs = unique(list.dirs(path = ".", full.names = T, recursive = F))
  cabinet = cabinet[!cabinet %in% dirs]
  save(cabinet, file = "./Functions/cabinet.RData")
  proj.env$cabinet = cabinet

  lock_proj()
}

#' Adds a file to the file cabinet
#'
#' @param file A character string giving the full path to the specified file (i.e. "./Codes/Model 1/Model1.R").
#' @return No return value
#' @description Adds a file path to the cabinet with the path being relative to the project's
#' working directory. The cabinet is saved as a package environment variable.
#' @examples
#' add_to_cabinet()
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
add_to_cabinet = function(file){
  unlock_proj()

  root = gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", proj.env$root.dir))
  file = gsub(root, ".", file)
  cabinet = gsub("//", "/", gsub(proj.env$root.dir, "\\.", unique(sort(c(proj.env$cabinet, file)))))
  save(cabinet, file = paste0(proj.env$root.dir, "/Functions/cabinet.RData"))
  proj.env$cabinet = cabinet

  lock_proj()
}

#' Removes a file from the project directory and the file cabinet
#'
#' @param files A vector of character strings containing full file
#' paths of files to removed. The files will be deleted from the actual
#' folders on memory and in the project environment's cabinet.
#' @return No return value
#' @description Removes files from the project directory and the file cabinet.
#' @examples
#' remove_file(c("./Codes/Model1.R", "./Codes/Model1.R"))
#' remove_file(get_file_path("Model1.R"))
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
remove_file = function(files){
  unlock_proj()

  #Files should be full file paths, can be more than 1
  for(i in files){
    if(file.exists(i)){
      invisible(file.remove(i))
    }
  }
  #root = gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", proj.env$root.dir))
  #files = gsub(root, "\\.", files)
  cabinet = unique(sort(proj.env$cabinet[!proj.env$cabinet %in% files]))
  save(cabinet, file = paste0(proj.env$root.dir, "/Functions/cabinet.RData"))
  proj.env$cabinet = cabinet

  lock_proj()
}

#' Get a file path relative to the root directory
#'
#' @param file A character string giving the name of the file to get the full folder path for (i.e. "Project Master.R").
#' @param inFolder An identifer to narrow the search in case there are multiple files with same name but in different folders (i.e. "Codes/Model1").
#' @param recall A boolean (T, F) indicator specifying whether to rebuild the cabinet if the file was not found the first time. The default is T.
#' @param allowMult A boolean (T, F) indicator specifying whether to allow multiple file paths upon return. The default is F.
#' @param full A boolean (T, F) indicator specifying whether to return a relative or full file path. Default is F.
#' @return A character string giving the file path of the specified file to the root directory.
#' @description Returns the relative file path to the file to the root directory. It searches in the package environment variable cabinet for the path.
#' @examples
#' get_file_path("Model1.R")
#' get_file_path("Model.R", inFolder = "Codes")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
get_file_path = function(file, inFolder = NULL, recall = T, allowMult = F, full = F){
  #Get the file extenstion
  ext = tools::file_ext(file)
  if(ext == ""){
    stop("No extension included in file. File must end in '.ext'.")
  }
  #Find all the paths that contain the file name in its name
  if(!is.null(inFolder)){
    #Find the correct drawer in the file cabinet
    drawer = unique(proj.env$cabinet[grepl(inFolder, proj.env$cabinet)])
    paths = unique(drawer[grepl(file, drawer)])
  }else{
    paths = unique(proj.env$cabinet[grepl(file, proj.env$cabinet)])
  }
  paths = paths[tools::file_ext(paths) == ext]
  paths = paths[basename(paths) == file]
  if(is.null(inFolder) & length(paths) > 1){
    paths = unique(paths[grepl(gsub(proj.env$root.dir, ".", get_output_dir()), paths)])
  }
  paths = paths[which.min(nchar(paths))]
  if(length(paths) == 1 | allowMult == T){
    #If only one unique file or allow multiple uniique files
    ret = paths
  }else if(length(paths) == 0){
    if(recall == T){
      #If the file is not found, rebuild the cabinet to check if it is there
      build_cabinet()
      ret = get_file_path(file = file, inFolder = inFolder, recall = F, allowMult = allowMult)
    }else{
      #If the file is still not found
      stop("File not found. Make sure the file exists or check the file name.")
    }
  }else if(length(paths) > 1){
    if(allowMult == F){
      stop("Found multiple matching files. Make 'inFolder' argument more specific or make file names unique.")
    }else{
      ret = paths
    }
  }
  if(full == F){
    return(gsub("//", "/", ret))
  }else{
    gsub("//", "/", paste0(proj.env$root.dir, substr(ret, 2, nchar(ret))))
  }
}

#' Get a full folder path
#'
#' @param file A character string giving the name of the file to get the full folder path for (i.e. "Project Master.R").
#' @param inFolder An identifer to narrow the search in case there are multiple files with same name but in different folders (i.e. "Codes/Model1").
#' @param recall A boolean (T, F) indicator specifying whether to rebuild the cabinet if the file was not found the first time. The default is T.
#' @param allowMult A boolean (T, F) indicator specifying whether to allow multiple file paths upon return. The default is F.
#' @return A character string giving the full folder path to the specified file.
#' @description Returns the full folder path to the file. It searches in the package environment variable cabinet for the path.
#' @examples
#' get_folder_path("Model1.R")
#' get_folder_path("Model.R", inFolder = "Codes")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
get_file_folder = function(file, inFolder = NULL, recall = T, allowMult = F){
  path = get_file_path(file = file, inFolder = inFolder, recall = recall, allowMult = allowMult)
  return(dirname(path))
}

#' Get the desired output directory
#'
#' @param doc Boolean (T, F) indicator of whether to set the output directory to the "Output" folder (used for R scripts calling the function) or the "Documentation" folder (used for Rmd scripts calling the function).
#' @return A character string giving a full folder path for the desired output directory.
#' @description Gets a standard output directory path to be used for saving data and graphical objects based on the folder path of the R script the function is used in.
#' @details The output directory will be selected to mimic the structure of the Codes folder (created at the set up of the project) and any subfolders created afterwords.
#' If the R script exists in "Users/alexhubbard/Project/Codes/Model1" then the output directory will be "Users/alexhubbard/Project/Output/Model1". If an script is insted
#' an Rmd file, the output directory will change to "Users/alexhubbard/Project/Documentation/Model1".
#' @examples
#' link_file()
#' get_output_dir()
#' get_output_dir(doc = T)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
get_output_dir = function(doc = F, app = F){
  #folder should be the full file path to the folder not including its name
  basefolders = list.dirs(path = proj.env$root.dir, recursive = F, full.names = F)
  rootfolder = names(which(sapply(basefolders, function(x){grepl(x, proj.env$current.dir)})))
  if(length(rootfolder) == 0){
    rootfolder = names(which(sapply(basefolders, function(x){grepl(x, proj.env$file)})))
    outputDir = dirname(trimws(paste0(ifelse(doc == T, "./Documentation", ifelse(app == T, "./App", "./Output")),
                                      substr(proj.env$file, gregexpr(rootfolder, proj.env$file)[[1]] + nchar(rootfolder), nchar(proj.env$file)))))

  }else{
    outputDir = trimws(paste0(ifelse(doc == T, "./Documentation", ifelse(app == T, "./App", "./Output")),
                              substr(proj.env$current.dir, gregexpr(rootfolder, proj.env$current.dir)[[1]] + nchar(rootfolder), nchar(proj.env$current.dir))))
  }
  if(doc == T){
    loc1 = gregexpr("/Documentation", outputDir)[[1]][1] + nchar("/Documentation")
    str = substr(outputDir, loc1, nchar(outputDir))
    loc2 = gregexpr("/", str)[[1]][2]
    loc2 = ifelse(is.na(loc2), nchar(str) + 1, loc2)
    outputDir = gsub("//", "/", paste0(substr(outputDir, 1, loc1), substr(str, 1, loc2 - 1)))
  }
  if(!dir.exists(outputDir)){
    #If an output directory doesn't exist, create it
    dir.create(outputDir, showWarnings = F, recursive = T)
  }
  return(gsub("//", "/", outputDir))
}

#' Read in a file in a standardized way
#'
#' @param file A character string giving the name of the file to get the full folder path for (i.e. "Project Master.R").
#' @param inFolder An identifer to narrow the search in case there are multiple files with same name but in different folders (i.e. "Codes/Model1").
#' @param showProgress A boolean (T, F) indicator specifying whether to show the read in progress if using data.table's fread.
#' @param na.strings A vector of character strings to convert to NA
#' @param ... Other arguments to pass to data.tables fread, the base load or readRDS, or xlsx's read.xlsx.
#' @return A data object (data.table or data.frame).
#' @description A wrapper function to read in a file containing data. It uses the file extenstion to determine whether to
#' use the base load or readRDS function for RData and rds files, data.table's fread function for csv files, or xlsx's read.xlsx for xls and xlsx files.
#' @examples
#' read_file("Model1.R")
#' read_file("Model1.R", inFolder = "Codes")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
read_file = function(file, inFolder = NULL, showProgress = F,
                na.strings = c("NULL","NA","na","N/A","n/a","<NA>","NONE","-",".",""," ","NaN","nan","Inf","-Inf"), ...){
  #File needs to be full file path
  file = get_file_path(file, inFolder)
  ext = tools::file_ext(file)

  if(ext == "RData"){
    load(file = file, envir = .GlobalEnv, ...)
  }else if(ext %in% c("csv", "txt")){
    return(data.table::fread(input = file, na.strings = na.strings, showProgress = showProgress, stringsAsFactors = stringsAsFactors, ...))
  }else if(ext == "rds"){
    return(readRDS(file = file, ...))
  }else if(ext %in% c("xls", "xlsx")){
    return(xlsx::read.xlsx(file = file, ...))
  }else{
    stop("Data extension must be RData, csv, txt, xls, xlsx, or rds")
  }
}

#' Source a file for the Project Master.R file execution.
#'
#' @param file A character string giving the name of the file to get the full folder path for (i.e. "Project Master.R").
#' @param inFolder An identifer to narrow the search in case there are multiple files with same name but in different folders (i.e. "Codes/Model1").
#' @return No return value
#' @description A wrapper function for the base source command but also perfoms some backend functions to track the progress of the
#' files executed in "Project Master.R" as well as updating the project progress bar. This function should only be used in the "Project Master.R" script.
#' @examples
#' link_file(init = T)
#' set_proj_models(
#'   Model1 = T,
#'   Model2 = T
#' )
#' if(execute_proj_model("Model1")){
#'   source_file("Model1.R", inFolder = "Codes")
#' }
#' if(execute_proj_model("Model2")){
#'   source_file("Model2.R", inFolder = "Codes")
#' }
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
source_file = function(file, inFolder = NULL){
  #If logging hasn't been started, start it
  unlock_proj()
  if(proj.env$startSourceLog == F){
    proj.env$startSourceLog = T
    proj.env$trace.message[[length(proj.env$trace.message) + 1]] = paste0("Start Time: ", Sys.time())
    cat(proj.env$trace.message[[1]], file = paste0(proj.env$logLocation, ".txt"), "\n", append = F)
  }
  #Get the file path and add to the project environment variables so it won't be removed
  unlock_proj()
  proj.env$file = get_file_path(file, inFolder)
  proj.env$current.dir = dirname(proj.env$file)
  #Prevent file info from being removed
  proj.env$trace.message[[length(proj.env$trace.message) + 1]] = paste("Executing", paste0("\"", basename(proj.env$file), "\""), "...")

  #Print info to screen
  cat("\014")
  cat(paste(proj.env$trace.message, collapse = "\n"), "\n")
  utils::setTxtProgressBar(proj.env$pb, proj.env$pbCounter)

  #Source the file
  lock_proj()
  assign("last.warning", NULL, envir = baseenv())
  if(tools::file_ext(proj.env$file) == "R"){
    invisible(capture.output(suppressMessages(source(proj.env$file, chdir = T))))
  }else if(tools::file_ext(proj.env$file) == "Rmd"){
    invisible(capture.output(suppressMessages(rmarkdown::render(proj.env$file, quiet = T, clean = T, knit_root_dir = proj.env$root.dir, output_dir = get_output_dir(doc = T)))))
  }else{
    stop("File extension must be either .R or .Rmd")
  }
  setwd(proj.env$root.dir)

  #Log the output
  unlock_proj()
  proj.env$current.dir = proj.env$root.dir
  proj.env$file = NULL
  proj.env$trace.message[[length(proj.env$trace.message)]] = paste0(proj.env$trace.message[[length(proj.env$trace.message)]], "Done.")
  cat(paste0("\n", proj.env$trace.message[[length(proj.env$trace.message)]]), file = paste0(proj.env$logLocation,".txt"), append = T)
  cat("\n", paste0(names(last.warning), "\n"), file = paste0(proj.env$logLocation, ".txt"), "\n", append = T)

  #Update the progress bar
  if(utils::getTxtProgressBar(proj.env$pb) > proj.env$numFiles){
    proj.env$pbCounter = proj.env$numFiles - 1
  }else if(utils::getTxtProgressBar(proj.env$pb) < proj.env$numFiles){
    proj.env$pbCounter = proj.env$pbCounter + 1
  }
  if(proj.env$pbCounter == proj.env$numFiles){
    cat("\014")
    proj.env$trace.message[[length(proj.env$trace.message) + 1]] = paste0("Finish Time: ", Sys.time())
    cat(paste(proj.env$trace.message, collapse = "\n"), "\n")
    utils::setTxtProgressBar(proj.env$pb, proj.env$pbCounter)
    cat("\n")
    cat(paste0("\n", proj.env$trace.message[[length(proj.env$trace.message)]]), file = paste0(proj.env$logLocation, ".txt"), "\n", append = T)
    reset_proj_env()
  }
  #Detach all packages except the required packages
  suppressWarnings(suppressMessages(pacman::p_unload(pacman::p_loaded()[!pacman::p_loaded() %in% proj.env$required.packages], character.only = T)))
  lock_proj()
}

#' A modified sum function
#'
#' @param x A vector of numeric values
#' @param na.rm A boolean (T, F) indicator of whether to include NA values or not in the calculation.
#' @return A numeric value
#' @description Corrects the problem of returning 0 when adding up a vector of all NA's when na.rm = T
#' @examples
#' sum_dt(c(1, 2, NA, 3), na.rm = T)
#' DT[, sum(col, na.rm = T)]
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
sum_dt = function(x, na.rm = F){
  #If all items in a vector are NA, return NaN
  if(all(is.na(x))){
    return(NaN)
  }else{
    #Otherwise sum as usual
    return(sum(x, na.rm = na.rm))
  }
}

#' A modified ggsave function
#'
#' @param filename File name to create on disk.
#' @param plot Plot or plots (in a list) to save, defaults to last plot displayed.
#' @param device Device to use. Can be either be a device function (e.g. png), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param path Path to save plot to (combined with filename).
#' @param combine Boolean (T, F) indicator of whether or not to combine the output from a list of ggplot items passed in plot to a single pdf.
#' @param ... Other arguments passed on to graphics device.
#' @return No return value
#' @description Redefined ggsave function from ggplot2 allowing to combine a list of ggplot items into a single pdf output. This function is used in the save function defined in this package.
#' @examples
#' See ggplot2 package for examples.
#' @author Alex Hubbard (hubbard.alex@gmail.com)
ggsave2 = function(filename, plot = last_plot(), device = NULL, path = NULL, combine = F,
                  scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
                  dpi = 300, limitsize = TRUE, ...){
  plot_dev = function(device, filename, dpi = 300){
    if(is.function(device)){
      return(device)
    }
    eps = function(...){
      grDevices::postscript(..., onefile = FALSE, horizontal = FALSE, paper = "special")
    }
    devices = list(eps = eps, ps = eps, tex = function(...) grDevices::pictex(...),
                   pdf = function(..., version = "1.4") grDevices::pdf(...,
                                                                       version = version), svg = function(...) svglite::svglite(...),
                   emf = function(...) grDevices::win.metafile(...), wmf = function(...) grDevices::win.metafile(...),
                   png = function(...) grDevices::png(..., res = dpi, units = "in"),
                   jpg = function(...) grDevices::jpeg(..., res = dpi, units = "in"),
                   jpeg = function(...) grDevices::jpeg(..., res = dpi,
                                                        units = "in"), bmp = function(...) grDevices::bmp(...,
                                                                                                          res = dpi, units = "in"), tiff = function(...) grDevices::tiff(...,
                                                                                                                                                                         res = dpi, units = "in"))
    if(is.null(device)){
      device = tolower(tools::file_ext(filename))
    }
    if(!is.character(device) || length(device) != 1){
      stop("`device` must be NULL, a string or a function.", call. = FALSE)
    }
    dev = devices[[device]]
    if(is.null(dev)){
      stop("Unknown graphics device '", device, "'", call. = FALSE)
    }
    dev
  }
  plot_dim = function(dim = c(NA, NA), scale = 1, units = c("in", "cm", "mm"), limitsize = TRUE){
    units = match.arg(units)
    to_inches = function(x) x/c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
    from_inches = function(x) x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
    dim = to_inches(dim) * scale
    if(any(is.na(dim))){
      if(length(grDevices::dev.list()) == 0){
        default_dim = c(7, 7)
      }
      else{
        default_dim = grDevices::dev.size() * scale
      }
      dim[is.na(dim)] = default_dim[is.na(dim)]
      dim_f = prettyNum(from_inches(dim), digits = 3)
      message("Saving ", dim_f[1], " x ", dim_f[2], " ", units, " image")
    }
    if(limitsize && any(dim >= 50)){
      stop("Dimensions exceed 50 inches (height and width are specified in '",
           units, "' not pixels). If you're sure you want a plot that big, use ",
           "`limitsize = FALSE`.", call. = FALSE)
    }
    dim
  }
  dev = plot_dev(device, filename, dpi = dpi)
  dim = plot_dim(c(width, height), scale = scale, units = units, limitsize = limitsize)
  if(!is.null(path)) {
    filename = file.path(path, filename)
  }
  dev(file = filename, width = dim[1], height = dim[2], ...)
  on.exit(utils::capture.output(grDevices::dev.off()))
  if(combine == T){
    for(i in 1:length(plot)){
      grid::grid.draw(plot[[i]])
    }
  }else{
    grid::grid.draw(plot)
  }
  invisible()
}

#' Grid of ggplot objects
#'
#' @param g A single ggplot object or a list of ggplot object defined as list(g1, g2, ...).
#' @param ... Other arguements passed to grid.arrange.
#' @return A plot
#' @description Modifies grid.arrange from the gridExtra package to take a list of objects of ggplot objects.
#' @examples
#' library(ggplot2)
#' g1 = ggplot(iris) +
#'   ggtitle("Title", subtitle = "Subtitle") +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species), size = 4) +
#'   od_theme()
#'
#' g2 = ggplot(iris) +
#'   ggtitle("Title", subtitle = "Subtitle") +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species), size = 4) +
#'   od_theme(n = length(unique(iris$Species)))
#'
#' ggplot_grid(list(g1, g2), nrow = 1, ncol = 2)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
ggplot_grid = function(g, ...){
  if(!ggplot2::is.ggplot(g)){
    if(!all(sapply(g, function(x){"ggplot" %in% class(x)}))){
      stop("All objects in g must be ggplot objects")
    }
    return(eval(parse(text = paste0("gridExtra::grid.arrange(", paste(paste0("g[[", 1:length(g), "]]"), collapse = ", "), ", ...)"))))
  }else{
    if(!"ggplot" %in% class(g)){
      stop("g must be a ggplot object")
    }
    return(gridExtra::grid.arrange(g, ...))
  }
}

#' Save a file
#'
#' @param ... An object to be saved, including any other arguments to be passed to the save function associated with the file extension.
#' @param file A character string giving the name of the file, including the extension, to be saved.
#' @param file.override A character string giveing the full file path if want to override the default the output directory.
#' @param plot If wanting to save a ggplot object, plot should be assigned the ggplot object.
#' @param doc Boolean (T, F) to change output directory to Documenation instad of Output. Default is F.
#' @param app Boolean (T, F) to change output directory to App instead of Output. Default is F.
#' @param ... Other arguments to pass to data.tables fread, the base load or readRDS, or xlsx's read.xlsx.
#' @return A data object (data.table or data.frame).
#' @description The function uses the file extension to select the appropriate save function to use. By default
#' it uses get_output_dir to set the output directory to save the output to. By default, get_output_dir sets the
#' working to directory to mimic the folder structure of "Output" to "Codes" by finding the folder path of the current R script.
#' @examples
#' data = data.table(x = 1:10, y = 1:10)
#' save_file(data, file = "data.RData")
#'
#' g = ggplot(data) +
#'   ggtitle("Example Plot", subtitle = "Simple Plot") +
#'   geom_line(aes(x = x, y = y)) +
#'   od_theme(colors = "blue")
#'
#' save_file(plot = g, file = "plot.png")
#'
#' g
#' save_file(file = "plot.png")
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
save_file = function(..., file = NULL, file.override = NULL, row.names = F, showProgress = F, paper = "USr", combine = F,
                width = 9, height = 5, units = "in", pointsize = 12, bg = "white", fg = "black", res = 300,
                append = F, plot = last_plot(), doc = F, app = F){
  if(is.null(file) & is.null(file.override)){
    stop("No file given.")
  }
  if(!is.null(file.override)){
    file = file.override
  }
  #File needs to be only the subdirectory and file name: i.e. "subdir/filename.csv"
  ext = tools::file_ext(file)
  if(ext != ""){
    if(!ext %in% c("RData","rds","csv","jpeg","png","tiff","bmp","pdf","xlsx")){
      stop("File extension must be one of RData, rds, csv, jpeg, png, tiff, bmp, pdf, xlsx.")
    }
  }else{
    stop("No file extension.")
  }
  if(combine == T & ext != "pdf"){
    stop("If combine = T, image extension must be a pdf.")
  }

  if(is.null(file.override)){
    outputDir = gsub("//", "/", paste(get_output_dir(doc = doc, app = app), gsub("\\.", "", dirname(file)), sep = "/"))
  }else{
    outputDir = dirname(file)
  }
  if(substr(outputDir, nchar(outputDir), nchar(outputDir)) != "/"){
    outputDir = paste0(outputDir, "/")
  }
  if(!dir.exists(outputDir)){
    #Create the folder if it doesn't exist
    dir.create(outputDir, showWarnings = F, recursive = T)
  }

  file = trimws(gsub("//", "/", paste0(outputDir, basename(file))))
  #Save the file using the specified function and data ext
  if(ext == "RData"){
    save(..., file = file)
  }else if(ext == "rds"){
    saveRDS(..., file = file)
  }else if(ext == "csv"){
    data.table::fwrite(..., file = file, row.names = row.names, showProgress = showProgress, append = append)
  }else if(ext %in% c("jpeg","png","tiff","bmp","pdf")){
    if(combine == F & (ggplot2::is.ggplot(plot) | grid::is.grob(plot))){
      ggsave2(filename = basename(file), plot = plot, device = ext, path = dirname(file), width = width, height = height, units = units, dpi = res)
      catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
    }

    #Default width:height is 16:9 ratio.
    if(ext == "jpeg"){
      grDevices::jpeg(file, width = width, height = height, units = units, pointsize = pointsize, bg = bg, res = res, ...)
      catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
    }else if(ext == "png"){
      grDevices::png(file, width = width, height = height, units = units, pointsize = pointsize, bg = bg, res = res, ...)
      catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
    }else if(ext == "tiff"){
      grDevices::tiff(file, width = width, height = height, units = units, pointsize = pointsize, bg = bg, res = res, ...)
      catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
    }else if(ext == "bmp"){
      grDevices::bmp(file, width = width, height = height, units = units, pointsize = pointsize, bg = bg, res = res, ...)
      catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
    }else if(ext == "pdf"){
      #Set paper = "US" for portrait. Defaults to "USr" for landscape. "special" sets paper to width and height.
      if(combine == T & all(sapply(plot, function(x){ggplot2::is.ggplot(x) | grid::is.grob(x)}))){
        ggsave2(filename = basename(file), plot = plot, device = "pdf", path = dirname(file), width = width, height = height, units = units, dpi = res,
               bg = ifelse(bg == "white", "transparent", bg), fg = fg, pointsize = pointsize, paper = paper, combine = combine, ...)
        catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
      }else{
        grDevices::pdf(file, width = width, height = height, bg = ifelse(bg == "white", "transparent", bg),
            fg = fg, pointsize = pointsize, paper = paper, ...)
        catch = tryCatch(grDevices::dev.off(), error = function(err){NULL})
      }
    }
  }else if(ext == "xlsx"){
    xlsx::saveWorkbook(..., file = file)
  }
  #Add the file to the cabinet and save the cabinet
  add_to_cabinet(file)
  message("File saved to ", dirname(file), ".")
  Sys.sleep(0.01)
}

#' Creates the Opendoor color scheme
#'
#' @description A vector of named hexidecimal colors
#' @examples
#' od.colors
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
od.colors = c(
  blue = "#1c85e8", navy = "#1d2c4c",
  iris = "#ab80de", turquoise = "#68e1c7", citrine = "#ffc259", ruby = "#ff7f82",
  lightgrey = "#babcbc", bluegrey = "#7b9cb1", coolgrey = "#506d7e", warmgrey = "#525975",
  lightgreytint = "#f0f0f0"
)
#' Opendoor's ggplot2 theme
#'
#' @param palette A character string giving the name of one of the palattes ("main", "cool", "warm", "grey").
#' @param discrete A boolean(T, F) indicator setting whether to use a discrete or continuous theme. The default is F.
#' @param addblack A boolean(T, F) indicator if want to add black to the color palatte. The default is F.
#' @param n An optional numeric value specifying the number of discrete colors to include in the palette.
#' @param colors An optional character vector of colors to use.
#' @param reverse A boolean(T, F) indicator for whether to reverse the color palette. The default is F.
#' @return A ggplot theme object
#' @description The full list of colors of the palette are: blue = "#1c85e8", navy = "#1d2c4c",
#' iris = "#ab80de", turquoise = "#68e1c7", citrine = "#ffc259", ruby = "#ff7f82",
#' lightgrey = "#babcbc", bluegrey = "#7b9cb1", coolgrey = "#506d7e", warmgrey = "#525975",
#' lightgreytint = "#f0f0f0".
#' @examples
#' library(ggplot2)
#' ggplot(iris) +
#'   ggtitle("Title", subtitle = "Subtitle") +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species), size = 4) +
#'   od_theme()
#'
#' ggplot(iris) +
#'   ggtitle("Title", subtitle = "Subtitle") +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species), size = 4) +
#'   od_theme(n = length(unique(iris$Species)))
#'
#' ggplot(iris) +
#'   ggtitle("Title", subtitle = "Subtitle") +
#'   geom_point(aes(x = Sepal.Width, y = Sepal.Length, color = Species), size = 4) +
#'   od_theme(colors = c("turquoise", "citrine", "iris"))
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
od_theme = function(palette = "main", discrete = T, reverse = F, addblack = F,
                    n = NULL, colors = NULL){
  #Check the inputs
  if(!is.logical(discrete)){
    stop("discrete must be T or F")
  }
  if(!is.logical(reverse)){
    stop("reverse must be T or F")
  }
  if(!is.logical(addblack)){
    stop("addblack must be T or F")
  }

  #Predefine some palettes
  od_palettes = list(
    main = od.colors[!grepl("grey", names(od.colors))],
    cool = od.colors[c("navy", "blue", "bluegrey", "turquoise")],
    warm = od.colors[c("ruby", "iris", "citrine")],
    grey = c(od.colors[names(od.colors)[grepl("grey", names(od.colors)) & !grepl("tint", names(od.colors))]], "black")
  )
  if(addblack){
    od_palettes = lapply(od_palettes, function(x){unique(c(x, "black"))})
  }
  if(!palette %in% names(od_palettes)){
    stop(paste("palette must be one of", paste(names(od_palettes), collapse = ", ")))
  }

  #Function to get the palette
  od_pal = function(palette = "main", reverse = F, ...){
    pal = od_palettes[[palette]]
    if(reverse){
      pal = rev(pal)
    }
    return(grDevices::colorRampPalette(pal, ...))
  }

  #Function the create the color scale
  scale_color_od = function(palette = "main", discrete = T, reverse = F, ...){
    pal = od_pal(palette = palette, reverse = reverse)
    if(discrete){
      pal = ggplot2::discrete_scale("colour", paste0("od_", palette), palette = pal, ...)
    }else{
      pal = ggplot2::scale_color_gradientn(colours = pal(256), ...)
    }
    return(pal)
  }

  #Function to create the fill scale
  scale_fill_od = function(palette = "main", discrete = T, reverse = F, ...){
    pal = od_pal(palette = palette, reverse = reverse)
    if(discrete){
      pal = ggplot2::discrete_scale("fill", paste0("od_", palette), palette = pal, ...)
    }else{
      pal = ggplot2::scale_fill_gradientn(colours = pal(256), ...)
    }
    return(pal)
  }

  #Define the ggplot theme
  od.theme = ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = od.colors["lightgreytint"], size = 0.5),
          panel.grid.minor = ggplot2::element_line(color = od.colors["lightgreytint"], linetype = "dashed", size = 0.5),
          panel.border = ggplot2::element_rect(color = od.colors["warmgrey"], fill = NA),
          legend.position = "bottom",
          plot.title = ggplot2::element_text(size = rel(1.3), face = "bold"),
          plot.subtitle = ggplot2::element_text(size = rel(1.1)),
          axis.title = ggplot2::element_text(size = rel(1.1)),
          axis.text = ggplot2::element_text(color = "black"),
          legend.title = ggplot2::element_text(size = rel(1.1)))

  #Add the desired color palaette
  if(is.null(n) & is.null(colors)){
    #Default palettes
    od.theme = list(od.theme,
                    scale_color_od(palette = palette, discrete = discrete, reverse = reverse),
                    scale_fill_od(palette = palette, discrete = discrete, reverse = reverse))
  }else if(is.numeric(n) & is.null(colors)){
    #Numeric specified palette
    od.theme = list(od.theme,
                    ggplot2::scale_color_manual(values = unname(rep(c(od_palettes[[palette]], "black"), length.out = n))))
  }else if(!is.null(colors) & (is.null(n) | !is.numeric(n))){
    #Manual specified palette
    od.theme = list(od.theme,
                    ggplot2::scale_color_manual(values = unname(unique(c(od.colors[names(od.colors) %in% colors], colors)))))
  }else{
    stop("Error defining color palette")
  }
  return(od.theme)
}

#' Build a query for Google BigQuery from a text string
#'
#' @param query A character string written in SQL language.
#' @param standard Appends "#standardSQL" to the top if set to T. Default is F.
#' @param limit Appends "LIMIT = 1000", for example, or whatever integer value limit is set to. Default is NULL and will not append anything.
#' @param show A boolean (T, F) indicator of whether to print the query to the console for visualization.
#' @return A character string.
#' @description Takes a character string and concatenates some optional lines
#' @examples
#' query = "SELECT * FROM table"
#' build_query(query, standard = T, limit = 1000, show = T)
#' @author Alex Hubbard (hubbard.alex@gmail.com)
#' @export
build_query = function(query, standard = T, limit = NULL, show = F){
  standard = ifelse(standard == T, "#standardSQL", "")
  limit = ifelse(!is.null(limit), paste("LIMIT", limit), "")
  ret = paste0(standard, "\n", query, "\n", limit)
  if(show == T){
    cat(ret)
  }else{
    return(ret)
  }
}

masterFile = '###############################################################################
#Project Master
#
#This runs all files necessary to estimate the model in the proper order.
#
#Authors: Author Name (author.name@email.com)
###############################################################################
rm(list = ls())
cat("\\014")

#Load projectmap
if(!"projectmap" %in% installed.packages()){
  if(!"devtools" %in% installed.packages()){
    install.packages("devtools")
  }
  devtools::install_github("opendoor-labs/projectmap")
}
library(projectmap)

#Link this file as part of the project
link_to_proj(init = T)

#These will be placed in the project environment.
set_proj_models(
  Example = T,
  Model1 = F
)

#Run the selected files
if(execute_proj_model("Example")){
  source_file("Example File.R", inFolder = NULL)
}
if(execute_proj_model("Model1")){
  source_file("Model1.R", inFolder = "Codes")
}

rm(list = ls())
'

exampleFile = '###############################################################################
#Example File
#
#Example code for how to set up a file for this project
#
#Authors: Author Name (author.name@email.com)
###############################################################################
#Clear the workspace
rm(list = ls())

#Load projectmap
library(projectmap)

#Link this file as part of the project
link_to_proj()

#Load other required packages
library(ggplot2)
library(data.table)

data = data.table(x = 1:10, y = 1:10, variable = "line")
save_file(data, file = "data.RData")

g = ggplot(data) +
  ggtitle("Example Plot", subtitle = "Simple Plot") +
  geom_line(aes(x = x, y = y, color = variable)) +
  od_theme()

save_file(plot = g, file = "plot.png")
save_file(plot = list(g, g), file = "plot.pdf", combine = T)

rm(data)
read_file(file = "data.RData", inFolder = NULL)
'

globalR = paste0('###############################################################################
#Global App Script
#
#Script used by both server.R and ui.R
#
#Authors: Author Name (author.name@email.com)
###############################################################################
#Clear the workspace
rm(list = ls())

#Load all required packages for global.R, server.R, and ui.R
if(!"projectmap" %in% installed.packages()){
  if(!"devtools" %in% installed.packages()){
    install.packages("devtools")
  }
  devtools::install_github("opendoor-labs/projectmap")
}
library(projectmap)
link_to_proj()
library(tools)
library(data.table)
library(plotly)
library(ggplot2)
library(shiny)
library(shinydashboard)

###############################################################################
#Read in the data
###############################################################################
#This will assign a read in of a csv to its file name
#Make sure all file names are unique
#Will replace all spaces in object name to "_"
files = c(list.files(pattern = "\\\\.RData"), list.files(pattern = "\\\\.csv"))
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

###############################################################################
#Define parameters
###############################################################################
plot_ht = 500
sidebar_wd = 300
')

uiR = '###############################################################################
#UI Script
#
#Script used to generate the UI
#Click "Run App" at the top right to start the application correctly.
#
#Authors: Author Name (author.name@email.com)
###############################################################################

###############################################################################
#Define the header
###############################################################################
#Place "Logo.png" in a folder called "www" in the "App" folder
#header = dashboardHeader(title = tags$a(tags$img(src = "Logo.png", height = "50", width = "50"),
#"Title"), titleWidth = sidebar_wd)
header = dashboardHeader(title = "Title", titleWidth = sidebar_wd)

###############################################################################
#Defie the sidebar
###############################################################################
sidebar = dashboardSidebar(width = sidebar_wd,
  #hr(),
  sidebarMenu(
    menuItem("Menu Item", tabName = "menu_item", icon = icon("tachometer"), selected = T,
      selectInput(inputId = "menu_itme", label = "Input",
                  choices = c("Choice1", "Choice2"),
                  selected = "Choice1",
                  multiple = F)
    )#,
  )
  #hr()
)

###############################################################################
#Define the body
###############################################################################
body = dashboardBody(
  tags$head(tags$style(HTML(paste0("
    /* logo */
    .skin-blue .main-header .logo {
    background-color: ", od.colors["blue"],";
    color: #ffffff;
    font-style: bold;
    }
    /* logo */
    .skin-blue .main-header a {
    background-color: ", od.colors["blue"],";
    color: #ffffff;
    font-style: bold;
    }
    /* logo when hovered */
    .skin-blue .main-header .logo:hover {
    background-color: ", od.colors["blue"],";
    }
    /* navbar (rest of the header) */
    .skin-blue .main-header .navbar {
    background-color: ", od.colors["blue"],";
    }
    /* active selected tab in the sidebarmenu */
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
    /*background-color: ", od.colors["blue"],";*/
    font-size: 12pt;
    }
    /* other links in the sidebarmenu */
    .skin-blue .main-sidebar .sidebar .sidebar-menu a{
    /*background-color: #000000;*/
    color: #ffffff;
    font-size: 12pt;
    }
    /* other links in the sidebarmenu when hovered */
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
    /*background-color: ", od.colors["navy"],";*/
    font-size: 12pt;
    }
    /* toggle button when hovered  */
    .skin-blue .main-header .navbar .sidebar-toggle:hover{
    background-color: ", od.colors["navy"],";
    }
    ")))),

  #Plot
  fluidRow(
    box(width = 12, column(width = 12,
      plotlyOutput("plot", height = plot_ht)
    ))
  )
)

###############################################################################
#Define the ui
###############################################################################
ui = dashboardPage(
  header, sidebar, body
)
'
serverR = '###############################################################################
#Server Script
#
#Script used to generate the server
#Click "Run App" at the top right to start the application correctly.
#
#Authors: Author Name (author.name@email.com)
###############################################################################

###############################################################################
#Define the server
###############################################################################
server = function(input, output, session){

  #A plotly plot
  output$plot = renderPlotly({
    data = data.table(x = 1:10, y = 1:10)
    plot_ly(data, x = ~x, y = ~y, type = "scatter", mode = "lines")
  })
}
'

#Call these to build the package
#devtools::document()
#devtools::build_vignettes()
#devtools::install()
#library(projectmap)





