StartUpRoutine <- function() {
  # Vector of libraries
  packages <- c("readxl", 
                "data.table", 
                "tidyverse", 
                "zoo", 
                "plotly", 
                "lubridate", 
                "DBI", 
                "RSQLite", 
                "refineR", 
                "rstudioapi", 
                "parallel",
                "stringdist",
                "knitr", 
                "parallel", 
                "robslopes", 
                "officedown", 
                "officer", 
                "mcr", 
                "flextable",
                "openxlsx",
                "CLSIEP15",
                "shiny",
                "shinythemes", 
                "shinyjs", 
                "shinycssloaders", 
                "DT",
                "quarto",
                "readr",
                "fs",
                "units",
                "scales"
                )
  
  # Loop to check if libraries are installed and install them if not and load them
  for (package in packages) {
    if (!(package %in% installed.packages())) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
  
  # Function to detect the operating system and return the corresponding database path
  getDatabasePath <- function() {
    # Detect operating system
    os <- Sys.info()["sysname"]
    
    # Set the path based on the operating system
    if (os == "Linux") {
      # Path for Linux (Ubuntu)
      # path <- "/home/olli/R_local/labStat/ClinicalChemistry_1.db"
      assign("path", "/home/olli/R_local/autoVal/ClinicalChemistry_1.db", envir = .GlobalEnv)
    } else if (os == "Windows") {
      # if (file.exists("H:\\R\\")) {
      # Path for Windows
      # Adjust the path as necessary for your Windows setup
      # path <- "C:/R_local/labStat/ClinicalChemistry_1.db"
      # assign("path", "H:/R/autoVal_H/ClinicalChemistry_2.db", envir = .GlobalEnv)
    # } else {
      assign("path", "C:/R_local/labStat/ClinicalChemistry_1.db", envir = .GlobalEnv)
         # }
    } else {
      stop("Operating system not supported")
    }
    
    return(path)
  }
  
  # Get the path of the current project
  # project_directory <- rstudioapi::getActiveProject()
  # assign("project_directory", project_directory, envir = .GlobalEnv)
  # 
  # # If running in an RStudio project, set the working directory to the project directory
  # # If not running in an RStudio project, print a message
  # if (!is.null(project_directory)) {
  #   setwd(project_directory)
  # } else {
  #   print("This R session is not running within an RStudio Project.")
  # }
  
  # Set database directory and connect to the database
  db.wd <- getDatabasePath()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db.wd)
  assign("con", con, envir = .GlobalEnv)
  
  return(invisible())
}
