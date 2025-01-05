StartUpRoutine <- function() {
  # Vector of libraries
  packages <- c("readxl", 
                "data.table", 
                "tidyverse",
                "tidymodels",
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
                "scales",
                "skimr",
                "pracma"
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
      assign("path", "/home/olli/R_local/labStat/ClinicalChemistry_1.db", envir = .GlobalEnv)
    } else if (os == "Windows") {
      assign("path", "C:/R_local/labStat/ClinicalChemistry_1.db", envir = .GlobalEnv)
    } else {
      stop("Operating system not supported")
    }
    
    return(path)
  }
  
  
  
  # Set database directory and connect to the database
  db.wd <- getDatabasePath()
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db.wd)
  assign("con", con, envir = .GlobalEnv)
  
  return(invisible())
}
