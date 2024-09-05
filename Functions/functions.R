# Function to write multiple objects to .rds files

  bulk_write_rds <- function(...) {
    # Load library
    library(readr)
    # Capture the names of the objects passed to the function
    object_names <- as.list(match.call())[-1]
    # Iterate through each object
    for (object_name in object_names) {
      # Evaluate the object name to get the actual object
      object <- eval(object_name, envir = parent.frame())
      # Construct the file path
      file_path <- paste0("../Variables/", deparse(object_name), ".rds")
      # Save the object to an .rds file
      write_rds(object, file = file_path)
    }
  }

# Function to read .rds files from Variables
  
  bulk_read_rds <- function(...) {
    # Collect object names from the function arguments
    object_names <- as.character(match.call(expand.dots = FALSE)$`...`)
    
    # Loop through each object name
    for (obj_name in object_names) {
      # Construct the file path
      file_path <- file.path("../Variables", paste0(obj_name, ".rds"))
      
      # Check if the file exists
      if (!file.exists(file_path)) {
        stop(paste("File does not exist:", file_path))
      }
      
      # Load the RDS file
      loaded_data <- readRDS(file_path)
      
      # Assign the loaded data to an object with the same name
      assign(obj_name, loaded_data, envir = .GlobalEnv)
    }
  }
  
