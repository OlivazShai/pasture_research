# Function to write multiple objects to .rds files

  bulk_rds <- function(...) {
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

# Next functions...
