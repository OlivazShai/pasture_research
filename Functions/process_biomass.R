library(glue)
library(sf)
library(terra)
library(exactextractr)
library(dplyr)

download_biomass <- function(api_links, folder_path) {
  # Check inputs
  if (!is.list(api_links)) {
    stop("Ensure `api_links` is a named list of links.")
  }
  
  # Loop through the api_links to download missing rasters
  for (i in seq_along(api_links)) {
    raster_name <- paste0(names(api_links)[i], ".tif")
    output_path <- file.path(folder_path, raster_name)
    
    # Check if the file already exists
    if (!file.exists(output_path)) {
      message(glue("Downloading {raster_name}..."))
      
      # Get the ith raster
      r <- terra::rast(api_links[[i]])
      
      # Save the raster to the specified folder
      terra::writeRaster(r, filename = output_path, overwrite = TRUE, filetype = "GTiff")
      message(glue("{raster_name} saved to {folder_path}"))
      
      # Clean up
      rm(r)
      gc()
    } else {
      message(glue("{raster_name} already exists in the folder, skipping download."))
    }
  }
}

###########################################################################

process_biomass <- function(folder_path, zones) {
  # Check inputs
  if (!inherits(zones, "sf")) {
    stop("Ensure `zones` is an sf object.")
  }
  
  # Step 1: Copy zones and convert to dataframe without geometry
  z <- zones |> 
    st_drop_geometry() |> 
    as_tibble()
  
  # Step 2: Initialize an empty list to store x_i columns
  x_columns <- list()
  
  # Step 3: Loop through the rasters in the folder
  raster_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  
  for (i in seq_along(raster_files)) {
    message(glue("Processing raster {i}/{length(raster_files)}: {basename(raster_files[i])}"))
    
    # Load the ith raster
    message("  Starting terra::rast()...")
    r <- terra::rast(raster_files[i])
    message("  Done with terra::rast().")
    
    # Perform exact_extract with "sum"
    message("  Starting exact_extract()...")
    x_i <- exactextractr::exact_extract(
      r, zones, fun = "sum"
    )
    message("  Done with exact_extract().")
    
    x_columns[[paste0("x_", i)]] <- x_i
    
    # Remove the raster object to save memory
    rm(r)
    gc()
  }
  
  # Step 4: Append all x_i vectors as columns to z
  z <- bind_cols(z, x_columns)
  
  # Step 5: Create a "biomass" column as the row sum of all x_i columns
  z <- z |> 
    mutate(biomass = rowSums(select(., starts_with("x_")), na.rm = TRUE)) |> 
    select(-starts_with("x_")) # Remove all x_i columns
  
  return(z)
}

