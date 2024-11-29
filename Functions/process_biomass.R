library(glue)
library(curl)
library(sf)
library(terra)
library(exactextractr)
library(dplyr)

download_biomass <- function(api_links, folder_path) {
  # Check inputs
  if (!is.list(api_links)) {
    stop("Ensure `api_links` is a named list of links.")
  }
  
  # Convert API links to a character vector and get output file paths
  download_links <- as.character(api_links)
  output_paths <- file.path(folder_path, paste0(names(api_links), ".tif"))
  
  # Identify files that need to be downloaded
  missing_links <- download_links[!file.exists(output_paths)]
  missing_paths <- output_paths[!file.exists(output_paths)]
  
  if (length(missing_links) == 0) {
    message("All files already exist. Nothing to download.")
    return(invisible(NULL))
  }
  
  message(glue("Downloading {length(missing_links)} files in parallel..."))
  
  # Perform parallel download using curl::multi_download
  results <- curl::multi_download(
    urls = missing_links,
    destfiles = missing_paths
  )
  
  # Check for errors
  failed <- results[results$status_code != 200, ]
  if (nrow(failed) > 0) {
    warning(glue("{nrow(failed)} files failed to download. Check the logs."))
    print(failed)
  } else {
    message("All files downloaded successfully.")
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
    rowwise() |> 
    mutate(biomass = sum(c_across(starts_with("x_")), na.rm = TRUE)) |> 
    ungroup() |> 
    # Remove all x_i columns
    select(-starts_with("x_"))
    
  
  return(z)
}

