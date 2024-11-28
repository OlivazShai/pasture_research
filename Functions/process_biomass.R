library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(glue)

process_biomass <- function(api_links, zones) {
  # Check inputs
  if (!is.list(api_links) || !inherits(zones, "sf")) {
    stop("Ensure `api_links` is a named list of links, and `zones` is an sf object.")
  }
  
  # Step 1: Copy zones and convert to dataframe without geometry
  z <- zones |> 
    st_drop_geometry() |> 
    as_tibble()
  
  # Step 2: Initialize an empty list to store x_i columns
  x_columns <- list()
  
  # Step 3: Loop through the api_links
  for (i in seq_along(api_links)) {
    message(glue::glue("Processing raster {i}/{length(api_links)}: {names(api_links)[i]}"))
    
    # Load the ith raster
    message("  Starting terra::rast()...")
    r <- terra::rast(api_links[[i]])
    message("  Done with terra::rast().")
    
    # Perform exact_extract with "sum"
    message("  Starting exact_extract()...")
    x_i <- exactextractr::exact_extract(
      r, zones, fun = "sum"
    )
    message("  Done with exact_extract().")
    
    x_columns[[paste0("x_", i)]] <- x_i
    
    # Save the raster to a file with maximum compression
    output_path <- file.path(
      "../Biomass", 
      paste0(names(api_links)[i], ".tif")
    )
    
    message("  Starting to save the raster...")
    terra::writeRaster(r, filename = output_path, overwrite = TRUE, filetype = "GTiff")
    message("  Done saving the raster.")
    
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
