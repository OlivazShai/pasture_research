calculate_structural_params <- function(int_model, ext_model) {
  # Extract coefficients and fixed effects from intensive margin
  int_coefs <- coef(int_model)
  int_fixed_effects <- fixef(int_model)$main_biome
  
  # Calculate intensive margin parameters
  alpha_p_delta <- - beta * int_coefs["fit_local_price"]
  phi <- (int_coefs["fit_local_price_2"] / alpha_p_delta) - 1
  
  # Extract coefficients and fixed effects from extensive margin
  ext_coefs <- coef(ext_model)
  ext_fixed_effects <- fixef(ext_model)$main_biome
  
  # Calculate extensive margin parameters
  delta <- ext_coefs["x2"]
  alpha_p <- alpha_p_delta * delta
  alpha_b <- - ext_coefs["biomass_density"]
  gamma_pasture_e <- ext_coefs["fit_high_share_tdif"]
  gamma_year_e <- ext_coefs["year_tdif"]
  
  # Back to intensive margin
  gamma_pasture <- - int_coefs["fit_high_share"] * delta
  gamma_year <- - int_coefs["year"] * delta
  
  # fixed effects
  biomes = c(
    "amazonia", "caatinga", "cerrado", "mata atlantica", "pampa", "pantanal"
  )
  
  gamma_biome <- map_vec(int_fixed_effects, function(fe) - fe * delta) # multiply int mar FE by delta
  names(gamma_biome) <- paste("$\\gamma_g: g =$", biomes)
  
  gamma_biome_e <- map_vec(ext_fixed_effects, function(fe) fe / (beta - 1)) # divide by beta - 1
  names(gamma_biome_e) <- paste("$\\gamma_g^e: g =$", biomes)
  
  # Combine parameters into a data frame
  result <- data.frame(
    Parameter = c(
      "$\\phi$", "$\\delta$", "$\\alpha_p$", "$\\alpha_b$",
      "$\\gamma_{pasture}$", 
      "$\\gamma_t$",
      names(gamma_biome), 
      "$\\gamma_{pasture}^e$",
      "$\\gamma_t^e$",
      names(gamma_biome_e)
    ),
    
    Value = c(
      phi, delta, alpha_p, alpha_b,
      gamma_pasture, gamma_year,
      unname(gamma_biome), 
      gamma_pasture_e,
      gamma_year_e,
      unname(gamma_biome_e) 
    ),
    
    # Add column with parameters divided by alpha_p
    Scaled = c(
      NA,
      NA,
      NA,
      map_vec(c(alpha_b,
                gamma_pasture, gamma_year, unname(gamma_biome),
                gamma_pasture_e, gamma_year_e, unname(gamma_biome_e)),
              \(x) x / alpha_p)
    )
  )
  
  return(result)
}