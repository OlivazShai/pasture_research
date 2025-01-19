tax_counterfactual <- function(
    int_mod, 
    ext_mod,
    ext_cf,
    tax) {
  
  # import structural parameter calculation
  source("../Functions/structural_parameters.R")
  
  # Extract original coefficients
  coefs <- coef(ext_mod)
  
  # calculate alpha_p
  alpha_p = calculate_structural_params(int_mod, ext_mod)$Value[3]
  
  # Replace the coefficient of biomass_density with new_alpha_b
  coefs["biomass_density"] <- - tax * alpha_p
  
  # Extract the design matrix for the NEW data
  design_matrix <- model.matrix(ext_mod, data = ext_cf)
  
  # Calculate the new predictions (excluding fixed effects)
  y_biomass <- as.vector(design_matrix %*% coefs)
  
  # Add the fixed effects back to the predictions
  fixed_effects <- predict(ext_mod, newdata = ext_cf, fixef = TRUE)$main_biome
  y_biomass <- y_biomass + fixed_effects
  
  # Return the predictions
  return(y_biomass)
}