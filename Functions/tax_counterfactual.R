y_tax <- function(
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
  y_tax <- y_biomass + fixed_effects
  
  # Return the predictions
  return(y_tax)
}


tax_counterfactual <- function(
    int_mod, 
    ext_mod,
    int_data,
    ext_cf,
    tax
    ) {
  
  # Get baseline deforestation estimate
  hats = recovery_counterfactual(
    int_mod = int_mod, 
    ext_mod = ext_mod, 
    int_data = int_data, 
    ext_cf = ext_cf, 
    min_high_share = 0,
  ) |>
    filter(type == "static") |>
    select(hat, hat_co2)
  
  ############# Calculate probabilities and aggregate ############
  
  result <- ext_cf |>
    mutate(
      y_tax = y_tax(
        int_mod = int_mod, 
        ext_mod = ext_mod,
        ext_cf = ext_cf, 
        tax = tax),
    ) |> 
    select(
      code_amc, year, rho_1, rho_2, y_tax, natural, biomass_density
    ) |> 
    mutate(
      across(
        .cols = c(y_tax),
        .fns = ~ logit(.x - 0.9*gamma + 0.9*log(rho_2)),
        .names = "rho_{.col}")
    ) |> 
    filter(
      year %in% c(2006, 2017)
    ) |> 
    summarise(
      # baseline
      hat = hats$hat,
      hat_co2 = hats$hat_co2,
      # counterfactual
      tax = sum(rho_y_tax*natural),
      tax_co2 = sum(rho_y_tax*natural*biomass_density) / 10**9
    )
  
  return(result)
}
