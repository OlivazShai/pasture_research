recovery_counterfactual <- function(
    int_mod, 
    ext_mod, 
    int_data, 
    ext_cf, 
    min_high_share,
    beta = 0.9,
    h_hat = predict(int_mod, newdata = int_data)
) {
  
  #################### Intensive Margin #################
  
  # New data with recovery in high share
  int_cf_recovery <- int_data |> 
    mutate(
      high_share = if_else(
        # if high_share is less than the minimum
        high_share < min_high_share, 
        # becomes min_high_share
        min_high_share,
        # else, keeps as usual
        high_share)
    )
  
  # Append to data
  int_cf_recovery_clean <- int_data |>
    select(
      code_amc,
      year
    ) |> 
    mutate(
      # Predicted baseline from model coefficients
      h_hat = h_hat,
      
      # Predicted from counterfactual recovery
      h_recovery = predict(int_mod, newdata = int_cf_recovery)
    )
  
  ################## Extensive Margin #################
  
  # Predicted with int margin
  ext_cf_predicted <- ext_cf |> 
    left_join(
      int_cf_recovery_clean,
      by = c("code_amc", "year")
    ) |> 
    # calculates predicted x2
    mutate(
      x2 = case_when(
        # census year
        year %in% c(2006,2017) ~ 0.45*(h_hat)**2,
        
        # next year
        year %in% c(2007,2018) ~ 0.45*(h)**2,
        .default = NA)
    )
  
  # Potential = disconsiders the effect on x2
  ext_cf_recovery_potential <- ext_cf |> 
    # increase high_share in t and t+1, but not in t+2
    mutate(
      high_share = if_else(
        # if high_share is less than the minimum
        high_share < min_high_share, 
        # becomes min_high_share
        min_high_share,
        # else, keeps as usual
        high_share
      )
    ) |> 
    # recalculate time difference
    mutate(
      .by = "code_amc",
      
      high_share_tdif = case_when(
        # census year
        year %in% c(2006,2017) ~ tdif(high_share),
        
        # next year
        year %in% c(2007,2018) ~ beta * high_share_2 - high_share,
        .default = NA),
      
      .after = high_share_tdif
    )|> 
    # join with intensive margin data
    left_join(
      int_cf_recovery_clean,
      by = c("code_amc", "year")
    ) |> 
    # calculates predicted x2
    mutate(
      x2 = case_when(
        # census year
        year %in% c(2006,2017) ~ 0.45*(h_hat)**2,
        
        # next year
        year %in% c(2007,2018) ~ 0.45*(h)**2,
        .default = NA)
    )
  
  
  # Effective = takes into account the increase in x2 from the intensive margin
  ext_cf_recovery_effective <- ext_cf_recovery_potential |>  
    # calculates predicted x2
    mutate(
      x2 = case_when(
        # census year
        year %in% c(2006,2017) ~ 0.45*(h_recovery)**2,
        
        # next year
        year %in% c(2007,2018) ~ 0.45*(h)**2,
        .default = NA)
    )
  
  # Append to data
  ext_cf_recovery_clean <- ext_cf |>
    select(
      code_amc, year, rho_1, rho_2, rho_3, natural, biomass_density
    ) |> 
    mutate(
      # predicted from extensive margin
      y_e = predict(ext_mod, newdata = ext_cf),
      # predicted
      y_hat = predict(ext_mod, newdata = ext_cf_predicted),
      # maximum potential
      y_potential = predict(ext_mod, newdata = ext_cf_recovery_potential),
      # actual effect
      y_effective = predict(ext_mod, newdata = ext_cf_recovery_effective)
    )
  
  ########## calculate new probabilities ###########
  
  logit = \(x) exp(x)/(1+exp(x))
  
  
  
  dynamic <- ext_cf_recovery_clean |> 
    # year t+1
    mutate(
      across(
        .cols = c(y_e, y_hat, y_effective, y_potential),
        .fns = ~ case_when(
          # census years
          year %in% c(2006,2017) ~ 
            logit(.x - 0.9*gamma + 0.9*log(
              # new rho_2
              logit( lead(.x)  - 0.9*gamma + 0.9*log(rho_3) ) )
            ),
          
          # next year
          year %in% c(2007,2018) ~ logit(.x - 0.9*gamma + 0.9*log(rho_2)),
          
          .default = NA
        ),
        .names = "rho_{.col}"
      )
    )
  
  static <- ext_cf_recovery_clean |> 
    # year t+1
    mutate(
      across(
        .cols = c(y_e, y_hat, y_effective, y_potential),
        .fns = ~ logit(.x - 0.9*gamma + 0.9*log(rho_2)),
        .names = "rho_{.col}"
      )
    )
  
  
  result_dynamic <- dynamic |>
    filter(
      year %in% c(2006, 2017)
    ) |> 
    summarise(
      # In converted area
      real = sum(rho_1*natural),
      e = sum(rho_y_e*natural),
      hat = sum(rho_y_hat*natural),
      effective = sum(rho_y_effective*natural),
      potential = sum(rho_y_potential*natural),
      
      # In CO2 emissions
      hat_co2 = sum(rho_y_hat*natural*biomass_density)/ 10**9,
      effective_co2 = sum(rho_y_effective*natural*biomass_density)/ 10**9,
      potential_co2 = sum(rho_y_potential*natural*biomass_density)/ 10**9
    )
  
  result_static <- static |>
    filter(
      year %in% c(2006, 2017)
    ) |> 
    summarise(
      # In converted area
      real = sum(rho_1*natural),
      e = sum(rho_y_e*natural),
      hat = sum(rho_y_hat*natural),
      effective = sum(rho_y_effective*natural),
      potential = sum(rho_y_potential*natural),
      
      # In CO2 emissions measured in GIGATON (E+9)
      hat_co2 = sum(rho_y_hat*natural*biomass_density) / 10**9,
      effective_co2 = sum(rho_y_effective*natural*biomass_density) / 10**9,
      potential_co2 = sum(rho_y_potential*natural*biomass_density) / 10**9
    )
  
  result <- bind_rows(
    list(
      dynamic = result_dynamic,
      static = result_static
    ),
    .id = "type"
  )
  
  return(result)
}
