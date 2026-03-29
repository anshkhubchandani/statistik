
# Define the complete set of all possible predictors (linear + quadratic)
all_predictors <- c(
  #Intercept
  #"(Intercept)",
  # Temperature
  "temp.0", "I(temp.0^2)",
  "temp.5", "I(temp.5^2)",
  "temp.10", "I(temp.10^2)",
  "temp.15", "I(temp.15^2)",
  # Moisture
  "smoi", "I(smoi^2)",
  "lmoi", "I(lmoi^2)",
  # Litter
  "litter.d", "I(litter.d^2)",
  "litter.c", "I(litter.c^2)",
  "litter.n", "I(litter.n^2)",
  # pH
  "ph.litter", "I(ph.litter^2)",
  "ph.soil0", "I(ph.soil0^2)",
  "ph.soil5", "I(ph.soil5^2)",
  # Roots
  "rootdw0", "I(rootdw0^2)",
  "rootdw5", "I(rootdw5^2)",
  "rootn0", "I(rootn0^2)",
  "rootc0", "I(rootc0^2)",
  "rootn5", "I(rootn5^2)",
  "rootc5", "I(rootc5^2)",
  # Soil nutrients
  "soiln0", "I(soiln0^2)",
  "soilc0", "I(soilc0^2)",
  "soiln5", "I(soiln5^2)",
  "soilc5", "I(soilc5^2)",
  # Litter nutrients
  "litno3", "I(litno3^2)",
  "litnh4", "I(litnh4^2)",
  "litdoc", "I(litdoc^2)",
  # Snow/soil solution
  "sno30", "I(sno30^2)",
  "snh40", "I(snh40^2)",
  "sdoc0", "I(sdoc0^2)",
  "sno35", "I(sno35^2)",
  "snh45", "I(snh45^2)",
  "sdoc5", "I(sdoc5^2)",
  # Distance
  "nn", "I(nn^2)"
)

# Function to extract coefficients and place them in a full predictor vector
extract_coefficients <- function(model, all_pred = all_predictors) {
  # Extract coefficients from the model
  coefs <- stats::coef(model)

  # Create a vector of zeros with names matching all predictors
  coef_vector <- setNames(
    rep(0, length(all_pred)),
    all_pred
  )
  
  # Fill in the coefficients at the correct positions 
  model_names <- names(coefs)
  #model_names <- model_names[model_names != "(Intercept)"]
  
  for (name in model_names) {
    if (name %in% all_pred) {
      coef_vector[name] <- coefs[name]
    } #else {
      #warning(paste("Predictor", name, "not found in all_predictors vector"))
    #}
  }
  
  return(coef_vector)
}

# Usage with regsubsets (model 6):
#coef_vec_6 <- extract_coefficients(stats::coef(fit, 6))

# Usage with lm object:
#coef_vec_lm <- extract_coefficients(best_model)  # best_model = lm(log(soil.res) ~ I(temp.15^2) + I(litter.d^2), data = hainich.data)

# Get coefficients for any model size from regsubsets
get_model_coefs <- function(regfit_obj, model_size, all_pred = all_predictors) {
  coefs <- stats::coef(regfit_obj, model_size)
  extract_coefficients(coefs, all_pred)
}

# Example: get coefficients for model with 6 predictors
#coef_vec_6 <- get_model_coefs(fit, 6)
