# Load the model from model_building.r
if (!require("leaps", quietly = TRUE)) {
  install.packages("leaps")
  library(leaps)
}

# setwd(dirname(sys.frame(1)$ofile))  # Commented out - doesn't work with Rscript
source("model_building.r")
source("helper.r")

# Access the best model directly (it's a global variable)
print(best_model)

# Parameter festlegung
n <- 100 #Stichprobenumfang
p <- 64 #Anzahl moeglicher Praediktoren
nsim <- 5000 #Anzahl simulationen
tmax <- numeric(nsim) #speichern des maximums der t-statistiken
sigma <- 1 # Fehlerstandardabweichung
nsteps <- 10 #Anzahl an vorwaertzselektionsschritten 

X <- matrix(rnorm(n * p), n, p)

# Original design matrix (from the model)
X_orig <- read.csv2("hainich.csv", header = TRUE, stringsAsFactors = FALSE)
X_orig <- X_orig[, -1]              # remove soil resp since it be target value

# Convert entire data frame to numeric matrix (ignoring column names)
X_orig <- as.matrix(sapply(X_orig, as.numeric))

max_positions <- numeric(nsim)

# Sample n rows (with replacement) from X_orig
set.seed(123)
X <- X_orig[sample(1:nrow(X_orig), n, replace = TRUE), ]

# Create new matrix: each column followed by its square
new_X <- do.call(cbind, lapply(1:ncol(X), function(i) cbind(X[,i], X[,i]^2)))

# Optional: add column names
colnames(new_X) <- as.vector(t(cbind(paste0("V", 1:ncol(X)), 
                                       paste0("V", 1:ncol(X), "_sq"))))

beta <- extract_coefficients(best_model)

df <- n - 2 #kompensierung fuer 2 praediktoren in best model
tcrit <- qt(0.975, df) #Kritischer wert

tmax_mat <- matrix(NA, nsim, nsteps) #speicher der tmax fuer jeden schritt der selektion
selection_mat <- matrix(NA, nsim, nsteps)


for(sim in 1:nsim){
  eps <- rnorm(n, 0, sigma)
  y <- new_X %*% beta + eps

  included <- c()

  for(step in 1:nsteps){
    remaining <- setdiff(1:p, included)
    tvals <- numeric(length(remaining))

    for(i in seq_along(remaining)){
      j <- remaining[i]

      if(length(included) == 0){
        model <- lm(y ~ new_X[, j])

      } else{
        model <- lm(y ~ new_X[, included] + new_X[, j])
      }

      coefs <- summary(model)$coefficients

      # safe extraction
      if(nrow(coefs) > 1){
        tvals[i] <- coefs[nrow(coefs), "t value"]
      } else {
        tvals[i] <- NA
      }
    }
    
    #stores max t val for this step
    tmax_mat[sim, step] <- max(abs(tvals), na.rm = TRUE)

    # check if any t exceeds critical value
    if(max(abs(tvals), na.rm=TRUE) > tcrit){
      best_j <- remaining[which.max(abs(tvals))]
      included <- c(included, best_j)
      selection_mat[sim, step] <- best_j
    } else {
      # no variable included for this step
      selection_mat[sim, step] <- NA
    }
  }
}
included_vars <- c()


# After all simulations and forward selection loops are complete:

for(step in 1:nsteps){
  
  # empirical error at this step
  emp_error <- mean(tmax_mat[, step] > tcrit, na.rm=TRUE)
  
  # most frequently included variable at this step (returns position number)
  # First, get the table of selections for this step
  step_table <- table(selection_mat[, step])
  
  # Check if table is empty or has no valid entries (all NA)
  if(length(step_table) == 0 || all(is.na(names(step_table)))){
    most_selected <- "none"
  } else {
    # Get the most frequently included variable position
    sorted_table <- sort(step_table, decreasing=TRUE)
    most_selected_pos <- as.numeric(names(sorted_table[1]))
    
    # Check if the position is valid and within bounds of all_predictors
    if(is.na(most_selected_pos) || most_selected_pos < 1 || most_selected_pos > length(all_predictors)){
      most_selected <- "none"
    } else {
      # Convert position to variable name from all_predictors in helper.r
      most_selected <- all_predictors[most_selected_pos]
    }
  }

  
  cat("Step", step,
      "Empirical error:", round(emp_error,3), "\n",
      "Most frequently included variable:", most_selected, "\n")
}

# ============================================================================
# Plotting: Visualize tmax evolution over simulation steps
# ============================================================================

# Create a data frame for plotting (long format)
tmax_df <- data.frame(
  simulation = rep(1:nsim, each = nsteps),
  step = rep(1:nsteps, times = nsim),
  tmax = as.vector(tmax_mat)
)

# Plot 5-14: Individual distribution plots for each time step
for(step in 1:nsteps){
  png_filename <- paste0("tmax_distribution_step", step, ".png")
  png(png_filename, width = 800, height = 600)
  
  # Get tmax values for this specific step
  tmax_values <- tmax_mat[, step]
  
  # Create histogram with density overlay
  hist(tmax_values, breaks = 30, freq = FALSE,
       main = paste("Distribution of tmax values at selection step", step),
       xlab = "tmax (maximum |t-statistic|)",
       ylab = "Density",
       col = "lightblue", border = "darkblue")
  
  # Add density curve
  lines(density(tmax_values, na.rm = TRUE), col = "blue", lwd = 2)
  
  # Add vertical line for critical value
  abline(v = tcrit, col = "red", lty = 2, lwd = 2)
  
  # Add legend
  legend("topright",
         legend = c(paste("Critical value (tcrit =", round(tcrit, 2), ")"),
                    paste("Mean tmax =", round(mean(tmax_values, na.rm = TRUE), 2)),
                    paste("Median tmax =", round(median(tmax_values, na.rm = TRUE), 2))),
         col = c("red", "blue", "blue"),
         lty = c(2, 1, 1),
         lwd = c(2, 2, 2))
  
  dev.off()
}
