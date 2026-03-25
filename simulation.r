# Load the model from model_building.r
if (!require("leaps", quietly = TRUE)) {
  install.packages("leaps")
  library(leaps)
}

setwd(dirname(sys.frame(1)$ofile))
source("model_building.r")
source("helper.r")

# Access the best model directly (it's a global variable)
print(best_model)

# Parameter festlegung
n <- 100 #Stichprobenumfang
p <- 64 #DOUBLE CHECK Anzahl moeglicher Praediktoren DOUBLE CHECK
nsim <- 500 #Anzahl simulationen
tmax <- numeric(nsim) #speichern des maximums der t-statistiken
sigma <- 1 # Fehlerstandardabweichung
nsteps <- 10 #Anzahl an vorwaertzselektionsschritten 

X <- matrix(rnorm(n * p), n, p)

# Original design matrix (from the model)
X_orig <- read.csv2("hainich.csv", header = TRUE, stringsAsFactors = FALSE)
X_orig <- X_orig[, -1]              # remove soil resp since it be target value

# Convert entire data frame to numeric matrix (ignoring column names)
X_orig <- as.matrix(sapply(X_orig, as.numeric))

#print(X_orig)

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

df <- n - 2
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

    #LIVETICKER DO NOT LEAVE ON FOR SUBMISSION most frequently included variable at this step (across all sims so far)
    #most_selected <- as.numeric(names(sort(table(selection_mat[,step]), decreasing=TRUE)[1]))
    #cat("Step", step,
    #  "Most frequently included variable so far:", most_selected,
    #  "\n")
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

# Plot 1: Boxplot of tmax values at each step
png("tmax_boxplot_by_step.png", width = 800, height = 600)
boxplot(tmax ~ step, data = tmax_df,
        main = "Distribution of tmax values at each selection step",
        xlab = "Selection Step",
        ylab = "tmax (maximum |t-statistic|)",
        col = rainbow(nsteps))
abline(h = tcrit, col = "red", lty = 2, lwd = 2)
legend("topright", legend = paste("Critical value (tcrit =", round(tcrit, 2), ")"),
       col = "red", lty = 2, lwd = 2)
dev.off()

# Plot 2: Line plot showing mean tmax with confidence intervals at each step
tmax_summary <- aggregate(tmax ~ step, data = tmax_df,
                          FUN = function(x) c(mean = mean(x),
                                              sd = sd(x),
                                              q025 = quantile(x, 0.025),
                                              q975 = quantile(x, 0.975)))
tmax_summary <- data.frame(
  step = tmax_summary$step,
  mean = tmax_summary$tmax[, 1],
  sd = tmax_summary$tmax[, 2],
  q025 = tmax_summary$tmax[, 3],
  q975 = tmax_summary$tmax[, 4]
)

png("tmax_mean_trajectory.png", width = 800, height = 600)
plot(tmax_summary$step, tmax_summary$mean, type = "b", pch = 19,
     main = "Mean tmax trajectory across simulation steps",
     xlab = "Selection Step",
     ylab = "tmax (maximum |t-statistic|)",
     ylim = range(c(tmax_summary$q025, tmax_summary$q975, tcrit)),
     col = "blue", lwd = 2)
arrows(tmax_summary$step, tmax_summary$q025,
       tmax_summary$step, tmax_summary$q975,
       code = 3, angle = 90, length = 0.1, col = "blue")
abline(h = tcrit, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Mean tmax", "95% CI", "Critical value"),
       col = c("blue", "blue", "red"), lty = c(1, 3, 2), lwd = c(2, 1, 2),
       pch = c(19, NA, NA))
dev.off()

# Plot 3: Individual trajectories (sample of simulations)
png("tmax_individual_trajectories.png", width = 800, height = 600)
sample_sims <- sample(1:nsim, min(50, nsim))
matplot(t(tmax_mat[sample_sims, ]), type = "l", lty = 1,
        main = "tmax trajectories for selected simulations",
        xlab = "Selection Step",
        ylab = "tmax (maximum |t-statistic|)",
        col = rainbow(length(sample_sims), alpha = 0.7))
abline(h = tcrit, col = "black", lty = 2, lwd = 2)
legend("topright", legend = paste("Critical value (tcrit =", round(tcrit, 2), ")"),
       col = "black", lty = 2, lwd = 2)
dev.off()

# Plot 4: Heatmap of tmax values across all simulations and steps
png("tmax_heatmap.png", width = 800, height = 600)
image(1:nsteps, 1:nsim, t(tmax_mat[, 1:nsteps]),
      main = "Heatmap of tmax values across simulations and steps",
      xlab = "Selection Step",
      ylab = "Simulation",
      col = heat.colors(256))
abline(v = which(1:nsteps == 1:nsteps), col = "white", lwd = 0.5)
contour(1:nsteps, 1:nsim, t(tmax_mat[, 1:nsteps]),
        levels = tcrit, add = TRUE, col = "black", lwd = 2)
dev.off()

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

cat("\nPlots saved:\n")
cat("  - tmax_boxplot_by_step.png: Boxplot showing tmax distribution at each step\n")
cat("  - tmax_mean_trajectory.png: Mean tmax with 95% CI across steps\n")
cat("  - tmax_individual_trajectories.png: Individual simulation trajectories\n")
cat("  - tmax_heatmap.png: Heatmap of all tmax values\n")