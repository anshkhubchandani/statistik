# laden des models von model_building.r
if (!require("leaps", quietly = TRUE)) {
  install.packages("leaps")
  library(leaps)
}


script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir)) {
  script_dir <- "statistik/abgabe_code"
}

#best_model <- lm(log(soil.res) ~ I(temp.15^2) + I(litter.d^2), data = hainich.data)
source("model_building.r")
source(file.path(script_dir, "helper.r"))

print(best_model)

# Parameter festlegung
n <- 100 #Stichprobenumfang
p <- 64 #Anzahl moeglicher Praediktoren
nsim <- 5000 #Anzahl simulationen
tmax <- numeric(nsim) #speichern des maximums der t-statistiken
sigma <- 1 # Fehlerstandardabweichung
nsteps <- 10 #Anzahl an vorwaertzselektionsschritten 

X <- matrix(rnorm(n * p), n, p)


X_orig <- read.csv2(file.path(script_dir, "..", "hainich.csv"), header = TRUE, stringsAsFactors = FALSE)
X_orig <- X_orig[, -1] # remove soil resp since it be target value

X_orig <- as.matrix(sapply(X_orig, as.numeric))

max_positions <- numeric(nsim)

set.seed(123)
X <- X_orig[sample(1:nrow(X_orig), n, replace = TRUE), ]

new_X <- do.call(cbind, lapply(1:ncol(X), function(i) cbind(X[,i], X[,i]^2)))

colnames(new_X) <- as.vector(t(cbind(paste0("V", 1:ncol(X)), 
                                       paste0("V", 1:ncol(X), "_sq"))))

beta <- extract_coefficients(best_model)

df <- n - 2 #kompensierung fuer 2 praediktoren in best model
tcrit <- qt(0.975, df) #Kritischer wert

tmax_mat <- matrix(NA, nsim, nsteps) #speicher der tmax fuer jeden schritt der selektion
selection_mat <- matrix(NA, nsim, nsteps) #speichert die aufgenommenen praediktoren 


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

      if(nrow(coefs) > 1){
        tvals[i] <- coefs[nrow(coefs), "t value"]
      } else {
        tvals[i] <- NA
      }
    }
    
    tmax_mat[sim, step] <- max(abs(tvals), na.rm = TRUE)

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

for(step in 1:nsteps){
  
  emp_error <- mean(tmax_mat[, step] > tcrit, na.rm=TRUE)
  
  step_table <- table(selection_mat[, step])
  
  if(length(step_table) == 0 || all(is.na(names(step_table)))){
    most_selected <- "none"
    second_most_selected <- "none"
    third_most_selected <- "none"
  } else {
    sorted_table <- sort(step_table, decreasing=TRUE)
    most_selected_pos <- as.numeric(names(sorted_table[1]))
    second_most_selected_pos <- as.numeric(names(sorted_table[2]))
    third_most_selected_pos <- as.numeric(names(sorted_table[3]))

    
    if(is.na(most_selected_pos) || most_selected_pos < 1 || most_selected_pos > length(all_predictors)){
      most_selected <- "none"
    } else {
      most_selected <- all_predictors[most_selected_pos]
    }
  }
      if(is.na(second_most_selected_pos) || second_most_selected_pos < 1 || second_most_selected_pos > length(all_predictors)){
      second_most_selected <- "none"
    } else {
      second_most_selected <- all_predictors[second_most_selected_pos]
    }
  
  if(is.na(third_most_selected_pos) || third_most_selected_pos < 1 || third_most_selected_pos > length(all_predictors)){
      third_most_selected <- "none"
    } else {
      third_most_selected <- all_predictors[third_most_selected_pos]
    }
  
  cat("Step", step,
      "Empirical error:", round(emp_error,3), "\n",
      "Most frequently included variable:", most_selected, "\n",
      "Second most frequently included variable:", second_most_selected, "\n",
      "Third most frequently included variable:", third_most_selected, "\n")
}

tmax_df <- data.frame(
  simulation = rep(1:nsim, each = nsteps),
  step = rep(1:nsteps, times = nsim),
  tmax = as.vector(tmax_mat)
)

for(step in 1:nsteps){
  png_filename <- paste0("tmax_distribution_step", step, ".png")
  png(png_filename, width = 800, height = 600)
  
  tmax_values <- tmax_mat[, step]
  
  hist(tmax_values, breaks = 30, freq = FALSE,
       main = paste("Distribution of tmax values at selection step", step),
       xlab = "tmax (maximum |t-statistic|)",
       ylab = "Density",
       col = "lightblue", border = "darkblue")
  
  lines(density(tmax_values, na.rm = TRUE), col = "blue", lwd = 2)
  
  abline(v = tcrit, col = "red", lty = 2, lwd = 2)
  
  legend("topright",
         legend = c(paste("Critical value (tcrit =", round(tcrit, 2), ")"),
                    paste("Mean tmax =", round(mean(tmax_values, na.rm = TRUE), 2)),
                    paste("Median tmax =", round(median(tmax_values, na.rm = TRUE), 2))),
         col = c("red", "blue", "blue"),
         lty = c(2, 1, 1),
         lwd = c(2, 2, 2))
  
  dev.off()
}
