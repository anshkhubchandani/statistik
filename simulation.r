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
  
  # most frequently included variable at this step
  most_selected <- as.numeric(names(sort(table(selection_mat[, step]), decreasing=TRUE)[1]))
  
  cat("Step", step,
      "Empirical error:", round(emp_error,3), "\n",
      "Most frequently included variable:", most_selected, "\n")
}