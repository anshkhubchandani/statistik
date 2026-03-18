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

X <- matrix(rnorm(n * p), n, p)
#X <- cbind(1,X)
#print(X)

# Original design matrix (from the model)
X_orig <- read.csv2("hainich.csv", header = TRUE, stringsAsFactors = FALSE)
X_orig <- X_orig[, -1]              # remove soil resp since it be target value

# Convert entire data frame to numeric matrix (ignoring column names)
X_orig <- as.matrix(sapply(X_orig, as.numeric))
#print(X_orig)

max_positions <- numeric(nsim)

print(new_X)
# Sample n rows (with replacement) from X_orig
set.seed(123)
X <- X_orig[sample(1:nrow(X_orig), n, replace = TRUE), ]

# Create new matrix: each column followed by its square
new_X <- do.call(cbind, lapply(1:ncol(X), function(i) cbind(X[,i], X[,i]^2)))

# Optional: add column names
colnames(new_X) <- as.vector(t(cbind(paste0("V", 1:ncol(X)), 
                                       paste0("V", 1:ncol(X), "_sq"))))

beta <- extract_coefficients(best_model)
#print(beta)

print(length(beta))
print(ncol(new_X))
#print(new_X)

new_X <- new_X[, -26]
beta <- beta[-26]

print(length(beta))
print(ncol(new_X))

for (i in 1:nsim) {
  
  # Fehler erzeugen
  eps <- rnorm(n, 0, sigma)
  
  # Pseudobeobachtungen
  y <- new_X %*% beta + eps
  
  # t-Werte speichern
  tvals <- numeric(p)
  
  for (j in 1:(p-1)) {
    
    model <- lm(y ~ new_X[, j])
    
    #berechnung t-statistik
    tvals[j] <- summary(model)$coefficients[2,3]
    
  }
  #print(tvals)
  #print(which.max(abs(tvals)))

  tmax[i] <- max(abs(tvals))
  max_positions[i] <- which.max(abs(tvals))
  
}
#print(tmax)
hist(tmax, probability = TRUE, breaks = 40,
     main = "Maximum der t-Statistiken",
     xlab = "max(|t|)")

df <- n - 2
curve(dt(x, df=df), add=TRUE, col="red", lwd=2)

tcrit <- qt(0.975, df) #Kritischer wert
#print(min(tmax))
#print(tmax < tcrit)
print(mean(tmax > tcrit)) #Empirische Fehlerwarscheinlichkeit
