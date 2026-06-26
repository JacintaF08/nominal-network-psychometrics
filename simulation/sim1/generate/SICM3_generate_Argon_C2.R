## =============================================================================
## Simulation 1: SICM Data Generation (3 Misconceptions, Condition 2)
## =============================================================================
##
## Description:
##   Generates one simulated item response dataset for Simulation 1,
##   Condition 2 using the SICM model (Bradshaw & Templin, 2014).
##   Designed for HPC array job submission (one job per replication).
##
## Usage (HPC array job):
##   Rscript SICM3_generate_Argon_C2.R <replication_number>
##   where <replication_number> is an integer from 1 to 1000.
##
## Condition 2 — High correlation (r = 0.8):
##   Misconception profile probabilities are derived from a multivariate
##   normal distribution dichotomized to produce a tetrachoric correlation
##   of 0.8 between misconceptions 1 and 2. All other pairwise correlations
##   are fixed at zero. Thresholds are drawn from the range observed in
##   Condition 1: U(0.335, 1.17).
##
## Output:
##   Data/ResponseMatrix3_C2_{c}.RData
##   Contains: itemResponse, latents, itemParameters, option_qmatrix,
##             corr3_2, misconceptionProbs, cutpoints
##
## Simulation design:
##   - 1,000 examinees, 30 items, 4 options per item (A = correct)
##   - 3 misconceptions: B = misconception 1, C = misconception 2,
##                       D = misconception 3
##   - Misconception 1 and 2 correlated at r = 0.8 (tetrachoric)
##   - Item intercepts ~ U(-2, 1.5); ability effects ~ U(0, 4);
##     misconception effects ~ U(0, 4)
##
## Note:
##   No random seed is set in this script. The 1,000 generated datasets
##   are provided in the repository's data/simulation/sim1/condition2/ folder
##   and represent the exact data used in the paper.
##
## Dependencies: psych, MASS
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)
c <- as.numeric(args[1])

library(psych)
library(MASS)

# -----------------------------------------------------------------------------
# Helper functions: decimal <-> binary conversion
# -----------------------------------------------------------------------------

dec2bin <- function(decimal_number, nattributes, basevector) {
  dec <- decimal_number
  profile <- matrix(NA, nrow = 1, ncol = nattributes)
  for (i in nattributes:1) {
    profile[1, i] <- dec %% basevector[i]
    dec <- (dec - dec %% basevector[i]) / basevector[i]
  }
  return(profile)
}

bin2dec <- function(binary_vector, nattributes, basevector) {
  dec <- 0
  for (i in nattributes:1) {
    dec <- dec + binary_vector[i] * (basevector[i]^(nattributes - i))
  }
  return(dec)
}

# -----------------------------------------------------------------------------
# Step 1: Generate misconception profile probabilities via tetrachoric model
# -----------------------------------------------------------------------------

nmisconceptions <- 3
nMisconceptionPatterns <- 2^3
nObsTC <- 10000000  # large N for stable probability estimates

# All possible misconception profiles (binary patterns)
misconceptionProfiles <- NULL
for (i in 1:nMisconceptionPatterns) {
  misconceptionProfiles <- rbind(
    misconceptionProfiles,
    dec2bin(i - 1, nmisconceptions, rep(2, nmisconceptions))
  )
}

# Tetrachoric correlation matrix: misconceptions 1 and 2 correlated at 0.8
# (Condition 2 = High Correlation)
corr3_2 <- matrix(0, nrow = 3, ncol = 3)
diag(corr3_2) <- 1
corr3_2[1, 2] <- 0.8
corr3_2[2, 1] <- 0.8

# Cutpoints drawn from range observed in Condition 1 (Bradshaw & Templin, 2014)
cutpoints <- runif(n = 3, min = 0.335, max = 1.17)

# Simulate continuous latent data and dichotomize
continuousData <- mvrnorm(n = nObsTC, mu = rep(0, nmisconceptions), Sigma = corr3_2)
dichotomousData <- continuousData
for (mis in 1:ncol(continuousData)) {
  dichotomousData[, mis] <- ifelse(continuousData[, mis] > cutpoints[mis], 1, 0)
}

# Convert binary profiles to class index
misconceptionClass <- rep(0, nObsTC)
for (i in 1:nObsTC) {
  misconceptionClass[i] <- bin2dec(dichotomousData[i, ], nmisconceptions, rep(2, nmisconceptions)) + 1
}

# Proportion of examinees in each misconception pattern
misconceptionProbs <- rep(0, nMisconceptionPatterns)
for (i in 1:nMisconceptionPatterns) {
  misconceptionProbs[i] <- sum(misconceptionClass == i) / nObsTC
}

# -----------------------------------------------------------------------------
# Step 2: Generate item response data for 1,000 examinees on 30 items
# -----------------------------------------------------------------------------

nObs   <- 1000
nItems <- 30

# Continuous ability (theta)
theta <- rnorm(n = nObs)

# Sample a misconception pattern for each examinee
sampmisconceptions <- sample(
  1:nMisconceptionPatterns,
  size    = nObs,
  replace = TRUE,
  prob    = misconceptionProbs
)
latents <- cbind(theta, misconceptionProfiles[sampmisconceptions, ])

# -----------------------------------------------------------------------------
# Step 3: Build Q-matrix
# Each item has 4 options: A (correct), B (mis 1), C (mis 2), D (mis 3)
# -----------------------------------------------------------------------------

num_distractors <- rep(4, nItems)

row_names <- c()
for (i in 1:length(num_distractors)) {
  row_names <- c(row_names, paste(i, letters[1:num_distractors[i]], sep = ""))
}

option_qmatrix <- matrix(0, nrow = length(row_names), ncol = nmisconceptions)
rownames(option_qmatrix) <- row_names
colnames(option_qmatrix) <- paste("d", 1:nmisconceptions, sep = "")

option_qmatrix[grep("b", rownames(option_qmatrix)), 1] <- 1  # B = misconception 1
option_qmatrix[grep("c", rownames(option_qmatrix)), 2] <- 1  # C = misconception 2
option_qmatrix[grep("d", rownames(option_qmatrix)), 3] <- 1  # D = misconception 3

# -----------------------------------------------------------------------------
# Step 4: Generate item parameters
# Option A (correct): intercept ~ U(-2, 1.5), ability effect ~ U(0, 4)
# Options B/C/D (incorrect): intercept ~ U(-2, 1.5), misconception effect ~ U(0, 4)
# -----------------------------------------------------------------------------

itemParameters <- list()
for (item in 1:nItems) {
  itemParameters[[item]] <- list()
  noptions <- num_distractors[item]
  for (option in 1:noptions) {
    if (option == 1) {
      # Correct option: has ability effect, no misconception effect
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),  # intercept
        runif(n = 1, min = 0, max = 4),      # ability main effect
        0                                     # misconception main effect
      )
    } else {
      # Incorrect option: has misconception effect, no ability effect
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),  # intercept
        0,                                    # ability main effect
        runif(n = 1, min = 0, max = 4)       # misconception main effect
      )
    }
  }
}

# -----------------------------------------------------------------------------
# Step 5: Simulate item responses using the SICM model
# -----------------------------------------------------------------------------

itemResponse <- data.frame()
for (person in 1:nObs) {
  option_num <- 1
  for (item in 1:nItems) {
    logItemModels <- numeric(num_distractors[item])
    for (option in 1:num_distractors[item]) {
      logItemModels[option] <-
        itemParameters[[item]]$optionParameters[[option]][1] +
        itemParameters[[item]]$optionParameters[[option]][2] * theta[person] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 1] * latents[person, 2] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 2] * latents[person, 3] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 3] * latents[person, 4]
      option_num <- option_num + 1
    }
    itemOptionProbs <- exp(logItemModels) / sum(exp(logItemModels))
    itemResponse[person, item] <- sample(1:num_distractors[item], 1, prob = itemOptionProbs)
  }
}

# -----------------------------------------------------------------------------
# Step 6: Save output
# -----------------------------------------------------------------------------

if (!dir.exists("Data")) dir.create("Data")

save(itemResponse, latents, itemParameters, option_qmatrix,
     corr3_2, misconceptionProbs, cutpoints,
     file = paste0("Data/ResponseMatrix3_C2_{", c, "}.RData"))

cat("Replication", c, "complete.\n")
