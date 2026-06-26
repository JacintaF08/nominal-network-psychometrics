## =============================================================================
## Simulation 1: SICM Data Generation (3 Misconceptions, Condition 3)
## =============================================================================
##
## Description:
##   Generates one simulated item response dataset for Simulation 1,
##   Condition 3 using the SICM model (Bradshaw & Templin, 2014).
##
## Condition 3 — Low correlation (r = 0.2):
##   Misconception profile probabilities are derived from a multivariate
##   normal distribution dichotomized to produce a tetrachoric correlation
##   of 0.2 between misconceptions 1 and 2. All other pairwise correlations
##   are fixed at zero. Thresholds are drawn from the range observed in
##   Condition 1: U(0.335, 1.17).
##
## Usage (HPC array job):
##   Rscript SICM3_generate_Argon_C3.R <replication_number>
##
## Output:
##   Data/ResponseMatrix3_C3_{c}.RData
##   Contains: itemResponse, latents, itemParameters, option_qmatrix,
##             corr3_3 (target correlation matrix), misconceptionProbs, cutpoints
##
## Note:
##   No random seed was set during original data generation. The 1,000
##   generated datasets are provided in data/simulation/sim1/condition3/.
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

nmisconceptions       <- 3
nMisconceptionPatterns <- 2^3
nObsTC                <- 10000000

misconceptionProfiles <- NULL
for (i in 1:nMisconceptionPatterns) {
  misconceptionProfiles <- rbind(
    misconceptionProfiles,
    dec2bin(i - 1, nmisconceptions, rep(2, nmisconceptions))
  )
}

# Condition 3: low correlation (r = 0.2) between misconceptions 1 and 2
corr3_3 <- matrix(0, nrow = 3, ncol = 3)
diag(corr3_3) <- 1
corr3_3[1, 2] <- 0.2
corr3_3[2, 1] <- 0.2

# Cutpoints drawn from range observed in Condition 1
cutpoints <- runif(n = 3, min = 0.335, max = 1.17)

continuousData  <- mvrnorm(n = nObsTC, mu = rep(0, nmisconceptions), Sigma = corr3_3)
dichotomousData <- continuousData
for (mis in 1:ncol(continuousData)) {
  dichotomousData[, mis] <- ifelse(continuousData[, mis] > cutpoints[mis], 1, 0)
}

misconceptionClass <- rep(0, nObsTC)
for (i in 1:nObsTC) {
  misconceptionClass[i] <- bin2dec(dichotomousData[i, ], nmisconceptions, rep(2, nmisconceptions)) + 1
}

misconceptionProbs <- rep(0, nMisconceptionPatterns)
for (i in 1:nMisconceptionPatterns) {
  misconceptionProbs[i] <- sum(misconceptionClass == i) / nObsTC
}

# -----------------------------------------------------------------------------
# Step 2: Generate item response data for 1,000 examinees on 30 items
# -----------------------------------------------------------------------------

nObs   <- 1000
nItems <- 30

theta <- rnorm(n = nObs)

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

option_qmatrix[grep("b", rownames(option_qmatrix)), 1] <- 1
option_qmatrix[grep("c", rownames(option_qmatrix)), 2] <- 1
option_qmatrix[grep("d", rownames(option_qmatrix)), 3] <- 1

# -----------------------------------------------------------------------------
# Step 4: Generate item parameters
# -----------------------------------------------------------------------------

itemParameters <- list()
for (item in 1:nItems) {
  itemParameters[[item]] <- list()
  noptions <- num_distractors[item]
  for (option in 1:noptions) {
    if (option == 1) {
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),
        runif(n = 1, min = 0, max = 4),
        0
      )
    } else {
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),
        0,
        runif(n = 1, min = 0, max = 4)
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
     corr3_3, misconceptionProbs, cutpoints,
     file = paste0("Data/ResponseMatrix3_C3_{", c, "}.RData"))

cat("Replication", c, "complete.\n")
