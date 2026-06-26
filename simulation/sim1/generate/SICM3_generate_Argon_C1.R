## =============================================================================
## Simulation 1: SICM Data Generation (3 Misconceptions, Condition 1)
## =============================================================================
##
## Description:
##   Generates one simulated item response dataset for Simulation 1,
##   Condition 1 using the SICM model (Bradshaw & Templin, 2014).
##
## Condition 1 — Article correlation (r ≈ 0.5):
##   Misconception profile probabilities are taken directly from the empirical
##   distribution reported in Bradshaw & Templin (2014), Table 1. 
##   This is the baseline condition replicating the original article's structure.
##
##   Misconception pattern proportions (Bradshaw & Templin, 2014):
##     [000] = 0.434, [001] = 0.061, [010] = 0.123, [011] = 0.013
##     [100] = 0.146, [101] = 0.025, [110] = 0.176, [111] = 0.022
##
## Usage (HPC array job):
##   Rscript SICM3_generate_Argon_C1.R <replication_number>
##
## Output:
##   Data/ResponseMatrix3_C1_{c}.RData
##   Contains: itemResponse, latents, itemParameters, option_qmatrix,
##             corData (observed tetrachoric correlations), cutpoints
##
## Note:
##   No random seed was set during original data generation. The 1,000
##   generated datasets are provided in data/simulation/sim1/condition1/.
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
# Step 1: Set misconception profile probabilities from Bradshaw & Templin (2014)
# -----------------------------------------------------------------------------

nmisconceptions       <- 3
nMisconceptionPatterns <- 2^3

# Empirical proportions from Table 1 of Bradshaw & Templin (2014)
# Order: [000],[001],[010],[011],[100],[101],[110],[111]
misconceptionProbs <- c(0.434, 0.061, 0.123, 0.013, 0.146, 0.025, 0.176, 0.022)

# All possible misconception profiles
misconceptionProfiles <- NULL
for (i in 1:nMisconceptionPatterns) {
  misconceptionProfiles <- rbind(
    misconceptionProfiles,
    dec2bin(i - 1, nmisconceptions, rep(2, nmisconceptions))
  )
}

# Estimate observed tetrachoric correlations for record-keeping
# (correlation emerges from the profile proportions, not set explicitly)
nObsCheck <- 10000000
simClass  <- sample(1:nMisconceptionPatterns, size = nObsCheck,
                    replace = TRUE, prob = misconceptionProbs)
simData   <- misconceptionProfiles[simClass, ]
corData   <- tetrachoric(simData)  # saved to output for reference

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
# cutpoints = corData$tau (thresholds from tetrachoric, saved for reference)
# -----------------------------------------------------------------------------

if (!dir.exists("Data")) dir.create("Data")

cutpoints <- corData[["tau"]]
save(itemResponse, latents, itemParameters, option_qmatrix,
     corData, cutpoints,
     file = paste0("Data/ResponseMatrix3_C1_{", c, "}.RData"))

cat("Replication", c, "complete.\n")
