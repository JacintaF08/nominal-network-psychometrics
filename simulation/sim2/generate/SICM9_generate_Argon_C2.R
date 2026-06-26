## =============================================================================
## Simulation 2: SICM Data Generation (9 Misconceptions, Condition 1)
## =============================================================================
##
## Description:
##   Generates one simulated item response dataset for Simulation 2,
##   Condition 2 using the SICM model (Bradshaw & Templin, 2014).
##
## Condition 2 — High correlation (within-set r = 0.8):
##   The 9 misconceptions are organized into 3 correlated sets, extending
##   the 3-misconception correlation structure from Bradshaw & Templin (2014).
##   Within each set, all pairwise tetrachoric correlations are set to 0.5.
##   Between sets, all correlations are fixed at 0.
##
##   Misconception sets:
##     Set 1: misconceptions 1, 4, 7  (correlated at r = 0.8)
##     Set 2: misconceptions 2, 5, 8  (correlated at r = 0.8)
##     Set 3: misconceptions 3, 6, 9  (correlated at r = 0.8)
##
## Assessment structure (30 items, 4 options each, A = correct):
##   Items  1-10: B = misconception 1, C = misconception 2, D = misconception 3
##   Items 11-20: B = misconception 4, C = misconception 5, D = misconception 6
##   Items 21-30: B = misconception 7, C = misconception 8, D = misconception 9
##
## Usage (HPC array job):
##   Rscript SICM9_generate_Argon_C2.R <replication_number>
##
## Output:
##   Data/ResponseMatrix9_C2_{c}.RData
##   Contains: itemResponse, latents, itemParameters, option_qmatrix,
##             corr9_2, cutpoints
##
## Note:
##   No random seed was set during original data generation. The 1,000
##   generated datasets are provided in data/simulation/sim2/condition2/.
##
## Dependencies: psych, MASS
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)
c <- as.numeric(args[1])

library(psych)
library(MASS)
options(scipen = 999)

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
# Step 1: Build 9x9 tetrachoric correlation matrix (within-set r = 0.8)
# -----------------------------------------------------------------------------

nmisconceptions        <- 9
nMisconceptionPatterns <- 2^9
nObsTC                 <- 10000000

misconceptionProfiles <- NULL
for (i in 1:nMisconceptionPatterns) {
  misconceptionProfiles <- rbind(
    misconceptionProfiles,
    dec2bin(i - 1, nmisconceptions, rep(2, nmisconceptions))
  )
}

corr9_2 <- matrix(0, nrow = 9, ncol = 9)
diag(corr9_2) <- 1

# Set 1: misconceptions 1, 4, 7
corr9_2[1, c(4, 7)] <- corr9_2[c(4, 7), 1] <- 0.8
corr9_2[4, 7]       <- corr9_2[7, 4]        <- 0.8

# Set 2: misconceptions 2, 5, 8
corr9_2[2, c(5, 8)] <- corr9_2[c(5, 8), 2] <- 0.8
corr9_2[5, 8]       <- corr9_2[8, 5]        <- 0.8

# Set 3: misconceptions 3, 6, 9
corr9_2[3, c(6, 9)] <- corr9_2[c(6, 9), 3] <- 0.8
corr9_2[6, 9]       <- corr9_2[9, 6]        <- 0.8

# Cutpoints drawn from range observed in Sim 1 Condition 1
cutpoints <- runif(n = 9, min = 0.335, max = 1.17)

# -----------------------------------------------------------------------------
# Step 2: Generate misconception profile probabilities
# -----------------------------------------------------------------------------

continuousData  <- mvrnorm(n = nObsTC, mu = rep(0, nmisconceptions), Sigma = corr9_2)
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
# Step 3: Generate item response data for 1,000 examinees on 30 items
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
# Step 4: Build Q-matrix using row index sequences
# -----------------------------------------------------------------------------

num_distractors <- rep(4, nItems)

row_names <- c()
for (i in 1:length(num_distractors)) {
  row_names <- c(row_names, paste(i, letters[1:num_distractors[i]], sep = ""))
}

option_qmatrix <- matrix(0, nrow = length(row_names), ncol = nmisconceptions)
rownames(option_qmatrix) <- row_names
colnames(option_qmatrix) <- paste("d", 1:nmisconceptions, sep = "")

# B options: misconceptions 1 (items 1-10), 4 (items 11-20), 7 (items 21-30)
option_qmatrix[seq(from =  2, to =  40, by = 4), 1] <- 1
option_qmatrix[seq(from = 42, to =  79, by = 4), 4] <- 1
option_qmatrix[seq(from = 82, to = 119, by = 4), 7] <- 1

# C options: misconceptions 2 (items 1-10), 5 (items 11-20), 8 (items 21-30)
option_qmatrix[seq(from =  3, to =  40, by = 4), 2] <- 1
option_qmatrix[seq(from = 43, to =  79, by = 4), 5] <- 1
option_qmatrix[seq(from = 83, to = 119, by = 4), 8] <- 1

# D options: misconceptions 3 (items 1-10), 6 (items 11-20), 9 (items 21-30)
option_qmatrix[seq(from =  4, to =  40, by = 4), 3] <- 1
option_qmatrix[seq(from = 44, to =  80, by = 4), 6] <- 1
option_qmatrix[seq(from = 84, to = 120, by = 4), 9] <- 1

# -----------------------------------------------------------------------------
# Step 5: Generate item parameters
# -----------------------------------------------------------------------------

itemParameters <- list()
for (item in 1:nItems) {
  itemParameters[[item]] <- list()
  noptions <- num_distractors[item]
  for (option in 1:noptions) {
    if (option == 1) {
      # Correct option: ability effect, no misconception effect
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),
        runif(n = 1, min = 0, max = 4),
        0
      )
    } else {
      # Incorrect option: misconception effect, no ability effect
      itemParameters[[item]]$optionParameters[[option]] <- c(
        runif(n = 1, min = -2, max = 1.5),
        0,
        runif(n = 1, min = 0, max = 4)
      )
    }
  }
}

# -----------------------------------------------------------------------------
# Step 6: Simulate item responses using the SICM model
# All 9 misconception effects computed explicitly per latent column
# -----------------------------------------------------------------------------

maineffect   <- list()
itemResponse <- data.frame()

for (person in 1:nObs) {
  maineffect[[person]] <- list()
  option_num <- 1

  for (item in 1:nItems) {
    logItemModels <- NA
    maineffect[[person]][[item]] <- list()

    for (option in 1:num_distractors[item]) {
      maineffect[[person]][[item]][[option]] <- c(
        itemParameters[[item]]$optionParameters[[option]][1],
        itemParameters[[item]]$optionParameters[[option]][2] * theta[person],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 1] * latents[person, 2],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 2] * latents[person, 3],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 3] * latents[person, 4],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 4] * latents[person, 5],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 5] * latents[person, 6],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 6] * latents[person, 7],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 7] * latents[person, 8],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 8] * latents[person, 9],
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 9] * latents[person, 10]
      )

      logItemModels[option] <-
        itemParameters[[item]]$optionParameters[[option]][1] +
        itemParameters[[item]]$optionParameters[[option]][2] * theta[person] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 1] * latents[person, 2] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 2] * latents[person, 3] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 3] * latents[person, 4] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 4] * latents[person, 5] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 5] * latents[person, 6] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 6] * latents[person, 7] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 7] * latents[person, 8] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 8] * latents[person, 9] +
        itemParameters[[item]]$optionParameters[[option]][3] * option_qmatrix[option_num, 9] * latents[person, 10]

      option_num <- option_num + 1
    }

    itemOptionProbs <- exp(logItemModels) / sum(exp(logItemModels))
    itemResponse[person, item] <- sample(1:num_distractors[item], 1, prob = itemOptionProbs)
  }
}

# -----------------------------------------------------------------------------
# Step 7: Save output
# -----------------------------------------------------------------------------

if (!dir.exists("Data")) dir.create("Data")

save(itemResponse, latents, itemParameters, option_qmatrix, corr9_2, cutpoints,
     file = paste0("Data/ResponseMatrix9_C2_{", c, "}.RData"))

cat("Replication", c, "complete.\n")
