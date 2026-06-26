## =============================================================================
## Simulation 2: Nominal Network Estimation (9 Misconceptions, Condition 1)
## =============================================================================
##
## Description:
##   Runs the nominal network psychometric analysis on one simulated dataset
##   from Simulation 2, Condition 2 (within-set r = 0.8). Estimates a
##   LASSO-regularized multinomial logistic regression network using glmnet,
##   classifies all item-option pairs into relationship groups, and computes
##   average edge weights per group.
##
## Usage (HPC array job):
##   Rscript network9_estimate_Argon_C2.R <replication_number>
##
## Input:
##   Data/ResponseMatrix9_C2_{c}.RData  (from SICM9_generate_Argon_C2.R)
##
## Output:
##   NetworkResults/rep{c}.RData
##   Contains: qgraphMatrix, qgraph_clean, qgraph_avg
##
## Group labeling logic (9-misconception version):
##   Requires both option number AND item group to classify pairs correctly.
##   Item groups: Group 1 = items 1-10, Group 2 = items 11-20, Group 3 = items 21-30.
##
##   "both correct"              - both options are correct (option 1)
##   "one correct"               - one correct, one incorrect
##   "same misconception"        - same option number, same item group
##   "correlated misconceptions" - same option number, different item group
##                                 (within-set: {1,4,7}, {2,5,8}, {3,6,9})
##   "uncorrelated misconceptions" - different option numbers
##
## Dependencies: glmnet, dplyr
## =============================================================================

args <- commandArgs(trailingOnly = TRUE)
c <- as.numeric(args[1])

library(glmnet)
library(dplyr)

# -----------------------------------------------------------------------------
# Load simulated dataset
# -----------------------------------------------------------------------------

load(paste0("Data/ResponseMatrix9_C2_{", c, "}.RData"))

data    <- as.data.frame(itemResponse)
names(data) <- seq_len(ncol(data))
nvar    <- ncol(data)
options <- 4

# -----------------------------------------------------------------------------
# Build node name index and factor-code items
# -----------------------------------------------------------------------------

bigMatrixNames <- NULL
for (var in 1:nvar) {
  data[, var] <- as.factor(data[, var])
  levels(data[, var]) <- 1:options
  bigMatrixNames <- c(bigMatrixNames, paste0("V", var, ".O", 1:options))
}

qgraphMatrix <- matrix(0, nrow = length(bigMatrixNames), ncol = length(bigMatrixNames))
rownames(qgraphMatrix) <- colnames(qgraphMatrix) <- bigMatrixNames

# -----------------------------------------------------------------------------
# Fit multinomial LASSO for each item
# -----------------------------------------------------------------------------

modelData       <- vector(mode = "list", length = nvar)
qgraphMatrixRow <- 0

for (var in 1:nvar) {
  predList <- (1:nvar)[-var]

  tempMatrix <- model.matrix(formula(~0 + data[, predList[1]]), data = data)
  colnames(tempMatrix) <- paste0("V", predList[1], ".O", 1:options)
  for (pred in 2:length(predList)) {
    tempMatrix2 <- model.matrix(formula(~0 + data[, predList[pred]]), data = data)
    colnames(tempMatrix2) <- paste0("V", predList[pred], ".O", 1:options)
    tempMatrix <- cbind(tempMatrix, tempMatrix2)
  }

  modelData[[var]]$y       <- data[, var]
  modelData[[var]]$x       <- tempMatrix
  modelData[[var]]$cvFit   <- cv.glmnet(x = modelData[[var]]$x, y = modelData[[var]]$y,
                                         family = "multinomial", type.multinomial = "grouped")
  modelData[[var]]$optimal <- glmnet(x = modelData[[var]]$x, y = modelData[[var]]$y,
                                      family = "multinomial",
                                      lambda = modelData[[var]]$cvFit$lambda.min,
                                      intercept = FALSE)
  modelData[[var]]$coef    <- coef(modelData[[var]]$optimal)

  for (pred in 1:length(levels(modelData[[var]]$y))) {
    qgraphMatrixRow <- qgraphMatrixRow + 1
    values    <- modelData[[var]]$coef[pred][[1]]
    locations <- rownames(modelData[[var]]$coef[pred][[1]])
    qgraphMatrix[qgraphMatrixRow, locations[which(values != 0)]] <- values[which(values != 0)]
  }
}

qgraphMatrix <- (qgraphMatrix + t(qgraphMatrix)) / 2

# -----------------------------------------------------------------------------
# Convert to long format (upper triangle only) and classify pairs
# -----------------------------------------------------------------------------

qgraph_long <- data.frame(
  row_index = rep(seq_len(nrow(qgraphMatrix)), times = ncol(qgraphMatrix)),
  col_index = rep(seq_len(ncol(qgraphMatrix)), each  = nrow(qgraphMatrix)),
  rowname   = rep(rownames(qgraphMatrix), times = ncol(qgraphMatrix)),
  colname   = rep(colnames(qgraphMatrix), each  = nrow(qgraphMatrix)),
  value     = as.vector(qgraphMatrix)
)

qgraph_upper <- qgraph_long[qgraph_long$row_index < qgraph_long$col_index,
                              c("rowname", "colname", "value")]

# Option number (last character of node name)
qgraph_upper$op1 <- as.numeric(substr(qgraph_upper$rowname,
                                       nchar(qgraph_upper$rowname),
                                       nchar(qgraph_upper$rowname)))
qgraph_upper$op2 <- as.numeric(substr(qgraph_upper$colname,
                                       nchar(qgraph_upper$colname),
                                       nchar(qgraph_upper$colname)))

# Item number (digits between "V" and ".")
qgraph_upper$item1 <- as.numeric(sub("V([0-9]+)\\..*", "\\1", qgraph_upper$rowname))
qgraph_upper$item2 <- as.numeric(sub("V([0-9]+)\\..*", "\\1", qgraph_upper$colname))

# Item group: 1 = items 1-10, 2 = items 11-20, 3 = items 21-30
qgraph_upper$misgroup1 <- NA
qgraph_upper$misgroup2 <- NA
qgraph_upper$misgroup1[which(qgraph_upper$item1 < 11)]                                    <- 1
qgraph_upper$misgroup1[which(qgraph_upper$item1 > 10 & qgraph_upper$item1 < 21)]          <- 2
qgraph_upper$misgroup1[which(qgraph_upper$item1 > 20)]                                    <- 3
qgraph_upper$misgroup2[which(qgraph_upper$item2 < 11)]                                    <- 1
qgraph_upper$misgroup2[which(qgraph_upper$item2 > 10 & qgraph_upper$item2 < 21)]          <- 2
qgraph_upper$misgroup2[which(qgraph_upper$item2 > 20)]                                    <- 3

# Remove zero edges
qgraph_clean <- qgraph_upper[which(qgraph_upper$value != 0), ]

# Assign relationship groups
qgraph_clean$group <- NA

qgraph_clean$group[which(qgraph_clean$op1 == 1 & qgraph_clean$op2 == 1)] <- "both correct"

qgraph_clean$group[which(qgraph_clean$op1 == 1 & qgraph_clean$op2 > 1)]  <- "one correct"
qgraph_clean$group[which(qgraph_clean$op1 > 1  & qgraph_clean$op2 == 1)] <- "one correct"

# Same misconception: same option, same item group
qgraph_clean$group[which(qgraph_clean$op1 != 1 &
                           qgraph_clean$op2 == qgraph_clean$op1 &
                           qgraph_clean$misgroup1 == qgraph_clean$misgroup2)] <- "same misconception"

# Correlated misconceptions: same option, different item group (within-set)
qgraph_clean$group[which(qgraph_clean$op1 != 1 &
                           qgraph_clean$op2 == qgraph_clean$op1 &
                           qgraph_clean$misgroup1 != qgraph_clean$misgroup2)] <- "correlated misconceptions"

# Uncorrelated misconceptions: different option numbers (within or across groups)
qgraph_clean$group[which(qgraph_clean$op1 != qgraph_clean$op2 &
                           qgraph_clean$op1 != 1 & qgraph_clean$op2 != 1 &
                           qgraph_clean$misgroup1 == qgraph_clean$misgroup2)] <- "uncorrelated misconceptions"

qgraph_clean$group[which(qgraph_clean$op1 != qgraph_clean$op2 &
                           qgraph_clean$op1 != 1 & qgraph_clean$op2 != 1 &
                           qgraph_clean$misgroup1 != qgraph_clean$misgroup2)] <- "uncorrelated misconceptions"

# -----------------------------------------------------------------------------
# Compute per-group summary statistics
# -----------------------------------------------------------------------------

qgraph_avg <- merge(
  x        = aggregate(value ~ group, data = qgraph_clean, FUN = mean),
  y        = aggregate(value ~ group, data = qgraph_clean, FUN = sd),
  by       = "group",
  suffixes = c("_mean", "_sd")
)
qgraph_avg$n <- as.numeric(table(qgraph_clean$group)[qgraph_avg$group])

# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

if (!dir.exists("NetworkResults")) dir.create("NetworkResults")

save.image(file = paste0("NetworkResults/rep", c, ".RData"))

cat("Replication", c, "complete.\n")
