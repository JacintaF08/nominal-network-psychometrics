## =============================================================================
## Figures and Real Data Analysis
## =============================================================================
##
## Description:
##   This script produces all figures in the paper and runs the nominal network
##   analysis on the Algebra Concept Inventory (ACI) real dataset.
##   It is organized into three sections:
##
##   SECTION 1: Figure 2 — Panel boxplots of simulation results
##              Loads aggregated network_avgs RData files for all 6 conditions
##              (Sim 1 conditions 1-3 and Sim 2 conditions 1-3) and produces
##              the two-panel boxplot figure.
##
##   SECTION 2: Real data preparation
##              Loads and cleans ACI Form A (Spring 2019) data, removing
##              items/options with insufficient observations for CV estimation.
##
##   SECTION 3: Figures 5A and 5B — ACI network models
##              Runs nominal network estimation on the cleaned ACI data and
##              produces three progressive network visualizations:
##              (a) Full network, (b) positive-connections-only (Figure 5A),
##              (c) incorrect-options-only (Figure 5B).
##
## Input:
##   - network_avgs3_1.RData, network_avgs3_2.RData, network_avgs3_3.RData
##     (Simulation 1 aggregated results, from aggregate_results_sim1.R)
##   - network_avgs9_1.RData, network_avgs9_2.RData, network_avgs9_3.RData
##     (Simulation 2 aggregated results, from aggregate_results_sim2.R)
##   - data/real/ACI_FormA_Spring2019.xlsx  (real dataset)
##
## Output:
##   - results/figures/panel_boxplot_figure.pdf  (Figure 2)
##   - results/figures/AlgInvAllOptions.pdf       (Figure 5A)
##   - results/figures/AlgInvNoCorrect.pdf        (Figure 5B)
##
## Dependencies: ggplot2, patchwork, glmnet, dplyr, qgraph, IsingFit, readxl
## =============================================================================

library(ggplot2)
library(patchwork)
library(glmnet)
library(dplyr)
library(qgraph)
library(IsingFit)
library(readxl)

# =============================================================================
# SECTION 1: Figure 2 — Simulation boxplots
# =============================================================================

# Load aggregated results for all 6 conditions
# Sim 1 (3 misconceptions): conditions use variable suffix 2, 5, 8
# indicating correlation conditions 0.2, 0.5, 0.8

load("simulation/sim1/aggregate/network_avgs3_1.RData")  # condition 1 (r = 0.5, article)
load("simulation/sim1/aggregate/network_avgs3_2.RData")  # condition 2 (r = 0.8, high)
load("simulation/sim1/aggregate/network_avgs3_3.RData")  # condition 3 (r = 0.2, low)
load("simulation/sim2/aggregate/network_avgs9_1.RData")
load("simulation/sim2/aggregate/network_avgs9_2.RData")
load("simulation/sim2/aggregate/network_avgs9_3.RData")

# Rename Sim 1 vectors to match correlation labels used in plot_data
# Condition 3 = 0.2, Condition 1 = 0.5, Condition 2 = 0.8
# (suffix in object name reflects correlation level, not condition number)
avg_bothcorrect2 <- avg_bothcorrect  # loaded from network_avgs3_3 (0.2)
avg_onecorrect2  <- avg_onecorrect
avg_samemis2     <- avg_samemis
avg_corrmis2     <- avg_corrmis
avg_uncorrmis2   <- avg_uncorrmis

load("simulation/sim1/aggregate/network_avgs3_1.RData")
avg_bothcorrect5 <- avg_bothcorrect  # loaded from network_avgs3_1 (0.5)
avg_onecorrect5  <- avg_onecorrect
avg_samemis5     <- avg_samemis
avg_corrmis5     <- avg_corrmis
avg_uncorrmis5   <- avg_uncorrmis

load("simulation/sim1/aggregate/network_avgs3_2.RData")
avg_bothcorrect8 <- avg_bothcorrect  # loaded from network_avgs3_2 (0.8)
avg_onecorrect8  <- avg_onecorrect
avg_samemis8     <- avg_samemis
avg_corrmis8     <- avg_corrmis
avg_uncorrmis8   <- avg_uncorrmis

# Repeat for Sim 2 (9 misconceptions) — load with _9 suffix
load("simulation/sim2/aggregate/network_avgs9_3.RData")
avg_bothcorrect2_9 <- avg_bothcorrect
avg_onecorrect2_9  <- avg_onecorrect
avg_samemis2_9     <- avg_samemis
avg_corrmis2_9     <- avg_corrmis
avg_uncorrmis2_9   <- avg_uncorrmis

load("simulation/sim2/aggregate/network_avgs9_1.RData")
avg_bothcorrect5_9 <- avg_bothcorrect
avg_onecorrect5_9  <- avg_onecorrect
avg_samemis5_9     <- avg_samemis
avg_corrmis5_9     <- avg_corrmis
avg_uncorrmis5_9   <- avg_uncorrmis

load("simulation/sim2/aggregate/network_avgs9_2.RData")
avg_bothcorrect8_9 <- avg_bothcorrect
avg_onecorrect8_9  <- avg_onecorrect
avg_samemis8_9     <- avg_samemis
avg_corrmis8_9     <- avg_corrmis
avg_uncorrmis8_9   <- avg_uncorrmis

# Combine into long-format data frames for plotting
plot_data_3 <- rbind(
  cbind(data.frame(value_mean = avg_bothcorrect2), category = "Both Correct\nAnswers",                       correlation = "0.2"),
  cbind(data.frame(value_mean = avg_bothcorrect5), category = "Both Correct\nAnswers",                       correlation = "0.5"),
  cbind(data.frame(value_mean = avg_bothcorrect8), category = "Both Correct\nAnswers",                       correlation = "0.8"),
  cbind(data.frame(value_mean = avg_onecorrect2),  category = "One Correct\nAnswer",                         correlation = "0.2"),
  cbind(data.frame(value_mean = avg_onecorrect5),  category = "One Correct\nAnswer",                         correlation = "0.5"),
  cbind(data.frame(value_mean = avg_onecorrect8),  category = "One Correct\nAnswer",                         correlation = "0.8"),
  cbind(data.frame(value_mean = avg_samemis2),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.2"),
  cbind(data.frame(value_mean = avg_samemis5),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.5"),
  cbind(data.frame(value_mean = avg_samemis8),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.8"),
  cbind(data.frame(value_mean = avg_uncorrmis2),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.2"),
  cbind(data.frame(value_mean = avg_uncorrmis5),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.5"),
  cbind(data.frame(value_mean = avg_uncorrmis8),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.8"),
  cbind(data.frame(value_mean = avg_corrmis2),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.2"),
  cbind(data.frame(value_mean = avg_corrmis5),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.5"),
  cbind(data.frame(value_mean = avg_corrmis8),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.8")
)

plot_data_9 <- rbind(
  cbind(data.frame(value_mean = avg_bothcorrect2_9), category = "Both Correct\nAnswers",                       correlation = "0.2"),
  cbind(data.frame(value_mean = avg_bothcorrect5_9), category = "Both Correct\nAnswers",                       correlation = "0.5"),
  cbind(data.frame(value_mean = avg_bothcorrect8_9), category = "Both Correct\nAnswers",                       correlation = "0.8"),
  cbind(data.frame(value_mean = avg_onecorrect2_9),  category = "One Correct\nAnswer",                         correlation = "0.2"),
  cbind(data.frame(value_mean = avg_onecorrect5_9),  category = "One Correct\nAnswer",                         correlation = "0.5"),
  cbind(data.frame(value_mean = avg_onecorrect8_9),  category = "One Correct\nAnswer",                         correlation = "0.8"),
  cbind(data.frame(value_mean = avg_samemis2_9),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.2"),
  cbind(data.frame(value_mean = avg_samemis5_9),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.5"),
  cbind(data.frame(value_mean = avg_samemis8_9),     category = "Both Incorrect-\nSame\nMisconception",        correlation = "0.8"),
  cbind(data.frame(value_mean = avg_uncorrmis2_9),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.2"),
  cbind(data.frame(value_mean = avg_uncorrmis5_9),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.5"),
  cbind(data.frame(value_mean = avg_uncorrmis8_9),   category = "Both Incorrect-\nUncorrelated\nMisconceptions", correlation = "0.8"),
  cbind(data.frame(value_mean = avg_corrmis2_9),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.2"),
  cbind(data.frame(value_mean = avg_corrmis5_9),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.5"),
  cbind(data.frame(value_mean = avg_corrmis8_9),     category = "Both Incorrect-\nCorrelated\nMisconceptions",   correlation = "0.8")
)

# Factor ordering for x-axis
category_order <- c(
  "Both Correct\nAnswers",
  "One Correct\nAnswer",
  "Both Incorrect-\nSame\nMisconception",
  "Both Incorrect-\nUncorrelated\nMisconceptions",
  "Both Incorrect-\nCorrelated\nMisconceptions"
)
plot_data_3$category   <- factor(plot_data_3$category,   levels = category_order)
plot_data_9$category   <- factor(plot_data_9$category,   levels = category_order)
plot_data_3$correlation <- factor(plot_data_3$correlation, levels = c("0.2", "0.5", "0.8"))
plot_data_9$correlation <- factor(plot_data_9$correlation, levels = c("0.2", "0.5", "0.8"))

# Build plots
p3 <- ggplot(plot_data_3, aes(x = category, y = value_mean, fill = correlation)) +
  geom_boxplot() +
  labs(
    title = "(A) Three misconceptions",
    x     = NULL,
    y     = "Mean Value of Coefficients",
    fill  = "Correlation Between\nMisconception Sets"
  ) +
  coord_cartesian(ylim = c(-0.2, 0.65)) +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0), axis.text.x = element_text(size = 9))

plot_data_9 <- plot_data_9[is.finite(plot_data_9$value_mean), ]
p9 <- ggplot(plot_data_9, aes(x = category, y = value_mean, fill = correlation)) +
  geom_boxplot() +
  labs(
    title = "(B) Nine misconceptions",
    x     = NULL,
    y     = "Mean Value of Coefficients",
    fill  = "Correlation Between\nMisconception Sets"
  ) +
  coord_cartesian(ylim = c(-0.2, 0.65)) +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0), axis.text.x = element_text(size = 9))

# Combine and save
p3_no_legend <- p3 + theme(legend.position = "none")
p3_no_legend / p9

if (!dir.exists("results/figures")) dir.create("results/figures", recursive = TRUE)
ggsave("results/figures/panel_boxplot_figure.pdf", width = 9, height = 10)
cat("Saved: results/figures/panel_boxplot_figure.pdf\n")

# =============================================================================
# SECTION 2: Real data preparation — Algebra Concept Inventory (Form A, Sp2019)
# =============================================================================
# Data: Form A, Spring 2019 (N = 183 → 143 after exclusions)
# Exclusions:
#   - Item 3: removed due to low discrimination and high guessing (3PL results)
#   - Items 1, 4, 5: removed (insufficient response category variation)
#   - 10 additional item options: removed (0 or 1 observations, CV would fail)
#   - 13 cases: removed (invalid/ambiguous paper form markings)
# Final dataset: 18 items, 65 options, 143 observations

Sp19 <- read_excel("data/real/ACI_Spring2019.xlsx", sheet = "Sp19 All Forms")

# Subset to Form A only
formA <- Sp19[Sp19$Form == "A", ]
formA <- formA[, -3]  # drop form column

colnames(formA) <- c("Class", "AnonymousID", paste("x", 1:(ncol(formA) - 2), sep = ""))

formA <- formA[, -5]    # remove item 3
formA <- na.omit(formA) # remove 13 cases with missing/invalid responses

# Remove items 1, 4, 5 (insufficient option variation for 20-fold CV)
formA <- formA[, -c(3, 5, 6)]

# Remove individual options with 0 or 1 observations (CV would fail)
formA <- formA[formA$x2  != "d" & formA$x2  != "c",  ]
formA <- formA[formA$x6  != "e" & formA$x6  != "d" & formA$x6  != "c", ]
formA <- formA[formA$x10 != "b" & formA$x10 != "d", ]
formA <- formA[formA$x11 != "c", ]
formA <- formA[formA$x12 != "d", ]
formA <- formA[formA$x13 != "c|d", ]
formA <- formA[formA$x14 != "d" & formA$x14 != "e", ]
formA <- formA[formA$x21 != "a", ]
formA <- formA[formA$x22 != "e", ]

# Re-letter options where gaps were created by removals
formA$x10[formA$x10 == "c"] <- "b"
formA$x11[formA$x11 == "d"] <- "c"
formA$x21[formA$x21 == "b"] <- "a"
formA$x21[formA$x21 == "c"] <- "b"
formA$x21[formA$x21 == "d"] <- "c"
formA$x21[formA$x21 == "e"] <- "d"

# Convert letters to numeric option codes
formA <- as.data.frame(formA)
formA[formA == "a"] <- 1
formA[formA == "b"] <- 2
formA[formA == "c"] <- 3
formA[formA == "d"] <- 4
formA[formA == "e"] <- 5
formA[, 3:20] <- formA[, 3:20] %>% mutate_if(is.character, as.numeric)

cat("Final ACI dataset: ", nrow(formA), "observations,", ncol(formA) - 2, "items\n")

# =============================================================================
# SECTION 3: Figures 5A and 5B — ACI network models
# =============================================================================

data  <- as.data.frame(formA[3:20])
names(data) <- seq_len(ncol(data))
nvar  <- ncol(data)

# Number of valid options per item (varies after exclusions)
options <- NULL
for (var in 1:nvar) {
  options <- c(options, length(levels(as.factor(data[, var]))))
}

# Build node names and factor-code items
bigMatrixNames <- NULL
for (var in 1:nvar) {
  data[, var] <- as.factor(data[, var])
  levels(data[, var]) <- 1:options[var]
  bigMatrixNames <- c(bigMatrixNames, paste0("V", var, ".O", 1:options[var]))
}

qgraphMatrix <- matrix(0, nrow = length(bigMatrixNames), ncol = length(bigMatrixNames))
rownames(qgraphMatrix) <- colnames(qgraphMatrix) <- bigMatrixNames

# Fit multinomial LASSO for each item
modelData     <- vector(mode = "list", length = nvar)
qgraphMatrixRow <- 0

for (var in 1:nvar) {
  cat(var, "\n")
  predList <- (1:nvar)[-var]

  tempMatrix <- model.matrix(formula(~0 + data[, predList[1]]), data = data)
  colnames(tempMatrix) <- paste0("V", predList[1], ".O", 1:options[predList[1]])
  for (pred in 2:length(predList)) {
    tempMatrix2 <- model.matrix(formula(~0 + data[, predList[pred]]), data = data)
    colnames(tempMatrix2) <- paste0("V", predList[pred], ".O", 1:options[predList[pred]])
    tempMatrix <- cbind(tempMatrix, tempMatrix2)
  }

  modelData[[var]]$y      <- data[, var]
  modelData[[var]]$x      <- tempMatrix
  modelData[[var]]$cvFit  <- cv.glmnet(x = tempMatrix, y = data[, var],
                                        family = "multinomial", type.multinomial = "grouped")
  modelData[[var]]$optimal <- glmnet(x = tempMatrix, y = data[, var],
                                      family = "multinomial",
                                      lambda = modelData[[var]]$cvFit$lambda.min,
                                      intercept = FALSE)
  modelData[[var]]$coef <- coef(modelData[[var]]$optimal)

  for (pred in 1:length(levels(modelData[[var]]$y))) {
    qgraphMatrixRow <- qgraphMatrixRow + 1
    values    <- modelData[[var]]$coef[pred][[1]]
    locations <- rownames(modelData[[var]]$coef[pred][[1]])
    qgraphMatrix[qgraphMatrixRow, locations[values != 0]] <- values[values != 0]
  }
}

qgraphMatrix <- (qgraphMatrix + t(qgraphMatrix)) / 2

# ---- Step 1: Remove nodes with all-zero coefficients ----
# Nodes: V3.O3, V4.O1, V7.O1, V8.O3, V9.O2, V10.O1, V12.O4, V13.O5, V14.O4,
#        V16.O2, V16.O4, V17.O4, V18.O3
zero_nodes <- c(7, 9, 18, 23, 25, 29, 40, 45, 49, 56, 58, 63, 66)
qgraphMatrix2 <- qgraphMatrix[-zero_nodes, -zero_nodes]

# ---- Step 2: Remove nodes with all-negative coefficients (Figure 5A) ----
# Nodes: V5.O3, V7.O2, V11.O4, V11.O5, V14.O2, V18.O1, V18.O4
neg_nodes <- c(12, 16, 29, 30, 39, 52, 54)
qgraphMatrix3 <- qgraphMatrix2[-neg_nodes, -neg_nodes]

# Rename nodes to original item-option labels
new_names <- c(
  "V2.O1",  "V2.O2",
  "V6.O1",  "V6.O2",
  "V7.O1",  "V7.O2",  "V7.O4",
  "V8.O2",  "V8.O3",
  "V9.O1",  "V9.O2",  "V9.O4",
  "V10.O1", "V10.O3",
  "V11.O4",
  "V12.O1", "V12.O2",
  "V13.O1", "V13.O3", "V13.O4", "V13.O5",
  "V14.O2", "V14.O3",
  "V15.O1", "V15.O2", "V15.O3",
  "V16.O1", "V16.O2", "V16.O3",
  "V17.O1", "V17.O2", "V17.O3", "V17.O4",
  "V18.O1", "V18.O3",
  "V19.O1", "V19.O2", "V19.O3", "V19.O4", "V19.O5",
  "V20.O1", "V20.O3", "V20.O5",
  "V21.O2", "V21.O3", "V21.O4",
  "V22.O2"
)
colnames(qgraphMatrix3) <- rownames(qgraphMatrix3) <- new_names

# Figure 5A: modified network (positive connections only)
ifobject3 <- list(q = qgraphMatrix3)
class(ifobject3) <- "IsingFit"

pdf("results/figures/AlgInvAllOptions.pdf", width = 7, height = 7)
plot(ifobject3, layout = "spring")
dev.off()
cat("Saved: results/figures/AlgInvAllOptions.pdf\n")

# ---- Step 3: Remove correct-answer nodes (Figure 5B) ----
# Correct options: V2.O1, V6.O2, V7.O1, V7.O2, V9.O1, V9.O2, V10.O1, V10.O3,
#                 V11.O4, V12.O2, V13.O1, V14.O3, V15.O5 (not present), ...
# (indices below correspond to qgraphMatrix3 row/col positions)
correct_nodes <- c(2, 4, 5, 9, 12, 13, 15, 17, 21, 23, 24, 28, 31, 35, 40, 43, 46, 47)
qgraphMatrixwrong <- qgraphMatrix3[-correct_nodes, -correct_nodes]

# Remove any remaining all-zero nodes after correct options removed
# Nodes: V5.O1, V9.O3, V11.O3, V14.O1, V15.O2, V15.O3
zero_after <- c(1, 6, 11, 15, 16, 21, 23, 24)
qgraphMatrixwrong2 <- qgraphMatrixwrong[-zero_after, -zero_after]

# Figure 5B: incorrect-options-only network
ifobjectwrong2 <- list(q = qgraphMatrixwrong2)
class(ifobjectwrong2) <- "IsingFit"

pdf("results/figures/AlgInvNoCorrect.pdf", width = 7, height = 7)
plot(ifobjectwrong2, layout = "spring")
dev.off()
cat("Saved: results/figures/AlgInvNoCorrect.pdf\n")
