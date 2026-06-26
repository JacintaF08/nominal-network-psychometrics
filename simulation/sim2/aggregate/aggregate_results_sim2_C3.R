## =============================================================================
## Simulation 2: Aggregate Network Results (9 Misconceptions, Condition 3)
## =============================================================================
##
## Description:
##   Reads all 1,000 NetworkResults/rep{c}.RData files produced by
##   network9_estimate_Argon_C3.R and aggregates the per-replication group
##   average edge weights into combined vectors used to produce Table 3
##   and Figure 2B in the paper.
##
##   Run locally after all 1,000 HPC jobs for Condition 3 are complete.
##   Point RESULTS_DIR at the NetworkResults folder for Condition 3.
##
## Condition 3 — Within-set correlation r = 0.2 (low)
##
## Input:
##   NetworkResults/rep1.RData ... NetworkResults/rep1000.RData
##
## Output:
##   network_avgs9_3.RData
##   Contains per-replication vectors of group mean edge weights:
##     avg_bothcorrect_9, avg_onecorrect_9, avg_samemis_9,
##     avg_corrmis_9, avg_uncorrmis_9
##   Also contains: summary_table (Table 3 values: mean, SD, n per group)
##
## Usage:
##   Set RESULTS_DIR to the path of the Condition 3 NetworkResults folder,
##   then run the full script.
##
## Dependencies: none (base R only)
## =============================================================================

RESULTS_DIR <- "NetworkResults"   # path to Condition 3 NetworkResults folder
N_REPS      <- 1000

# -----------------------------------------------------------------------------
# Initialize storage
# -----------------------------------------------------------------------------

avg_bothcorrect_9 <- numeric(N_REPS)
avg_onecorrect_9  <- numeric(N_REPS)
avg_samemis_9     <- numeric(N_REPS)
avg_corrmis_9     <- numeric(N_REPS)
avg_uncorrmis_9   <- numeric(N_REPS)
skipped           <- c()

# -----------------------------------------------------------------------------
# Loop over replications and extract group averages
# -----------------------------------------------------------------------------

for (r in 1:N_REPS) {

  rep_file <- file.path(RESULTS_DIR, paste0("rep", r, ".RData"))

  if (!file.exists(rep_file)) {
    cat("Missing:", rep_file, "\n")
    skipped <- c(skipped, r)
    avg_bothcorrect_9[r] <- avg_onecorrect_9[r] <- avg_samemis_9[r] <-
      avg_corrmis_9[r] <- avg_uncorrmis_9[r] <- NA
    next
  }

  env <- new.env()
  load(rep_file, envir = env)
  avg_df <- env$qgraph_avg

  get_mean <- function(group_label) {
    row <- avg_df[avg_df$group == group_label, ]
    if (nrow(row) == 0) return(NA)
    return(row$value_mean)
  }

  avg_bothcorrect_9[r] <- get_mean("both correct")
  avg_onecorrect_9[r]  <- get_mean("one correct")
  avg_samemis_9[r]     <- get_mean("same misconception")
  avg_corrmis_9[r]     <- get_mean("correlated misconceptions")
  avg_uncorrmis_9[r]   <- get_mean("uncorrelated misconceptions")

  if (r %% 100 == 0) cat("Processed rep", r, "\n")
}

if (length(skipped) > 0) {
  cat("\nSkipped", length(skipped), "replications:", skipped, "\n")
} else {
  cat("\nAll", N_REPS, "replications processed successfully.\n")
}

# -----------------------------------------------------------------------------
# Build summary table (Table 3 in paper)
# -----------------------------------------------------------------------------

summary_table <- data.frame(
  group        = c("Both correct", "One correct", "Same misconception",
                   "Correlated misconceptions", "Uncorrelated misconceptions"),
  average_mean = c(mean(avg_bothcorrect_9, na.rm = TRUE),
                   mean(avg_onecorrect_9,  na.rm = TRUE),
                   mean(avg_samemis_9,     na.rm = TRUE),
                   mean(avg_corrmis_9,     na.rm = TRUE),
                   mean(avg_uncorrmis_9,   na.rm = TRUE)),
  average_sd   = c(sd(avg_bothcorrect_9, na.rm = TRUE),
                   sd(avg_onecorrect_9,  na.rm = TRUE),
                   sd(avg_samemis_9,     na.rm = TRUE),
                   sd(avg_corrmis_9,     na.rm = TRUE),
                   sd(avg_uncorrmis_9,   na.rm = TRUE)),
  n_reps       = c(sum(!is.na(avg_bothcorrect_9)), sum(!is.na(avg_onecorrect_9)),
                   sum(!is.na(avg_samemis_9)),     sum(!is.na(avg_corrmis_9)),
                   sum(!is.na(avg_uncorrmis_9)))
)

print(summary_table)

# -----------------------------------------------------------------------------
# Save aggregated results
# -----------------------------------------------------------------------------

save(avg_bothcorrect_9, avg_onecorrect_9, avg_samemis_9,
     avg_corrmis_9, avg_uncorrmis_9, summary_table, skipped,
     file = "network_avgs9_3.RData")

cat("\nSaved: network_avgs9_3.RData\n")
