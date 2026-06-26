## =============================================================================
## Simulation 1: Aggregate Network Results (3 Misconceptions, Condition 2)
## =============================================================================
##
## Description:
##   Reads all 1,000 NetworkResults/rep{c}.RData files produced by
##   network3_estimate_Argon_C2.R and aggregates the per-replication group
##   average edge weights into combined vectors used to produce Table 2
##   and Figure 2A in the paper.
##
##   Run locally after all 1,000 HPC jobs for Condition 2 are complete.
##   Point RESULTS_DIR at the NetworkResults folder for Condition 2.
##
## Condition 2 — High correlation (r = 0.8)
##
## Input:
##   NetworkResults/rep1.RData ... NetworkResults/rep1000.RData
##
## Output:
##   network_avgs3_2.RData
##   Contains per-replication vectors of group mean edge weights:
##     avg_bothcorrect, avg_onecorrect, avg_samemis,
##     avg_corrmis, avg_uncorrmis
##   Also contains: summary_table (Table 2 values: mean, SD per group)
##
## Usage:
##   Set RESULTS_DIR to the path of the Condition 2 NetworkResults folder,
##   then run the full script.
##
## Dependencies: none (base R only)
## =============================================================================

RESULTS_DIR <- "NetworkResults"   # path to Condition 2 NetworkResults folder
N_REPS      <- 1000

# -----------------------------------------------------------------------------
# Initialize storage
# -----------------------------------------------------------------------------

avg_bothcorrect <- numeric(N_REPS)
avg_onecorrect  <- numeric(N_REPS)
avg_samemis     <- numeric(N_REPS)
avg_corrmis     <- numeric(N_REPS)
avg_uncorrmis   <- numeric(N_REPS)
skipped         <- c()

# -----------------------------------------------------------------------------
# Loop over replications and extract group averages
# -----------------------------------------------------------------------------

for (r in 1:N_REPS) {

  rep_file <- file.path(RESULTS_DIR, paste0("rep", r, ".RData"))

  if (!file.exists(rep_file)) {
    cat("Missing:", rep_file, "\n")
    skipped <- c(skipped, r)
    avg_bothcorrect[r] <- avg_onecorrect[r] <- avg_samemis[r] <-
      avg_corrmis[r] <- avg_uncorrmis[r] <- NA
    next
  }

  # Load into a temporary environment to avoid overwriting workspace objects
  env <- new.env()
  load(rep_file, envir = env)
  avg_df <- env$qgraph_avg

  get_mean <- function(group_label) {
    row <- avg_df[avg_df$group == group_label, ]
    if (nrow(row) == 0) return(NA)
    return(row$value_mean)
  }

  avg_bothcorrect[r] <- get_mean("both correct")
  avg_onecorrect[r]  <- get_mean("one correct")
  avg_samemis[r]     <- get_mean("same misconception")
  avg_corrmis[r]     <- get_mean("correlated misconceptions")
  avg_uncorrmis[r]   <- get_mean("uncorrelated misconceptions")

  if (r %% 100 == 0) cat("Processed rep", r, "\n")
}

if (length(skipped) > 0) {
  cat("\nSkipped", length(skipped), "replications:", skipped, "\n")
} else {
  cat("\nAll", N_REPS, "replications processed successfully.\n")
}

# -----------------------------------------------------------------------------
# Build summary table (Table 2 in paper)
# -----------------------------------------------------------------------------

summary_table <- data.frame(
  group        = c("Both correct", "One correct", "Same misconception",
                   "Correlated misconceptions", "Uncorrelated misconceptions"),
  average_mean = c(mean(avg_bothcorrect, na.rm = TRUE),
                   mean(avg_onecorrect,  na.rm = TRUE),
                   mean(avg_samemis,     na.rm = TRUE),
                   mean(avg_corrmis,     na.rm = TRUE),
                   mean(avg_uncorrmis,   na.rm = TRUE)),
  average_sd   = c(sd(avg_bothcorrect, na.rm = TRUE),
                   sd(avg_onecorrect,  na.rm = TRUE),
                   sd(avg_samemis,     na.rm = TRUE),
                   sd(avg_corrmis,     na.rm = TRUE),
                   sd(avg_uncorrmis,   na.rm = TRUE))
)

print(summary_table)

# -----------------------------------------------------------------------------
# Save aggregated results
# -----------------------------------------------------------------------------

save(avg_bothcorrect, avg_onecorrect, avg_samemis, avg_corrmis, avg_uncorrmis,
     summary_table, skipped,
     file = "network_avgs3_2.RData")

cat("\nSaved: network_avgs3_2.RData\n")
