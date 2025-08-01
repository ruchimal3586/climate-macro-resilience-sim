simulate_debt_pathway <- function(years = 30,
                                  gdp_init = 100,
                                  debt_init = 50,
                                  rev_ratio = 0.15,
                                  spend_ratio = 0.18,
                                  shock_prob = 0.2,
                                  shock_loss_pct = 0.05,
                                  emergency_spend_pct = 0.03,
                                  adaptation_cost_pct = 0.02,
                                  adaptation_years = 5,
                                  adaptation_effectiveness = 0.5,
                                  run_adaptation = TRUE,
                                  seed = 123) {
  
  set.seed(seed)
  results <- data.frame(year = 1:years,
                        GDP = NA,
                        Debt = NA,
                        Debt_to_GDP = NA,
                        Shock = FALSE)
  
  gdp <- gdp_init
  debt <- debt_init
  
  for (t in 1:years) {
    adapt_cost <- if (run_adaptation && t <= adaptation_years) adaptation_cost_pct else 0
    
    gdp <- gdp - (gdp * adapt_cost)
    spend <- spend_ratio * gdp + gdp * adapt_cost
    shock <- runif(1) < shock_prob
    
    if (shock) {
      loss <- shock_loss_pct * (1 - if (run_adaptation) adaptation_effectiveness else 1)
      emerg_spend <- emergency_spend_pct * (1 - if (run_adaptation) adaptation_effectiveness else 1)
      gdp <- gdp * (1 - loss)
      spend <- spend + gdp * emerg_spend
    }
    
    revenue <- rev_ratio * gdp
    deficit <- spend - revenue
    debt <- debt + deficit
    
    results$GDP[t] <- gdp
    results$Debt[t] <- debt
    results$Debt_to_GDP[t] <- debt / gdp
    results$Shock[t] <- shock
  }
  
  return(results)
}

------------------------------------------------------------------------------
  # simulate_multiple_runs()
  # ------------------------------------------------------------------------------
# Runs multiple iterations (n) of the sovereign debt simulation model to account
# for randomness in climate shock timing and impact.
#
# This function calls simulate_debt_pathway() for both adaptation and no-adaptation
# scenarios, assigns unique seeds for reproducibility, and binds results across runs.
#
# Arguments:
#   - n: number of simulation runs (e.g., 100)
#   - run_adaptation: whether to include adaptation costs/effects
#   - ...: any other parameters passed to simulate_debt_pathway()
#
# Returns:
#   - A data.frame of stacked simulation results with year, GDP, Debt, Scenario, etc.
#
# Usage:
#   results <- simulate_multiple_runs(n = 100, run_adaptation = TRUE, ...)
# ------------------------------------------------------------------------------


simulate_multiple_runs <- function(n = 100, run_adaptation = TRUE, ...) {
  all_runs <- lapply(1:n, function(i) {
    result <- simulate_debt_pathway(run_adaptation = run_adaptation, seed = i, ...)
    result$sim_id <- i
    result$Scenario <- if (run_adaptation) "Adaptation" else "No Adaptation"
    return(result)
  })
  do.call(rbind, all_runs)
}

