

# ------------------------------------------------------------------------------
# Monte Carlo wrapper for the joint macro->micro model
# ------------------------------------------------------------------------------

run_joint_multiple <- function(n = 500, params_country, company_assets, seed0 =1, ...) {
  set.seed(seed0)
  runs <- lapply(1:n, function(i) {
    out <- run_joint_path(params_country, company_assets, seed = i, ...)
    out$sim_id <- i
    out
  })
  dplyr::bind_rows(runs)
}
