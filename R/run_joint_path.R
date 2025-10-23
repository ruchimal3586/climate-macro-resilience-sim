

# ------------------------------------------------------------------------------
# Bridge: sovereign macro path -> company cash flows (per year)
#   - Uses sovereign shocks to trigger firm-level intensity sampling
#   - GDP growth shifts demand (revenue)
#   - Debt/GDP shifts spread (placeholder calc)
# ------------------------------------------------------------------------------

run_joint_path <- function(params_country, company_assets,
                           base_rev, base_cogs, base_opex,base_rate = 0.06,
                           demand_elasticity = 1.2,         # GDP -> revenue elasticity
                           spread_beta = 0.5, # bps per 10pp ΔDebt/GDP (toy)
                           firm_debt = 5e8,
                           intensity_sampler = function() rgamma(1, 2, 1.1),
                           seed = 1) {
  
  set.seed(seed)
  
  # Run sovereign path once
  ctry <- simulate_debt_pathway(
    years = params_country$years,
    gdp_init = params_country$gdp_init,
    debt_init = params_country$debt_init,
    rev_ratio = params_country$rev_ratio,
    spend_ratio = params_country$spend_ratio,
    shock_prob = params_country$shock_prob,
    shock_loss_pct = params_country$shock_loss_pct,
    emergency_spend_pct = params_country$emergency_spend_pct,
    adaptation_cost_pct = params_country$adaptation_cost_pct,
    adaptation_years = params_country$adaptation_years,
    adaptation_effectiveness = params_country$adaptation_effectiveness,
    run_adaptation = params_country$run_adaptation,
    seed = seed
  )
  
  
  # ---- normalize column names from simulate_debt_pathway() ----
  # normalize column names
  #names(ctry) <- tolower(names(ctry))
  
  yrs <- nrow(ctry)
  out <- data.frame(
    year = 1:yrs,
    Shock = ctry$Shock,
    GDP = ctry$GDP,
    Debt_to_GDP = ctry$Debt_to_GDP,
    EBIT = NA_real_,
    FCF = NA_real_,
    Downtime_days = 0,
    Spread_bps = NA_real_,
    Rate_eff   = NA_real_,    # base_rate + spread
    Interest   = NA_real_    # interest expense used in FCF
  )
  
  # GDP growth as demand shifter for revenue
  g_growth <- c(NA, diff(ctry$GDP) / head(ctry$GDP, -1))
  debt_gdp0 <- ctry$Debt_to_GDP[1]
  
  for (t in 1:yrs) {
    growth_factor <- ifelse(is.na(g_growth[t]), 0, g_growth[t])
    rev_t <- base_rev * (1 + demand_elasticity * growth_factor)
    
    # Financing environment proxy
    out$Spread_bps[t] <- spread_beta * 10 * (ctry$Debt_to_GDP[t] - debt_gdp0)
    # Effective funding rate at the firm
    out$Rate_eff[t] <- base_rate + out$Spread_bps[t] / 1e4
    # Interest expense using firm debt (kept constant here; you can make it dynamic)
    out$Interest[t] <- firm_debt * out$Rate_eff[t]
    
   
    
    if (!ctry$Shock[t]) {
      # No shock: simple FCF without disruption
      ebit <- rev_t - base_cogs - base_opex
      
      ebt  <- ebit - out$Interest[t]
      tax  <- pmax(0, ebt) * 0.25
      
      fcf  <- (ebt - tax) + 0.04 * rev_t- out$Interest[t]  # add back D&A proxy
      
      out$EBIT[t] <- ebit
      out$FCF[t]  <- fcf
      out$Downtime_days[t] <- 0
      
    } else {
      # Shock year: draw intensities per asset and compute P&L impact
      intensities <- replicate(nrow(company_assets), intensity_sampler())
      # Protection multiplier: scale hazard if adaptation is active
      prot_mult <- if (isTRUE(params_country$run_adaptation)) {
        1 - params_country$adaptation_effectiveness
      } else {
        1
      }
      
      pl <- company_pl(
        assets_df = company_assets, intensities = intensities,
        base_revenue = rev_t, base_cogs = base_cogs, base_opex = base_opex,
        prot_mult = prot_mult
      )
      
      # EBIT from micro-layer already includes net repairs & surge opex
      ebit <- pl$ebit
      
      # ↓ finance & tax
      ebt  <- ebit - out$Interest[t]
      tax  <- pmax(0, ebt) * 0.25
      
      # add back D&A proxy; capex already includes repairs inside company_pl()
      fcf  <- (ebt - tax) + 0.04 * rev_t
      
      out$EBIT[t] <- pl$ebit
      out$FCF[t]  <- pl$fcf - out$Interest[t]
      out$Downtime_days[t] <- pl$downtime_days_total
    }
  }
  
  out
}
