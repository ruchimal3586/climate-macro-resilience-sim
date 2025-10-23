
# ------------------------------------------------------------------------------
# Company/sector layer: map hazard intensity -> damage, downtime -> P&L -> FCF
# ------------------------------------------------------------------------------

# Vulnerability curve: intensity -> damage ratio (toy)
damage_ratio <- function(intensity, prot = 1) pmin(1, 0.03 *  (intensity * prot)^1.4)

# Downtime curve: intensity -> lost days (toy)
downtime_days <- function(intensity, prot = 1) round(pmin(180, 2 + 12 * (intensity * prot)))

# Per-asset impact model

asset_impact <- function(asset_value, intensity, daily_revenue,
                        gross_margin = 0.35, price_pass_through = 0.3,
                        insurance_deductible = 0.01, insurance_limit = 0.30,
                        prot_mult = 1){
  
  dr <- damage_ratio(intensity, prot=prot_mult)
  dd <- downtime_days(intensity, prot=prot_mult)
  
  # Physical repair cost and simple insurance recovery
  repair_cost <- asset_value * dr
  covered <- pmax(0, pmin(repair_cost - asset_value * insurance_deductible,
                          asset_value * insurance_limit))
  net_repair <- pmax(0, repair_cost - covered)
  
  # Revenue loss from downtime; pass-through can offset margin hit
  lost_revenue <- daily_revenue * dd
  lost_gross_profit <- (1 - price_pass_through) * lost_revenue * gross_margin
  
  # Opex surge during disruption
  surge_opex <- 0.05 * lost_revenue
  
  list(dr = dr, dd = dd, net_repair = net_repair,
       lost_revenue = lost_revenue,
       lost_gross_profit = lost_gross_profit,
       surge_opex = surge_opex)
}

# Company-level P&L for one period given asset intensities
company_pl <- function(assets_df, intensities,
                       base_revenue, base_cogs, base_opex,
                       tax_rate = 0.25, adaptation_capex = 0, 
                       prot_mult = 1) {
  
  stopifnot(length(intensities) == nrow(assets_df))
  
  # Map over assets and sum fields
  imps <- Map(asset_impact,
              asset_value = assets_df$asset_value,
              intensity   = intensities,
              daily_revenue = assets_df$daily_revenue,
              gross_margin = assets_df$gross_margin,
              price_pass_through = assets_df$price_pass_through,
              insurance_deductible = assets_df$insurance_deductible,
              insurance_limit = assets_df$insurance_limit,
              prot_mult = prot_mult)
  
  agg <- Reduce(function(a,b) Map(`+`, a, b), imps)
  
  # Shocked P&L
  revenue <- base_revenue - agg$lost_revenue
  cogs    <- base_cogs + (agg$lost_revenue - agg$lost_gross_profit)  # margin compression
  opex    <- base_opex + agg$surge_opex
  ebit    <- revenue - cogs - opex - agg$net_repair
  
  tax     <- pmax(0, ebit) * tax_rate
  nopat   <- ebit - tax
  d_and_a <- 0.04 * base_revenue                   # simple proxy
  capex   <- adaptation_capex + agg$net_repair      # treat repair as capex
  d_nwc   <- 0.05 * agg$lost_revenue                # friction in working capital
  
  fcf     <- nopat + d_and_a - capex - d_nwc
  
  list(ebit = ebit, fcf = fcf, downtime_days_total = agg$dd)
}
