# Create asset table in R
assets <- data.frame(
  asset_id = c("PLANT_A", "DC_B"),
  asset_value = c(400000000, 150000000),
  daily_revenue = c(1200000, 500000),
  gross_margin = c(0.38, 0.30),
  price_pass_through = c(0.25, 0.15),
  insurance_deductible = c(0.01, 0.01),
  insurance_limit = c(0.30, 0.30)
)

# Save as CSV (comma-separated text)
write.csv(assets, file = "data/assets_companyA.csv", row.names = FALSE)
