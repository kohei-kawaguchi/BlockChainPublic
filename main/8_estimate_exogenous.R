
# initialize --------------------------------------------------------------

library(BlockChainPublic)
library(magrittr)
library(foreach)


# read data ---------------------------------------------------------------

# currency rate data
rate <- readRDS(file = "cleaned/currency_rate_date_yahoo.rds")


# estimate parameters -----------------------------------------------------

distr <- "t"
start <- list(df = 10)

rate_estimate <-
  estimate_exogenous(
    rate, 
    distr,
    start
  )

rate_estimate <-
  rate_estimate %>%
  tidyr::pivot_wider(
    id_cols = currency,
    values_from = c(mean, sd, df, se),
    names_glue = "{name}_{.value}"
  )

saveRDS(
  rate_estimate,
  file = "output/rate_estimate.rds"
)
