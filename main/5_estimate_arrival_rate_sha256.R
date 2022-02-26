
# initialize --------------------------------------------------------------

library(BlockChainPublic)
library(magrittr)
library(foreach)
library(modelsummary)

# read data ---------------------------------------------------------------
epoch_currency <- readRDS(file = here::here("output/epoch_currency_sha256.rds"))


# transform data ----------------------------------------------------------

datetime_initial <- 
  epoch_currency %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::filter(datetime_revised == min(datetime_revised)) %>%
  dplyr::pull(datetime_revised)

algo_epoch_variables <-
  epoch_currency %>%
  dplyr::filter(algo == "sha256") %>%
  dplyr::mutate(
    reward_quoteusd_expected = winning_rate * reward_quoteusd,
    reward_expected = winning_rate * reward
  ) %>%
  tidyr::pivot_wider(
    id_cols = epoch,
    names_from = currency,
    values_from = c(winning_rate, rate_quoteusd, reward_expected, reward_quoteusd, reward_quoteusd_expected, volume)
  ) 
algo_epoch_variables <-
  epoch_currency %>%
  dplyr::filter(algo == "sha256") %>%
  dplyr::select(epoch, blockheight, datetime_revised, currency, generated, epoch_time, winning_rate) %>%
  dplyr::mutate(
    date = lubridate::as_date(datetime_revised),
    week = lubridate::floor_date(datetime_revised, unit = "week"),
    month = lubridate::floor_date(datetime_revised, unit = "month"),
    quarter = lubridate::floor_date(datetime_revised, unit = "quarter"),
    t = difftime(datetime_revised, datetime_initial, unit = "week") %>% as.numeric(),
    t = t + 1
  ) %>%
  dplyr::left_join(
    algo_epoch_variables,
    by = "epoch"
  ) %>%
  dplyr::arrange(currency, datetime_revised) 

# halving datetime
datetime_halving <-
  algo_epoch_variables %>%
  dplyr::filter(blockheight %% 210000 == 0) %>%
  dplyr::group_by(currency, blockheight) %>%
  dplyr::filter(datetime_revised == min(datetime_revised)) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(currency, blockheight, datetime_revised)

# make data after bsv joined
algo_epoch_variables_after_bsv <-
  algo_epoch_variables %>%
  tidyr::drop_na() 

algo_epoch_variables_after_bsv_local <-
  algo_epoch_variables_after_bsv %>%
  dplyr::filter(
    datetime_revised >= datetime_halving %>% dplyr::filter(currency == "BCH", blockheight == 630000) %>% dplyr::pull(datetime_revised) - 3600 * 24 * 28,
    datetime_revised <= datetime_halving %>% dplyr::filter(currency == "BTC", blockheight == 630000) %>% dplyr::pull(datetime_revised) + 3600 * 24 * 28
  )

# make data before bsv joins
algo_epoch_variables_after_bch_before_bsv <-
  algo_epoch_variables %>%
  dplyr::filter(
    is.na(winning_rate_BSV),
    !is.na(winning_rate_BCH)
  ) %>%
  dplyr::filter(
    currency != "BSV"
  )

# make data before bch joins
algo_epoch_variables_before_bch <-
  algo_epoch_variables %>%
  dplyr::filter(
    is.na(winning_rate_BSV),
    is.na(winning_rate_BCH)
  ) %>%
  dplyr::filter(
    currency == "BTC"
  )

algo_epoch_variables_before_bch_local <-
  algo_epoch_variables_before_bch  %>%
  dplyr::group_by(epoch) %>%
  dplyr::mutate(
    neighbor = blockheight %% 210000,
    neighbor = (neighbor < 1000) | (neighbor > 200000),
    neighbor = max(neighbor)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(neighbor == 1)

# estimate arrival rate ---------------------------------------------------

## estimate data after bsv ------------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(currency):log(reward_quoteusd_expected_BSV) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(currency):log(reward_quoteusd_expected_BSV) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(currency):log(reward_quoteusd_expected_BSV) +
      as.factor(month),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(month),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(currency):log(volume_BSV) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(currency):log(volume_BSV) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(currency):log(volume_BSV) +
      as.factor(month)
  )



### use all data -----------------------------------------------------------

mle_list <-
  estimate_arrival_rate(
    h_variable_list = h_variable_list,
    algo_epoch_variables = algo_epoch_variables_after_bsv
  )

epoch_range <- 
  algo_epoch_variables_after_bsv %>%
  dplyr::summarise(
    start = min(epoch),
    end = max(epoch)
  ) %>%
  unlist()

result_list <-
  list(
    h_variable_list = h_variable_list,
    mle_list = mle_list,
    epoch_range = epoch_range
  )

saveRDS(result_list, file = "output/estimate_arrival_rate_after_bsv.rds")

result_list <- readRDS(file = "output/estimate_arrival_rate_after_bsv.rds")

result_list$mle_list %>%
  modelsummary()

mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
saveRDS(mle_list, file = "output/estimate_arrival_rate_after_bsv_modelsummary.rds")



### use near the reward change ---------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(currency):log(reward_quoteusd_expected_BSV),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(reward_expected_BSV) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(rate_quoteusd_BSV) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(currency):log(volume_BSV)
  )

mle_list <-
  estimate_arrival_rate(
    h_variable_list = h_variable_list,
    algo_epoch_variables = algo_epoch_variables_after_bsv_local
  )

epoch_range <- 
  algo_epoch_variables_after_bsv_local %>%
  dplyr::summarise(
    start = min(epoch),
    end = max(epoch)
  ) %>%
  unlist()

result_list <-
  list(
    h_variable_list = h_variable_list,
    mle_list = mle_list,
    epoch_range = epoch_range
  )

saveRDS(result_list, file = "output/estimate_arrival_rate_after_bsv_local.rds")

result_list <- readRDS(file = "output/estimate_arrival_rate_after_bsv_local.rds")

result_list$mle_list %>%
  modelsummary()

mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
saveRDS(mle_list, file = "output/estimate_arrival_rate_after_bsv_local_modelsummary.rds")


### estimate by month ---------------------------------------------------

foreach (i = 1:length(h_variable_list)) %do% {
  h_variable <- h_variable_list[i]
  df <-
    algo_epoch_variables_after_bsv %>%
    dplyr::group_split(month)
  models <- 
    df %>%
    purrr::map(
      .,
      ~ dplyr::pull(., month) %>% unique()
    ) %>%
    purrr::reduce(c) %>%
    gsub(" UTC", "", .)
  df <- 
    df %>%
    purrr::map(
      .,
      ~ estimate_arrival_rate(
        h_variable_list = h_variable,
        algo_epoch_variables = .
      )
    ) %>%
    purrr::map(., ~ .[[1]]) %>%
    magrittr::set_names(models)
  
  result_list <-
    list(
      h_variable_list = h_variable_list,
      mle_list = df
    )

  saveRDS(result_list, file = paste("output/estimate_arrival_rate_after_bsv_month_", i, ".rds", sep = ""))
  
  result_list <- readRDS(file = paste("output/estimate_arrival_rate_after_bsv_month_", i, ".rds", sep = ""))
  
  result_list$mle_list %>%
    modelsummary()
  
  mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
  saveRDS(mle_list, file = paste("output/estimate_arrival_rate_after_bsv_month_", i, "_modelsummary.rds", sep = "")) 
  
  
}

## estimate after bch before bsv ------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH) +
      as.factor(month),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(month),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      log(t),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(quarter),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH) +
      as.factor(month)
  )

### use all data ----------------------------------------------------------

mle_list <-
  estimate_arrival_rate(
    h_variable_list = h_variable_list,
    algo_epoch_variables = algo_epoch_variables_after_bch_before_bsv
  )

epoch_range <- 
  algo_epoch_variables_after_bch_before_bsv %>%
  dplyr::summarise(
    start = min(epoch),
    end = max(epoch)
  ) %>%
  unlist()

result_list <-
  list(
    h_variable_list = h_variable_list,
    mle_list = mle_list,
    epoch_range = epoch_range
  )

saveRDS(result_list, file = "output/estimate_arrival_rate_after_bch_before_bsv.rds")

result_list <- readRDS(file = "output/estimate_arrival_rate_after_bch_before_bsv.rds")

result_list$mle_list %>%
  modelsummary()

mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
saveRDS(mle_list, file = "output/estimate_arrival_rate_after_bch_before_bsv_modelsummary.rds")

### estimate by month ---------------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_quoteusd_expected_BTC) +
      as.factor(currency):log(reward_quoteusd_expected_BCH),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH),
    ~ -1 +
      as.factor(currency) +
      as.factor(currency):log(reward_expected_BTC) +
      as.factor(currency):log(reward_expected_BCH) +
      as.factor(currency):log(rate_quoteusd_BTC) +
      as.factor(currency):log(rate_quoteusd_BCH) +
      as.factor(currency):log(volume_BTC) +
      as.factor(currency):log(volume_BCH)
  )

foreach (i = 1:length(h_variable_list)) %do% {
  h_variable <- h_variable_list[i]
  df <-
    algo_epoch_variables_after_bch_before_bsv %>%
    dplyr::group_split(month)
  models <- 
    df %>%
    purrr::map(
      .,
      ~ dplyr::pull(., month) %>% unique()
    ) %>%
    purrr::reduce(c) %>%
    gsub(" UTC", "", .)
  df <- 
    df %>%
    purrr::map(
      .,
      ~ estimate_arrival_rate(
        h_variable_list = h_variable,
        algo_epoch_variables = .
      )
    ) %>%
    purrr::map(., ~ .[[1]]) %>%
    magrittr::set_names(models)
  
  result_list <-
    list(
      h_variable_list = h_variable_list,
      mle_list = df
    )
  
  saveRDS(result_list, file = paste("output/estimate_arrival_rate_after_bch_before_bsv_month_", i, ".rds", sep = ""))
  
  result_list <- readRDS(file = paste("output/estimate_arrival_rate_after_bch_before_bsv_month_", i, ".rds", sep = ""))
  
  result_list$mle_list %>%
    modelsummary()
  
  mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
  saveRDS(mle_list, file = paste("output/estimate_arrival_rate_after_bch_before_bsv_month_", i, "_modelsummary.rds", sep = "")) 
}


## estimate before bch joins ----------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      log(reward_quoteusd_expected_BTC) +
      log(t),
    ~ -1 +
      log(reward_quoteusd_expected_BTC) +
      as.factor(quarter),
    ~ -1 +
      log(reward_quoteusd_expected_BTC) +
      as.factor(month),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      log(t),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      as.factor(quarter),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      as.factor(month),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      log(volume_BTC) +
      log(t),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      log(volume_BTC) +
      as.factor(quarter),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      log(volume_BTC) +
      as.factor(month)
  )


### use all data ----------------------------------------------------------

mle_list <-
  estimate_arrival_rate(
    h_variable_list = h_variable_list,
    algo_epoch_variables = algo_epoch_variables_before_bch
  )


epoch_range <- 
  algo_epoch_variables_before_bch %>%
  dplyr::summarise(
    start = min(epoch),
    end = max(epoch)
  ) %>%
  unlist()

result_list <-
  list(
    h_variable_list = h_variable_list,
    mle_list = mle_list,
    epoch_range = epoch_range
  )

saveRDS(result_list, file = "output/estimate_arrival_rate_before_bch.rds")

result_list <- readRDS(file = "output/estimate_arrival_rate_before_bch.rds")

result_list$mle_list %>%
  modelsummary()

mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
saveRDS(mle_list, file = "output/estimate_arrival_rate_before_bch_modelsummary.rds")



### use near the reward change --------------------------------------------

h_variable_list <-
  c(
    ~ -1 +
      log(reward_quoteusd_expected_BTC),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC),
    ~ -1 +
      log(reward_expected_BTC) +
      log(rate_quoteusd_BTC) +
      log(volume_BTC)
  )

mle_list <-
  estimate_arrival_rate(
    h_variable_list = h_variable_list,
    algo_epoch_variables = algo_epoch_variables_before_bch_local
  )

epoch_range <- 
  algo_epoch_variables_before_bch_local %>%
  dplyr::summarise(
    start = min(epoch),
    end = max(epoch)
  ) %>%
  unlist()

result_list <-
  list(
    h_variable_list = h_variable_list,
    mle_list = mle_list,
    epoch_range = epoch_range
  )

saveRDS(result_list, file = "output/estimate_arrival_rate_before_bch_local.rds")

result_list <- readRDS(file = "output/estimate_arrival_rate_before_bch_local.rds")

result_list$mle_list %>%
  modelsummary()

mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
saveRDS(mle_list, file = "output/estimate_arrival_rate_before_bch_local_modelsummary.rds")



# estimate by month -----------------------------------------------------

foreach (i = 1:length(h_variable_list)) %do% {
  h_variable <- h_variable_list[i]
  df <-
    algo_epoch_variables_before_bch %>%
    dplyr::group_split(month)
  models <- 
    df %>%
    purrr::map(
      .,
      ~ dplyr::pull(., month) %>% unique()
    ) %>%
    purrr::reduce(c) %>%
    gsub(" UTC", "", .)
  df <- 
    df %>%
    purrr::map(
      .,
      ~ estimate_arrival_rate(
        h_variable_list = h_variable,
        algo_epoch_variables = .
      )
    ) %>%
    purrr::map(., ~ .[[1]]) %>%
    magrittr::set_names(models)
  
  result_list <-
    list(
      h_variable_list = h_variable_list,
      mle_list = df
    )
  
  saveRDS(result_list, file = paste("output/estimate_arrival_rate_before_bch_month_", i, ".rds", sep = ""))
  
  result_list <- readRDS(file = paste("output/estimate_arrival_rate_before_bch_month_", i, ".rds", sep = ""))
  
  result_list$mle_list %>%
    modelsummary()
  
  mle_list <- modelsummary(result_list$mle_list, stars = TRUE)
  saveRDS(mle_list, file = paste("output/estimate_arrival_rate_before_bch_month_", i, "_modelsummary.rds", sep = "")) 
}






