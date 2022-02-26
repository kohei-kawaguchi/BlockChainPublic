
# initialize --------------------------------------------------------------

rm(list = ls())
library(BlockChain)
library(foreach)
library(magrittr)
library(doParallel)
registerDoParallel()
options(dplyr.summarise.inform = FALSE)


# read data ---------------------------------------------------------------

# daily hashrate data
currency_date_hashrate <- readRDS(file = "cleaned/currency_date_hashrate.rds")
# base reward data
reward <- readRDS(file = "cleaned/currency_reawrd_date.rds")
# currency rate data
rate <- readRDS(file = "cleaned/currency_rate_date_yahoo.rds")
# asic machine
asic_list <- readRDS(file = "cleaned/asic_list.rds")
# cpu list
gpu_list <- readRDS(file = "cleaned/gpu_list.rds")

# check data
currency_date_hashrate %>%
  dplyr::filter(currency == "DCR") %>%
  dplyr::group_by(currency, algo, date) %>%
  dplyr::mutate(n = length(date)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(currency, algo, date)

currency_date_hashrate %>%
  dplyr::filter(currency == "KMD") %>%
  dplyr::group_by(currency, algo, date) %>%
  dplyr::mutate(n = length(date)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n > 1) %>%
  dplyr::arrange(currency, algo, date)


# transform data ----------------------------------------------------------

## construct currency-algo-date-level data --------------------------------

# construct currency-algo-date-level header
currency_algo_date <-
  currency_date_hashrate %>%
  dplyr::left_join(reward, by = c("currency", "date")) %>%
  dplyr::left_join(rate, by = c("currency", "date")) %>%
  dplyr::arrange(currency, algo, date) %>%
  dplyr::group_by(currency, algo) %>%
  tidyr::fill(reward, rate_quoteusd, volume, .direction = "down") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(reward_quoteusd = reward * rate_quoteusd) %>%
  dplyr::filter(!is.na(winning_rate))

# asic-resist
currency_algo_date <-
  currency_algo_date %>%
  dplyr::mutate(
    asic_dummy = ifelse(algo %in% unique(asic_list$algo), TRUE, FALSE)
  )

# check NA structure
currency_algo_date_NA <-
  currency_algo_date %>%
  dplyr::mutate(
    hash_rate_info = !is.na(hash_rate),
    reward_info = !is.na(reward_quoteusd)
  ) %>%
  dplyr::group_by(currency, algo, hash_rate_info, reward_info) %>%
  dplyr::summarise(n = length(currency)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(currency, algo) %>%
  dplyr::mutate(n = n / sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!reward_info)

# drop dates without reward information
currency_algo_date <-
  currency_algo_date %>%
  dplyr::filter(
    !is.na(hash_rate),
    !is.na(reward_quoteusd),
    !is.na(volume)
  )

# save
saveRDS(currency_algo_date, file = "output/currency_algo_date.rds")

# make scrypt dummy
currency_algo_date <-
  currency_algo_date %>%
  dplyr::group_by(currency) %>%
  dplyr::mutate(scrypt_dummy = max(algo == "scrypt")) %>%
  dplyr::ungroup()
currency_algo_date_top <-
  currency_algo_date %>%
  dplyr::filter(scrypt_dummy == FALSE)
currency_algo_date_scrypt <-
  currency_algo_date %>%
  dplyr::filter(scrypt_dummy == TRUE)

# cut at the terminal date
terminal_date_top <-
  currency_algo_date_top %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::summarise(date = max(date)) %>%
  dplyr::pull(date)
terminal_date_scrypt <-
  currency_algo_date_scrypt %>%
  dplyr::filter(currency == "XVG") %>%
  dplyr::summarise(date = max(date)) %>%
  dplyr::pull(date)

currency_algo_date_top <-
  currency_algo_date_top %>%
  dplyr::filter(date <= terminal_date_top)
currency_algo_date_scrypt <-
  currency_algo_date_scrypt %>%
  dplyr::filter(date <= terminal_date_scrypt)

saveRDS(currency_algo_date_top, file = "output/currency_algo_date_top.rds")
saveRDS(currency_algo_date_scrypt, file = "output/currency_algo_date_scrypt.rds")

## make index -------------------------------------------------------------

# currency k = 1, ..., K
currency_top_index <-
  currency_algo_date_top %>%
  dplyr::distinct(currency) %>%
  dplyr::arrange(currency) %>%
  dplyr::mutate(k = 1:length(currency))
currency_scrypt_index <-
  currency_algo_date_scrypt %>%
  dplyr::distinct(currency) %>%
  dplyr::arrange(currency) %>%
  dplyr::mutate(k = 1:length(currency))

# algo a = 1, ..., A
algo_top_index <-
  currency_algo_date_top %>%
  dplyr::distinct(algo) %>%
  dplyr::arrange(algo) %>%
  dplyr::mutate(a = 1:length(algo))
algo_scrypt_index <-
  currency_algo_date_scrypt %>%
  dplyr::distinct(algo) %>%
  dplyr::arrange(algo) %>%
  dplyr::mutate(a = 1:length(algo))

# currency-algo pair i = 1, ..., I
currency_algo_top_index <-
  currency_algo_date_top %>%
  dplyr::distinct(currency, algo) %>%
  dplyr::arrange(currency, algo) %>%
  dplyr::mutate(i = 1:length(currency))
currency_algo_scrypt_index <-
  currency_algo_date_scrypt %>%
  dplyr::distinct(currency, algo) %>%
  dplyr::arrange(currency, algo) %>%
  dplyr::mutate(i = 1:length(currency))

# asic machine n = 1, ..., N
asic_top_index <-
  asic_list %>%
  dplyr::filter(algo %in% algo_top_index$algo) %>%
  dplyr::distinct(model) %>%
  dplyr::arrange(model) %>%
  dplyr::mutate(n = 1:length(model))
asic_scrypt_index <-
  asic_list %>%
  dplyr::filter(algo %in% algo_scrypt_index$algo) %>%
  dplyr::distinct(model) %>%
  dplyr::arrange(model) %>%
  dplyr::mutate(n = 1:length(model))

# gpu machine m = 1, ..., M
gpu_index <-
  gpu_list %>%
  dplyr::distinct(model) %>%
  dplyr::arrange(model) %>%
  dplyr::mutate(m = 1:length(model))


index <-
  list(
    currency_top_index = currency_top_index,
    currency_scrypt_index = currency_scrypt_index,
    algo_top_index = algo_top_index,
    algo_scrypt_index = algo_scrypt_index,
    currency_algo_top_index = currency_algo_top_index,
    currency_algo_scrypt_index = currency_algo_scrypt_index,
    asic_top_index = asic_top_index,
    asic_scrypt_index = asic_scrypt_index,
    gpu_index = gpu_index
  )
saveRDS(index, file = "output/index.rds")


## make spec matrix -------------------------------------------------------

# spec matrix of asic machine for top currencies
asic_spec_top <-
  asic_list %>%
  dplyr::filter(algo %in% algo_top_index$algo) %>%
  dplyr::select(model, algo, release, hash_rate_spec, power)

# spec matrix of asic machine for scrypt currencies
asic_spec_scrypt <-
  asic_list %>%
  dplyr::filter(algo %in% algo_scrypt_index$algo) %>%
  dplyr::select(model, algo, release, hash_rate_spec, power)

# spec matrix of gpu 
gpu_spec <-
  gpu_list %>%
  dplyr::left_join(gpu_index, by = "model") %>%
  dplyr::select(model, release, hash_rate_spec, power)

# save
saveRDS(asic_spec_top, file = "output/asic_spec_top.rds")
saveRDS(asic_spec_scrypt, file = "output/asic_spec_scrypt.rds")
saveRDS(gpu_spec, file = "output/gpu_spec.rds")







