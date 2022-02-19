
# initialize --------------------------------------------------------------

rm(list = ls())
library(BlockChain)
library(foreach)
library(magrittr)
library(doParallel)
registerDoParallel()
options(dplyr.summarise.inform = FALSE)


# read data ---------------------------------------------------------------

# blockheight hashrate data
currency_blockheight_hashrate <- readRDS(file = "cleaned/currency_blockheight_hashrate.rds")
# base reward data
reward <- readRDS(file = "cleaned/currency_reawrd_date.rds")
# currency rate data
rate <- readRDS(file = "cleaned/currency_rate_date_yahoo.rds")
# asic machine
asic_list <- readRDS(file = "cleaned/asic_list.rds")
# cpu list
gpu_list <- readRDS(file = "cleaned/gpu_list.rds")

# transform data ----------------------------------------------------------

## construct currency-algo-blockheight-level data --------------------------------
# make data
currency_blockheight_hashrate <-
  currency_blockheight_hashrate %>%
  dplyr::mutate(date = as.Date(datetime))

# construct currency-algo-blockheight-level header
currency_algo_blockheight <-
  currency_blockheight_hashrate %>%
  dplyr::left_join(reward, by = c("currency", "date")) %>%
  dplyr::left_join(rate, by = c("currency", "date")) %>%
  dplyr::arrange(currency, algo, blockheight) %>%
  dplyr::group_by(currency, algo) %>%
  tidyr::fill(reward, rate_quoteusd, volume, .direction = "down") %>%
  dplyr::ungroup() %>%
  dplyr::mutate(reward_quoteusd = reward * rate_quoteusd) %>%
  dplyr::filter(!is.na(winning_rate))

# asic-resist
currency_algo_blockheight <-
  currency_algo_blockheight %>%
  dplyr::mutate(
    asic_dummy = ifelse(algo %in% unique(asic_list$algo), TRUE, FALSE)
  )

# check NA structure
currency_algo_blockheight_NA <-
  currency_algo_blockheight %>%
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

# drop blockheights without reward information
currency_algo_blockheight <-
  currency_algo_blockheight %>%
  dplyr::filter(
    !is.na(hash_rate),
    !is.na(reward_quoteusd),
    !is.na(volume)
  )

# save
saveRDS(currency_algo_blockheight, file = "output/currency_algo_blockheight.rds")
currency_algo_blockheight <- readRDS(file = "output/currency_algo_blockheight.rds")

# make scrypt dummy
currency_algo_blockheight <-
  currency_algo_blockheight %>%
  dplyr::group_by(currency) %>%
  dplyr::mutate(scrypt_dummy = max(algo == "scrypt")) %>%
  dplyr::ungroup()
currency_algo_blockheight_top <-
  currency_algo_blockheight %>%
  dplyr::filter(scrypt_dummy == FALSE)
currency_algo_blockheight_scrypt <-
  currency_algo_blockheight %>%
  dplyr::filter(scrypt_dummy == TRUE)

# cut at the terminal blockheight
terminal_blockheight_top <-
  currency_algo_blockheight_top %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::summarise(blockheight = max(blockheight)) %>%
  dplyr::pull(blockheight)
terminal_blockheight_scrypt <-
  currency_algo_blockheight_scrypt %>%
  dplyr::filter(currency == "XVG") %>%
  dplyr::summarise(blockheight = max(blockheight)) %>%
  dplyr::pull(blockheight)

currency_algo_blockheight_top <-
  currency_algo_blockheight_top %>%
  dplyr::filter(blockheight <= terminal_blockheight_top)
currency_algo_blockheight_scrypt <-
  currency_algo_blockheight_scrypt %>%
  dplyr::filter(blockheight <= terminal_blockheight_scrypt)

saveRDS(currency_algo_blockheight_top, file = "output/currency_algo_blockheight_top.rds")
saveRDS(currency_algo_blockheight_scrypt, file = "output/currency_algo_blockheight_scrypt.rds")


## make algo-epoch-currency-level data ------------------------------------

# filter sha256 data and complete
epoch_currency_sha256 <-
  currency_algo_blockheight %>%
  dplyr::filter(algo == "sha256") 

epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::select(-reward, -reward_quoteusd) %>%
  dplyr::mutate(
    reward = 50 / (2^(blockheight %/% 210000)),
    reward_quoteusd = reward * rate_quoteusd
  )
  
# revise the datetime to match with the blockheight
epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::mutate(datetime_revised = datetime) %>%
  dplyr::group_split(currency, algo, .keep = TRUE) %>%
  purrr::map(
    .,
    function (df) {
      df <- 
        df %>% 
        dplyr::arrange(blockheight)
      for (i in 2:nrow(df)) {
        df[i, "datetime_revised"] <-
          ifelse(
            df[i, "datetime_revised"] >= df[i - 1, "datetime_revised"],
            df[i, "datetime_revised"],
            df[i - 1, "datetime_revised"]
          )
      }
      df <-
        df %>%
        dplyr::mutate(datetime_revised = as.POSIXct(datetime_revised, origin = "1970-01-01", tz = "UTC"))
      return(df)
    }
  ) %>%
  dplyr::bind_rows()

# check the revisions
epoch_currency_sha256 %>%
  dplyr::select(currency, blockheight, datetime, datetime_revised)

epoch_currency_sha256 %>%
  dplyr::group_by(currency, algo) %>%
  dplyr::arrange(blockheight) %>%
  dplyr::filter(
    datetime < dplyr::lag(datetime, 1) |
      datetime > dplyr::lead(datetime, 1)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::select(currency, blockheight, datetime, datetime_revised)

epoch_currency_sha256 %>%
  dplyr::group_by(currency, algo) %>%
  dplyr::arrange(blockheight) %>%
  dplyr::filter(
    datetime_revised < dplyr::lag(datetime_revised, 1) |
      datetime_revised > dplyr::lead(datetime_revised, 1)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(currency, blockheight, datetime, datetime_revised)

epoch_currency_sha256 %>%
  dplyr::filter(
    blockheight >= 321189,
    blockheight <= 321193
  ) %>%
  dplyr::select(currency, blockheight, datetime, datetime_revised)

# define epoch and epoch time
epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::arrange(datetime_revised, currency) %>%
  dplyr::mutate(epoch = 1:length(datetime_revised)) %>%
  tidyr::complete(tidyr::nesting(epoch, datetime_revised, datetime), currency) %>%
  dplyr::mutate(date = lubridate::as_date(datetime_revised))

epoch_currency_sha256 %>%
  dplyr::filter(
    currency == "BCH",
    blockheight >= 479850,
    blockheight <= 479852
  )

# check the last available date
date_last <-
  currency_algo_blockheight %>%
  dplyr::filter(algo == "sha256") %>%
  dplyr::group_by(currency) %>%
  dplyr::filter(datetime == max(datetime)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(datetime == min(datetime)) %>%
  dplyr::pull(date)

epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::filter(date <= date_last)


# fill the gap and calculate epoch_time and generation dummy
epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::arrange(currency, epoch) %>%
  dplyr::group_by(currency) %>%
  tidyr::fill(dplyr::everything(), .direction = "down") %>%
  dplyr::mutate(
    generated = (blockheight != dplyr::lag(blockheight)),
    epoch_time = difftime(datetime_revised, dplyr::lag(datetime_revised), units = "secs"),
    epoch_time = as.integer(epoch_time)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(algo))

epoch_currency_sha256 %>%
  dplyr::select(epoch, currency, blockheight, datetime, datetime_revised, epoch_time)

# adjust the initial block
epoch_currency_sha256 <-
  epoch_currency_sha256 %>% 
  dplyr::mutate(
    epoch_time = ifelse(is.na(epoch_time), block_time, epoch_time),
    generated = ifelse(is.na(generated), TRUE, generated)
    )

# adjust the block time
epoch_currency_sha256 <-
  epoch_currency_sha256 %>%
  dplyr::arrange(currency, generated, datetime_revised) %>%
  dplyr::group_by(currency, generated) %>%
  dplyr::mutate(
    block_time = ifelse(
      generated,
      difftime(datetime_revised, dplyr::lag(datetime_revised), units = "secs") %>%
        as.integer(),
      NA
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    block_time = ifelse(is.na(block_time) & generated, 0, block_time)
  ) %>%
  dplyr::arrange(epoch, currency, datetime_revised)

# check the consistency of data
epoch_currency_sha256 %>% 
  dplyr::group_by(epoch) %>%
  dplyr::summarise(generated = sum(generated)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(generated > 1) %>%
  nrow()

epoch_currency_sha256 %>% 
  dplyr::group_by(epoch) %>%
  dplyr::summarise(generated = sum(generated)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(generated < 1) %>%
  nrow()

epoch_currency_sha256 %>% 
  dplyr::group_by(epoch) %>%
  dplyr::summarise(epoch_time = max(epoch_time) - min(epoch_time)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(epoch_time > 0) %>%
  nrow()

epoch_currency_sha256 %>%
  summary()

epoch_currency_sha256 %>%
  dplyr::filter(
    currency == "BCH",
    blockheight >= 479850,
    blockheight <= 479852
  )

epoch_currency_sha256 %>%
  dplyr::arrange(currency, epoch) %>%
  dplyr::group_by(currency) %>%
  dplyr::filter(blockheight < dplyr::lag(blockheight, 1)) %>%
  dplyr::ungroup() %>%
  nrow()

# save
saveRDS(epoch_currency_sha256, file = "output/epoch_currency_sha256.rds")



