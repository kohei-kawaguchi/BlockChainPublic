# initialize --------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  setting <- args[1]
} else {
  setting <- "asert_asert_cw144"
}
print(setting)

library(BlockChain)
library(codetools)
library(magrittr)
library(foreach)
library(ggplot2)
library(doParallel)

workers = makeCluster(16, type = "PSOCK")
registerDoParallel(workers)

# import python module ----------------------------------------------------

if (!("blockchain" %in% reticulate::conda_list()$name)) {
  # set up python environment and install pyblp
  reticulate::conda_create(
    envname = "blockchain",
    forge = TRUE,
    python_version = "3.9"
  )
} else {
  # load conda environment and load pyublp
  reticulate::use_condaenv("blockchain", required = TRUE)
  asert <- 
    reticulate::import_from_path(
      module = "asert",
      path = "module"
    )
}

# load data ---------------------------------------------------------------

epoch_currency <- readRDS(file = "output/epoch_currency_sha256.rds")
rate_estimate <- readRDS(file = here::here("output/rate_estimate.rds"))
model_arrival <- readRDS(file = "output/estimate_arrival_rate_after_bsv_local.rds")

# extract objects ---------------------------------------------------------

algo <- "sha256"
currency_base <- "BTC"
specification <- 1
mle <- model_arrival$mle_list[[specification]]
h_variable <- model_arrival$h_variable_list[[specification]]
epoch_range <- model_arrival$epoch_range
theta <- mle$estimate
keep_reward <- TRUE

# set constants -----------------------------------------------------------
daa_actual <-
  list(
    "BTC" = update_winning_rate_btc_actual,
    "BCH" = update_winning_rate_bch_actual,
    "BSV" = update_winning_rate_bsv_actual
  )
daa_original_original_cw144 <-
  list(
    "BTC" = update_winning_rate_btc_actual,
    "BCH" = update_winning_rate_bch_uses_btc_daa,
    "BSV" = update_winning_rate_bsv_actual
  )
daa_original_original_original <-
  list(
    "BTC" = update_winning_rate_btc_actual,
    "BCH" = update_winning_rate_bch_uses_btc_daa,
    "BSV" = update_winning_rate_bsv_uses_btc_daa
  )

daa_original_asert_cw144 <-
  list(
    "BTC" = update_winning_rate_btc_actual,
    "BCH" = update_winning_rate_bch_uses_asert_daa,
    "BSV" = update_winning_rate_bsv_actual
  )
daa_original_asert_asert <-
  list(
    "BTC" = update_winning_rate_btc_actual,
    "BCH" = update_winning_rate_bch_uses_asert_daa,
    "BSV" = update_winning_rate_bsv_uses_asert_daa
  )
daa_asert_asert_asert <-
  list(
    "BTC" = update_winning_rate_btc_uses_asert_daa,
    "BCH" = update_winning_rate_bch_uses_asert_daa,
    "BSV" = update_winning_rate_bsv_uses_asert_daa
  )
daa_cw144_cw144_cw144 <-
  list(
    "BTC" = update_winning_rate_btc_uses_bch_daa,
    "BCH" = update_winning_rate_bch_actual,
    "BSV" = update_winning_rate_bsv_actual
  )
daa_cw144_asert_asert <-
  list(
    "BTC" = update_winning_rate_btc_uses_bch_daa,
    "BCH" = update_winning_rate_bch_uses_asert_daa,
    "BSV" = update_winning_rate_bsv_uses_asert_daa
  )
daa_asert_asert_cw144 <-
  list(
    "BTC" = update_winning_rate_btc_uses_asert_daa,
    "BCH" = update_winning_rate_bch_uses_asert_daa,
    "BSV" = update_winning_rate_bsv_actual
  )

emergency_daa_start <-  epoch_currency %>%
  dplyr::filter(currency == "BCH") %>%
  dplyr::filter(epoch == min(epoch)) %>%
  dplyr::pull(datetime)
emergency_daa_end <- as.POSIXct(1510600000, origin = "1970-01-01", tz = "UTC")

# set the starting epoch
epoch_start <- 
  epoch_currency %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::filter(generated) %>%
  dplyr::filter(blockheight == 630000 - 1) %>%
  dplyr::pull(epoch)

# set the horizon of simulations
horizon <- 2016 * 10

# set the number of simulations
size <- 120

# set the base currency
currency_base <- "BTC"

# set the number of iteration in simulations
iter = 96


# transform data ----------------------------------------------------------

# make the simulation header
epoch_currency_header <-
  epoch_currency %>%
  dplyr::filter(epoch <= epoch_start) %>%
  dplyr::group_by(epoch) %>%
  dplyr::mutate(blockheight_btc = sum(blockheight * (currency == "BTC"))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(blockheight_btc >= (max(blockheight_btc) - 2016)) %>%
  dplyr::select(-blockheight_btc) %>%
  dplyr::arrange(epoch) %>%
  dplyr::left_join(
    rate_estimate,
    by = "currency"
  )

# filter the last epoch
epoch_currency_n <-
  epoch_currency_header %>%
  dplyr::filter(epoch == max(epoch)) 

blockheight_anchor_bch <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BCH") %>%
  dplyr::pull(blockheight)
bits_anchor_bch <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BCH") %>%
  dplyr::pull(bits) %>%
  strtoi(., base = 16)
datetime_anchor_bch <-
  epoch_currency_header %>%
  dplyr::filter(
    currency == "BCH",
    generated,
    blockheight == blockheight_anchor_bch - 1
  ) %>%
  dplyr::pull(datetime) %>%
  as.numeric()

blockheight_anchor_bsv <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BSV") %>%
  dplyr::pull(blockheight)
bits_anchor_bsv <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BSV") %>%
  dplyr::pull(bits) %>%
  strtoi(., base = 16)
datetime_anchor_bsv <-
  epoch_currency_header %>%
  dplyr::filter(
    currency == "BSV",
    generated,
    blockheight == blockheight_anchor_bsv - 1
  ) %>%
  dplyr::pull(datetime) %>%
  as.numeric()

blockheight_anchor_btc <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::pull(blockheight)
bits_anchor_btc <- 
  epoch_currency_n %>%
  dplyr::filter(currency == "BTC") %>%
  dplyr::pull(bits) %>%
  strtoi(., base = 16)
datetime_anchor_btc <-
  epoch_currency_header %>%
  dplyr::filter(
    currency == "BTC",
    generated,
    blockheight == blockheight_anchor_btc - 1
  ) %>%
  dplyr::pull(datetime) %>%
  as.numeric()

anchor <-
  list(
    "BTC" = list(
      blockheight_anchor = blockheight_anchor_btc,
      bits_anchor = bits_anchor_btc,
      datetime_anchor = datetime_anchor_btc
    ),
    "BCH" = list(
      blockheight_anchor = blockheight_anchor_bch,
      bits_anchor = bits_anchor_bch,
      datetime_anchor = datetime_anchor_bch
    ),
    "BSV" = list(
      blockheight_anchor = blockheight_anchor_bsv,
      bits_anchor = bits_anchor_bsv,
      datetime_anchor = datetime_anchor_bsv
    )    
  )



# check time --------------------------------------------------------------

set.seed(1)

time <- system.time(foreach (i = 1:2, 
                     .packages = c(
                       "BlockChain",
                       "dplyr",
                       "magrittr"
                     )
) %dopar% {
epoch_currency_added <-
  simulate_epoch_stochastic_reduced(
    epoch_currency = epoch_currency_header,
    daa = daa_original_asert_asert,
    theta,
    h_variable,
    horizon = 100,
    currency_base = "BTC",
    keep_reward = TRUE,
    anchor
  )
}
)

time

cat("expected time of one loop is", horizon / 100 * time["elapsed"])


# perform simulation ------------------------------------------------------

foreach::foreach(i = 1:iter,
                 .packages = c(
                   "BlockChain",
                    "dplyr",
                    "magrittr"
                   ),
                 .export = ls(envir=parent.frame())
                 ) %dopar% {

  if (setting == "actual") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "actual",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_actual,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_actual_", 
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "original_original_cw144") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "original_original_cw144",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_original_original_cw144,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_original_original_cw144_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "original_original_original") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "original_original_original",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_original_original_original,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_original_original_original_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "cw144_cw144_cw144"){
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "cw144_cw144_cw144",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_cw144_cw144_cw144,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_cw144_cw144_cw144_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "original_asert_cw144") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "original_asert_cw144",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_original_asert_cw144,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_original_asert_cw144_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "original_asert_asert") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "original_asert_asert",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_original_asert_asert,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_original_asert_asert_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "asert_asert_asert") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "asert_asert_asert",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_asert_asert_asert,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_asert_asert_asert_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "cw144_asert_asert") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "cw144_asert_asert",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_cw144_asert_asert,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_cw144_asert_asert_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else if (setting == "asert_asert_cw144") {
    
    epoch_currency_header <-
      epoch_currency_header %>% 
      dplyr::mutate(
        setting = "asert_asert_cw144",
        trial = i 
      )
    
    epoch_currency <-
      simulate_epoch_stochastic_reduced(
        epoch_currency = epoch_currency_header,
        daa = daa_asert_asert_cw144,
        theta,
        h_variable,
        horizon,
        currency_base,
        keep_reward = TRUE,
        anchor
      )
    mypath <- paste("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_asert_asert_cw144_",
                    i,
                    ".rds",
                    sep = "") 
    saveRDS(epoch_currency, file = mypath)
    
  } else {
    stop("setting is invalid")
  }
  
}


