# converts bits to hex integer
convert_bits <-
  function(bits) {
    a <- substring(bits, 1, 2)
    b <- substring(bits, 3, nchar(bits))
    a <- as.integer(as.hexmode(a))
    b <- as.integer(as.hexmode(b))
    h <- b * 2 ^ (8 * (a - 3))
    return(h)
  }

# convert bits to target
# reference: https://gist.github.com/wakiyamap/f1a0dd0e2b160cd7f2e8c5c74b05dbb2
bits_to_target <-
  function(bits) {
    MM <- 16777216
    a <- bits %% MM
    if (a < 0x8000) {
      a <- a * 256
    }
    target <- a * (2 ** (8 * (bits %/% MM - 3)))
    return(target)
  }

# convert target to bits
# reference: https://gist.github.com/wakiyamap/f1a0dd0e2b160cd7f2e8c5c74b05dbb2
target_to_bits <-
  function(target) {
    MM <- 16777216
    # cc <- as.hexmode(as.integer(target)) # do not work with a large integer
    cc <- gmp::as.bigz(target) %>% as.character(b = 16)
    cc_width <- max(64, nchar(cc))
    cc <- substring(format(cc, width = cc_width), 3, cc_width)
    i <- 31
    while (substring(cc, 1, 2) == "00") {
      cc <- substring(cc, 3, nchar(cc))
      i <- i - 1
    }
    cc <- strtoi(paste0("0x", substr(cc, 1, 6)), base = 16)
    if (cc >= 0x800000) {
      cc <- cc %/% 256
      i <- i + 1
    }
    bits <- cc + MM * i
    return(bits)
  }

# preclean currency output
preclean_currency_output <-
  function (file, frequency) {
    tryCatch({
      # read data
      currency_output <- readr::read_csv(file)
      currency_output <- currency_output %>%
        dplyr::filter(!is.na(bits))
      
      # append updates
      if (grepl("bitcoin_output", file)) {
        file_update <-
          list.files(path = "input/raw_data_final/data",
                     pattern = "bitcoin_output_update.*csv",
                     full.names = TRUE)
        currency_output_update <-
          readr::read_csv(file_update) %>%
          dplyr::select(-`...1`)
        currency_output_update <-
          currency_output_update %>%
          dplyr::filter(!is.na(bits)) %>%
          dplyr::filter(!(blockheight %in% currency_output$blockheight))
        currency_output <-
          currency_output %>%
          dplyr::bind_rows(currency_output_update)
      } else if (grepl("bitcoinCash_output", file)) {
        file_update <-
          list.files(path = "input/raw_data_final/data",
                     pattern = "bitcoinCash_output_update.*csv",
                     full.names = TRUE)
        currency_output_update <-
          readr::read_csv(file_update) %>%
          dplyr::select(-`...1`)
        currency_output_update <-
          currency_output_update %>%
          dplyr::filter(!is.na(bits)) %>%
          dplyr::filter(!(blockheight %in% currency_output$blockheight))
        currency_output <-
          currency_output %>%
          dplyr::bind_rows(currency_output_update)
      } else if (grepl("bitcoinSV_output", file)) {
        file_update <-
          list.files(path = "input/raw_data_final/data",
                     pattern = "bitcoinSV_output_update.*csv",
                     full.names = TRUE)
        currency_output_update <-
          readr::read_csv(file_update) %>%
          dplyr::select(-`...1`)
        currency_output_update <-
          currency_output_update %>%
          dplyr::filter(!is.na(bits)) %>%
          dplyr::filter(!(blockheight %in% currency_output$blockheight))
        currency_output <-
          currency_output %>%
          dplyr::bind_rows(currency_output_update)
      }
      
      # currency symbol
      if (grepl("ethereum_output", file)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(currency = "ETH") %>%
          dplyr::rename(time = timestamp,
                        blockheight = number) %>%
          dplyr::filter(blockheight > 0)
      } else if (grepl("ethereumClassic_output", file)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(currency = "ETC") %>%
          dplyr::rename(time = timestamp,
                        blockheight = number) %>%
          dplyr::filter(blockheight > 0)
      } else if (grepl("monero_output", file)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(currency = "XMR")
      } else if (grepl("siacoin_output", file)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(currency = "SC")
      }
      
      # sort data at the currency-algo level
      currency_output <-
        currency_output %>%
        dplyr::arrange(currency, algo, time, blockheight)
      
      # convert unix time to datetime and date
      currency_output <-
        currency_output %>%
        dplyr::mutate(
          datetime = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
          date = lubridate::as_date(datetime)
        )
      
      dplyr::select(currency_output, blockheight, time, datetime, date)
      
      # deal with the hardfork
      if (grepl("bitcoinCash", file)) {
        # Forked from BTC at block 478558.
        # No change in the hash algorithm.
        # 1 August 2017.
        # 1 BCH for 1 BTC
        currency_output <-
          currency_output %>%
          dplyr::filter(blockheight >= 478558)
      }
      if (grepl("bitcoinDiamond", file)) {
        # Forked from BTC at block 495866.
        # THe hash algorithm changed from sha56 to x13 at xxxx.
        currency_output <-
          currency_output %>%
          dplyr::filter(blockheight >= 495866)
      }
      if (grepl("bitcoinGold", file)) {
        # Forked from BTC at block 491407.
        # 24 October 2017.
        # 1 BTG for 1 BTC.
        currency_output <-
          currency_output %>%
          dplyr::filter(blockheight >= 491407)
      }
      if (grepl("bitcoinSV", file)) {
        # Forked from BCH at block 556766.
        # 15 November 2018.
        # 1 BSV for 1 BCH.
        currency_output <-
          currency_output %>%
          dplyr::filter(blockheight >= 556766)
      }
      if (grepl("ethereumClassic_output", file)) {
        # Forked from ETH at block 1920000
        # July 20 2016
        currency_output <-
          currency_output %>%
          dplyr::filter(blockheight >= 1920000)
      }
      
      # calculate the target from the bits
      if (grepl("ethereum_output", file) |
          grepl("ethereumClassic_output", file)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(winning_rate = 1 / difficulty)
      } else {
        currency_output <-
          currency_output %>%
          dplyr::rowwise() %>%
          dplyr::mutate(winning_rate = convert_bits(bits) / 2 ^ 256) %>%
          dplyr::ungroup()
      }
      
      # calculate blockhash
      if ("blockhash" %in% colnames(currency_output)) {
        currency_output <-
          currency_output %>%
          dplyr::mutate(
            blockhash = Rmpfr::mpfr(blockhash, prec = 256, base = 16) %>% as.numeric(),
            blockhash = blockhash / 2 ^ 256
          )
      } else {
        currency_output <-
          currency_output %>%
          dplyr::mutate(blockhash = NA)
      }
      
      # calculate the block time for each currency-algo
      currency_output <-
        currency_output %>%
        dplyr::group_by(currency, algo) %>%
        dplyr::mutate(block_time = datetime - dplyr::lag(datetime)) %>%
        dplyr::mutate(block_time = as.integer(block_time)) %>%
        tidyr::fill(block_time, .direction = "up") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(block_time = ifelse(block_time == 0, 0.5, block_time))
      
      # calculate the inverse hash rate
      currency_output <-
        currency_output %>%
        dplyr::mutate(inverse_hash_rate = winning_rate * block_time)
      
      # aggregate at the date level
      if (frequency == "blockheight") {
        currency_output <-
          currency_output %>%
          dplyr::mutate(hash_rate = 1 / inverse_hash_rate) %>%
          dplyr::select(
            currency,
            algo,
            blockheight,
            datetime,
            winning_rate,
            block_time,
            inverse_hash_rate,
            hash_rate,
            blockhash,
            bits
          )
      } else if (frequency == "date") {
        currency_output <-
          currency_output %>%
          dplyr::group_by(currency, algo, date) %>%
          dplyr::mutate(
            blockheight = min(blockheight, na.rm = TRUE),
            hash_rate_sd = sd(1 / inverse_hash_rate, na.rm = TRUE),
            block_time_sd = sd(block_time, na.rm = TRUE),
            winning_rate = mean(winning_rate),
            block_time = mean(block_time),
            inverse_hash_rate = mean(inverse_hash_rate)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::distinct(currency, algo, date, .keep_all = TRUE) %>%
          dplyr::mutate(hash_rate = 1 / inverse_hash_rate) %>%
          dplyr::select(
            currency,
            algo,
            date,
            blockheight,
            winning_rate,
            block_time,
            inverse_hash_rate,
            hash_rate,
            hash_rate_sd,
            block_time_sd
          )
        
        # make index
        currency_output <-
          currency_output %>%
          dplyr::group_by(currency) %>%
          tidyr::complete(tidyr::nesting(currency, algo), date = seq(min(date), max(date), by = 1)) %>%
          dplyr::ungroup()
        currency_output <-
          currency_output %>%
          dplyr::group_by(currency, algo) %>%
          tidyr::fill(
            winning_rate,
            block_time,
            inverse_hash_rate,
            hash_rate,
            hash_rate_sd,
            block_time_sd,
            .direction = "down"
          ) %>%
          dplyr::ungroup()
      } else if (frequency == "week") {
        currency_output <-
          currency_output %>%
          dplyr::mutate(week = lubridate::floor_date(date, unit = "week")) %>%
          dplyr::group_by(currency, algo, week) %>%
          dplyr::mutate(
            hash_rate_sd = sd(1 / inverse_hash_rate, na.rm = TRUE),
            block_time_sd = sd(block_time, na.rm = TRUE),
            winning_rate = mean(winning_rate),
            block_time = mean(block_time),
            inverse_hash_rate = mean(inverse_hash_rate)
          ) %>%
          dplyr::ungroup() %>%
          dplyr::distinct(currency, algo, week, .keep_all = TRUE) %>%
          dplyr::mutate(hash_rate = 1 / inverse_hash_rate) %>%
          dplyr::select(
            currency,
            algo,
            week,
            winning_rate,
            block_time,
            inverse_hash_rate,
            hash_rate,
            hash_rate_sd,
            block_time_sd
          )
        
        # make index
        currency_output <-
          currency_output %>%
          dplyr::group_by(currency) %>%
          tidyr::complete(tidyr::nesting(currency, algo), week = seq(min(week), max(week), by = 7)) %>%
          dplyr::ungroup()
        currency_output <-
          currency_output %>%
          dplyr::group_by(currency, algo) %>%
          tidyr::fill(
            winning_rate,
            block_time,
            inverse_hash_rate,
            hash_rate,
            hash_rate_sd,
            block_time_sd,
            .direction = "down"
          ) %>%
          dplyr::ungroup()
      } else {
        stop("frequency is invalid")
      }
      
      # result
      output <-
        list(currency_output = currency_output,
             result = "success")
      
    }, error = function(e) {
      # error
      output <-
        list(currency_output = NULL,
             result = file)
    })
    
    return(output)
    
  }

# make baseline data for computing utility and choice probability
make_baseline_data_asic <-
  function(spec, currency_algo_date, currency_base) {
    # expand asic spec vector for top currencies
    algo_date <-
      spec %>%
      tidyr::expand(tidyr::nesting(model, algo),
                    date = seq(min(release), to = as.Date("2020-12-31"), by = 1)) %>%
      dplyr::left_join(spec,
                       by = c("model", "algo", "date" = "release")) %>%
      dplyr::group_by(model) %>%
      tidyr::fill(dplyr::everything(), .direction = "down") %>%
      dplyr::ungroup()
    # make baseline data
    # complete in asic x (currency x algo x date)
    # include NA
    base_currency_algo_date <-
      currency_algo_date %>%
      dplyr::filter(asic_dummy)
    base_currency_algo_date <-
      base_currency_algo_date %>%
      tidyr::expand(model = unique(algo_date$model),
                    tidyr::nesting(currency, algo, date)) %>%
      dplyr::left_join(algo_date,
                       by = c("model", "algo", "date")) %>%
      dplyr::filter(!is.na(hash_rate_spec)) %>%
      dplyr::left_join(currency_algo_date,
                       by = c("currency", "algo", "date")) %>%
      dplyr::left_join(spec %>%
                         dplyr::select(model, release),
                       by = "model")
    base_currency_algo_date <-
      base_currency_algo_date %>%
      dplyr::mutate(currency_base = (currency == currency_base))
    return(base_currency_algo_date)
  }

make_baseline_data_gpu <-
  function(spec, currency_algo_date) {
    # expand asic spec vector for top currencies
    algo_date <-
      spec %>%
      tidyr::expand(tidyr::nesting(model),
                    date = seq(min(release), to = as.Date("2020-12-31"), by = 1)) %>%
      dplyr::left_join(spec,
                       by = c("model", "date" = "release")) %>%
      dplyr::group_by(model) %>%
      tidyr::fill(dplyr::everything(), .direction = "down") %>%
      dplyr::ungroup()
    # make baseline data
    # complete in asic x (currency x algo x date)
    # include NA
    base_currency_algo_date <-
      currency_algo_date %>%
      dplyr::filter(!asic_dummy)
    base_currency_algo_date <-
      base_currency_algo_date %>%
      tidyr::expand(model = unique(algo_date$model),
                    tidyr::nesting(currency, algo, date)) %>%
      dplyr::left_join(algo_date,
                       by = c("model", "date")) %>%
      dplyr::filter(!is.na(hash_rate_spec)) %>%
      dplyr::left_join(currency_algo_date,
                       by = c("currency", "algo", "date")) %>%
      dplyr::left_join(spec %>%
                         dplyr::select(model, release),
                       by = "model")
    return(base_currency_algo_date)
  }

# compute choice probability
compute_choice_probability <-
  function(base_currency_algo_date,
           sigma,
           electricity_cost,
           tax,
           xi) {
    # compute mean utility of mining
    df <-
      base_currency_algo_date %>%
      dplyr::left_join(tax, by = "currency") %>%
      dplyr::left_join(xi, by = "currency") %>%
      dplyr::mutate(
        xi = ifelse(currency_base, 0, xi),
        mean_utility = hash_rate_spec * winning_rate * reward_quoteusd * (1 - tax) - power * electricity_cost + xi
      )
    
    # compute choice probability
    df <-
      df %>%
      dplyr::group_by(model, date) %>%
      dplyr::mutate(choice_probability = exp(mean_utility / sigma) / (1 + sum(exp(
        mean_utility / sigma
      ), na.rm = TRUE))) %>%
      dplyr::ungroup()
    
    # return
    return(df)
  }

# compute choice probability for sha256
compute_choice_probability_sha256 <-
  function(base_currency_algo_date,
           sigma,
           electricity_cost,
           war,
           xi,
           liquidity) {
    # compute mean utility of mining
    df <-
      base_currency_algo_date %>%
      dplyr::left_join(war, by = "currency") %>%
      dplyr::left_join(xi, by = "currency") %>%
      dplyr::left_join(liquidity, by = "currency") %>%
      dplyr::mutate(
        xi = ifelse(currency_base, 0, xi),
        mean_utility = hash_rate_spec * winning_rate * reward_quoteusd - power * electricity_cost + xi,
        mean_utility = mean_utility + war * (date >= "2018-11-15") * (date <= "2018-11-26"),
        mean_utility = mean_utility + liquidity * log(volume)
      )
    
    # compute choice probability
    df <-
      df %>%
      dplyr::group_by(model, date) %>%
      dplyr::mutate(choice_probability = exp(mean_utility / sigma) / (1 + sum(exp(
        mean_utility / sigma
      ), na.rm = TRUE))) %>%
      dplyr::ungroup()
    
    # return
    return(df)
  }

# fit diffusion
fit_diffusion <-
  function(t, speed) {
    q <- speed * log(1 + t)
    return(q)
  }

# predict hashrate per machine per day
predict_hash <-
  function(base_currency_algo_date,
           parameters,
           electricity_cost,
           algo = "sha256",
           fixed = NA) {
    # dependent on algo -------------------------------------------------------
    
    # compute choice probability
    if (algo == "sha256") {
      # extract parameters
      sigma <- parameters$sigma
      speed <- parameters$speed
      war <- parameters$war
      xi <- parameters$xi
      liquidity <- parameters$liquidity
      
      # compute choice probability
      df_hash_predicted <-
        compute_choice_probability_sha256(
          base_currency_algo_date = base_currency_algo_date,
          sigma = sigma,
          electricity_cost = electricity_cost,
          war = war,
          xi = xi,
          liquidity = liquidity
        )
    } else {
      # extract parameters
      sigma <- parameters$sigma
      speed <- parameters$speed
      tax <- parameters$tax
      xi <- parameters$xi
      df_hash_predicted <-
        compute_choice_probability(
          base_currency_algo_date = base_currency_algo_date,
          sigma = sigma,
          electricity_cost = electricity_cost,
          tax = tax,
          xi = xi
        )
    }
    
    # independent of algo -----------------------------------------------------
    
    # calculate hashrate per machine
    df_hash_predicted <-
      df_hash_predicted %>%
      dplyr::mutate(hash_rate_per_machine = hash_rate_spec * choice_probability)
    
    # guess machine allocation
    df_hash_predicted <-
      df_hash_predicted %>%
      dplyr::left_join(speed,
                       by = "model")
    
    # predict diffusion
    if (is.na(fixed)) {
      if ("datetime" %in% colnames(df_hash_predicted)) {
        df_hash_predicted <-
          df_hash_predicted %>%
          dplyr::mutate(
            t = difftime(datetime, release, units = "secs") %>%
              as.numeric(),
            t = t / (24 * 3600)
          ) %>%
          dplyr::mutate(q = fit_diffusion(t, speed))
      } else {
        df_hash_predicted <-
          df_hash_predicted %>%
          dplyr::mutate(t = difftime(date, release, units = "days") %>%
                          as.integer()) %>%
          dplyr::mutate(q = fit_diffusion(t, speed))
      }
    } else {
      df_hash_predicted <-
        df_hash_predicted %>%
        dplyr::filter(release < fixed) %>%
        dplyr::mutate(
          t = difftime(fixed, release, units = "secs") %>%
            as.numeric(),
          t = t / (24 * 3600)
        ) %>%
        dplyr::mutate(q = fit_diffusion(t, speed))
    }
    
    # predict hash supply
    df_hash_predicted <-
      df_hash_predicted %>%
      dplyr::mutate(hash_rate_predicted = q * hash_rate_per_machine)
    
    # aggregate at the currency-algo level
    df_hash_predicted <-
      df_hash_predicted %>%
      dplyr::group_by(currency, algo, date) %>%
      dplyr::mutate(hash_rate_predicted = sum(hash_rate_predicted)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(currency, algo, date, .keep_all = TRUE) %>%
      dplyr::select(currency, algo, date, hash_rate, hash_rate_predicted)
    
    # return
    return(df_hash_predicted)
    
  }

# bundle parameters to theta
bundle_theta <-
  function(parameters, algo = "sha256") {
    if (algo == "sha256") {
      theta <-
        c(
          parameters$sigma,
          parameters$speed %>%
            dplyr::mutate(speed = log(speed)) %>%
            dplyr::pull(speed),
          parameters$war %>%
            dplyr::pull(war) ,
          parameters$xi %>%
            dplyr::pull(xi),
          parameters$liquidity %>%
            dplyr::pull(liquidity)
        )
      names(theta) <-
        c(
          "sigma",
          paste("speed", parameters$speed$model, sep = "_"),
          paste("war", parameters$war$currency, sep = "_"),
          paste("xi", parameters$xi$currency, sep = "_"),
          paste("liquidity", parameters$liquidity$currency, sep = "_")
        )
    } else {
      theta <-
        c(
          parameters$sigma,
          parameters$speed %>%
            dplyr::mutate(speed = log(speed)) %>%
            dplyr::pull(speed),
          parameters$tax %>%
            dplyr::mutate(tax = log(tax / (1 - tax))) %>%
            dplyr::pull(tax) ,
          parameters$xi %>%
            dplyr::pull(xi)
        )
    }
    
    return(theta)
  }


# unbundle theta to parameters
unbundle_theta <-
  function(theta,
           parameters,
           algo = "sha256") {
    if (algo == "sha256") {
      sigma <- theta[1]
      parameters$sigma <- sigma
      start <- 2
      end <- 1 + nrow(parameters$speed)
      speed <- theta[start:end]
      speed <- exp(speed)
      parameters$speed$speed <- speed
      start <- end + 1
      end <- end + nrow(parameters$war)
      war <- theta[start:end]
      parameters$war$war <- war
      start <- end + 1
      end <- end + nrow(parameters$xi)
      parameters$xi$xi <- theta[start:end]
      start <- end + 1
      end <- end + nrow(parameters$liquidity)
      parameters$liquidity$liquidity <- theta[start:end]
    } else {
      sigma <- theta[1]
      start <- 2
      end <- 1 + nrow(parameters$speed)
      speed <- theta[start:end]
      speed <- exp(speed)
      parameters$speed$speed <- speed
      start <- end + 1
      end <- end + nrow(parameters$tax)
      tax <- theta[start:end]
      tax <- exp(tax) / (1 + exp(tax))
      parameters$tax$tax <- tax
      start <- end + 1
      end <- end + nrow(parameters$xi)
      parameters$xi$xi <- theta[start:end]
    }
    return(parameters)
  }


# compute objective function of the minium distance estimator
compute_objective_minimum_distance_estimator <-
  function(theta,
           base_currency_algo_date,
           parameters,
           electricity_cost,
           algo = "sha256") {
    parameters <- unbundle_theta(theta, parameters, algo)
    df_hash_predicted <-
      predict_hash(base_currency_algo_date,
                   parameters,
                   electricity_cost,
                   algo)
    distance <-
      (log(df_hash_predicted$hash_rate) - log(df_hash_predicted$hash_rate_predicted)) ^
      2
    distance <- mean(distance) / 2
    return(distance)
  }

# estimate the parameters
estimate_parameters_minimum_distance_estimator <-
  function(base_currency_algo_date,
           parameters,
           electricity_cost) {
    theta <- bundle_theta(parameters = parameters)
    result <-
      optim(
        par = theta,
        fn = compute_objective_minimum_distance_estimator,
        method = "BFGS",
        control = list(trace = 3),
        base_currency_algo_date = base_currency_algo_date,
        parameters = parameters,
        electricity_cost = electricity_cost
      )
    return(result)
  }

# compute objective function of the maximum likelihood estimator
compute_loglikelihood <-
  function(theta,
           base_currency_algo_date,
           epoch_currency,
           parameters,
           electricity_cost,
           algo = "sha256") {
    parameters <- unbundle_theta(theta, parameters, algo)
    df_hash_predicted <-
      predict_hash(base_currency_algo_date,
                   parameters,
                   electricity_cost,
                   algo)
    loglikelihood <-
      epoch_currency %>%
      dplyr::left_join(
        df_hash_predicted %>%
          dplyr::select(currency, algo, date, hash_rate_predicted),
        by = c("currency", "algo", "date")
      ) %>%
      dplyr::summarise(
        loglikelihood = -sum(winning_rate * hash_rate_predicted * epoch_time) + sum(generated * log(winning_rate * hash_rate_predicted))
      ) %>%
      dplyr::pull(loglikelihood)
    
    return(loglikelihood)
  }

# make baseline data for computing utility and choice probability
make_base_epoch_curency <-
  function(spec,
           epoch_currency,
           currency_base,
           rate_estimate) {
    # expand asic spec vector for top currencies
    models <-
      spec %>%
      dplyr::filter(algo %in% unique(epoch_currency$algo)) %>%
      dplyr::pull(model)
    
    # make baseline data
    # complete in asic x (currency x algo x date)
    # include NA
    base_epoch_currency <-
      epoch_currency %>%
      tidyr::expand(model = models,
                    tidyr::nesting(currency, algo, epoch)) %>%
      dplyr::left_join(epoch_currency,
                       by = c("currency", "algo", "epoch")) %>%
      dplyr::left_join(spec %>%
                         dplyr::select(-algo),
                       by = "model") %>%
      dplyr::mutate(date = lubridate::as_date(datetime)) %>%
      dplyr::filter(date >= release,!is.na(hash_rate_spec))
    base_epoch_currency <-
      base_epoch_currency %>%
      dplyr::mutate(currency_base = (currency == currency_base))
    
    base_epoch_currency <-
      base_epoch_currency %>%
      dplyr::left_join(rate_estimate,
                       by = "currency")
    return(base_epoch_currency)
  }

# update hash rate
update_hash_rate <-
  function(epoch_currency_n,
           base_epoch_currency_n,
           spec,
           parameters,
           electricity_cost,
           algo = "sha256",
           fixed = NA) {
    # predict hash rate
    df_hash_n <-
      predict_hash(
        base_currency_algo_date = base_epoch_currency_n,
        parameters = parameters,
        electricity_cost = electricity_cost,
        algo = algo,
        fixed = fixed
      ) %>%
      dplyr::select(-hash_rate,-algo,-date) %>%
      dplyr::rename(hash_rate = hash_rate_predicted)
    
    # update hash rate
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::select(-hash_rate) %>%
      dplyr::left_join(df_hash_n,
                       by = "currency") %>%
      dplyr::mutate(inverse_hash_rate = 1 / hash_rate)
    
    return(epoch_currency_n)
  }

# update epoch time
update_epoch_time_stochastic <-
  function(epoch_currency_n) {
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::select(-epoch_time) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(epoch_time = rexp(n = 1, rate = hash_rate * winning_rate)) %>%
      dplyr::ungroup()
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::mutate(
        generated = (epoch_time == min(epoch_time)),
        blockheight = blockheight + generated,
        blockhash = generated * runif(1, min = 0, max = winning_rate) + (1 - generated) * blockhash,
        epoch_time = min(epoch_time),
        datetime = datetime + epoch_time,
        epoch = epoch + 1
      )
    return(epoch_currency_n)
  }

estimate_exogenous <-
  function(rate,
           distr,
           start) {
    # take difference
    rate_estimate <-
      rate %>%
      tidyr::pivot_longer(cols = c(rate_quoteusd, volume)) %>%
      dplyr::mutate(value = log(value)) %>%
      dplyr::arrange(currency, name, date) %>%
      dplyr::group_by(currency, name) %>%
      dplyr::mutate(value = value - dplyr::lag(value)) %>%
      dplyr::filter(is.finite(value)) %>%
      dplyr::ungroup()
    
    # standardize
    rate_estimate <-
      rate_estimate %>%
      dplyr::group_by(currency, name) %>%
      dplyr::mutate(
        mean = mean(value),
        sd = sd(value),
        value = (value - mean) / sd
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_split(currency, name, .keep = TRUE)
    
    # fit
    rate_fit_tdistribution <-
      rate_estimate %>%
      purrr::map(.,
                 ~ fitdistrplus::fitdist(
                   data = .$value,
                   distr = distr,
                   start = start
                 ))
    
    # summarize
    rate_estimate <-
      rate_estimate %>%
      purrr::map(
        .,
        ~ dplyr::distinct(., currency, name, .keep_all = TRUE) %>%
          dplyr::select(currency, name, mean, sd)
      ) %>%
      purrr::map2(.,
                  rate_fit_tdistribution,
                  ~ tibble::tibble(.x,
                                   df = .y$estimate,
                                   se = .y$sd)) %>%
      dplyr::bind_rows()
    
    return(rate_estimate)
    
  }

# update exogenous state variables
update_exogenous_stochastic <-
  function(base_epoch_currency_n) {
    # select relevant variables
    exogenous_n <-
      base_epoch_currency_n %>%
      dplyr::select(
        currency,
        epoch,
        dplyr::starts_with("rate_quoteusd"),
        dplyr::starts_with("volume"),
        epoch_time
      ) %>%
      dplyr::distinct(currency, epoch, .keep_all = TRUE)
    
    # draw rate and volume
    exogenous_n <-
      exogenous_n %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        rate_quoteusd_change = rt(1, df = rate_quoteusd_df),
        volume_change = rt(1, df = volume_df)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        rate_quoteusd_change = (rate_quoteusd_mean + rate_quoteusd_change * rate_quoteusd_sd) * epoch_time / (24 * 3600),
        volume_change = (volume_mean + volume_change * volume_sd) * epoch_time / (24 * 3600)
      ) %>%
      dplyr::select(currency, epoch, rate_quoteusd_change, volume_change)
    
    # update rate and volume
    base_epoch_currency_n <-
      base_epoch_currency_n %>%
      dplyr::left_join(exogenous_n,
                       by = c("currency", "epoch")) %>%
      dplyr::mutate(
        rate_quoteusd = rate_quoteusd * exp(rate_quoteusd_change),
        volume = volume * exp(volume_change)
      ) %>%
      dplyr::select(-rate_quoteusd_change,-volume_change)
    
    # update reward_quoteusd
    base_epoch_currency_n <-
      base_epoch_currency_n %>%
      dplyr::mutate(reward_quoteusd = reward * rate_quoteusd)
    
    return(base_epoch_currency_n)
  }

# update winning rate
update_winning_rate_bitcoin_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # is mod 0?
    if ((mod(blockheight_n, 2016) == 0) & generated_n) {
      # check the currency history
      epoch_currency_history <-
        epoch_currency %>%
        dplyr::filter(currency == currency_n) %>%
        dplyr::arrange(epoch) %>%
        dplyr::filter(epoch <= epoch_n)
      
      # slice relevant epoch
      epoch_currency_history <-
        epoch_currency_history %>%
        dplyr::mutate(blockheight_last = ifelse(
          max(blockheight) - 2016 >= min(blockheight),
          max(blockheight) - 2016,
          min(blockheight)
        )) %>%
        dplyr::filter(blockheight == max(blockheight) |
                        blockheight == blockheight_last) %>%
        dplyr::filter(epoch == max(epoch) |
                        epoch == min(epoch))
      
      # calculate the elapsed time
      elapsed_time <-
        epoch_currency_history %>%
        dplyr::summarise(
          elapsed_time = difftime(max(datetime), min(datetime), units = "secs"),
          elapsed_time = as.integer(elapsed_time)
        ) %>%
        dplyr::pull(elapsed_time)
      
      # update winning rate
      winning_rate_hat <-
        (elapsed_time) / (2016 * 10 * 60) * winning_rate_n
      
      winning_rate_n <-
        max(c(min(
          c(winning_rate_hat, 4 * winning_rate_n)
        ),
        1 / 4 * winning_rate_n))
    }
    
    return(winning_rate_n)
  }

# update winning rate
update_winning_rate_bitcoin_cash_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    target_spacing <- 600
    # is mod 0?
    if (generated_n) {
      # check the currency history
      epoch_currency_history <-
        epoch_currency %>%
        dplyr::filter(currency == currency_n) %>%
        dplyr::filter(generated) %>%
        dplyr::arrange(epoch) %>%
        dplyr::filter(epoch <= epoch_n)
      
      # Special difficulty rule for testnet:
      # If the new block's timestamp is more than 2* 10 minutes then allow
      # mining of a min-difficulty block.
      
      # calculate blockheight_first and blockheight_last
      blockheight_first <-
        epoch_currency_history %>%
        dplyr::filter(blockheight >= (blockheight_n - 147),
                      blockheight <= (blockheight_n - 145))
      if (blockheight_first[1, "datetime"] > blockheight_first[3, "datetime"]) {
        blockheight_first <-
          dplyr::bind_rows(blockheight_first[3,],
                           blockheight_first[2,],
                           blockheight_first[1,])
      }
      if (blockheight_first[1, "datetime"] > blockheight_first[2, "datetime"]) {
        blockheight_first <-
          dplyr::bind_rows(blockheight_first[2,],
                           blockheight_first[1,],
                           blockheight_first[3,])
      }
      if (blockheight_first[2, "datetime"] > blockheight_first[3, "datetime"]) {
        blockheight_first <-
          dplyr::bind_rows(blockheight_first[1,],
                           blockheight_first[3,],
                           blockheight_first[2,])
      }
      blockheight_first <-
        blockheight_first[2, "blockheight"] %>% as.integer()
      
      blockheight_last <-
        epoch_currency_history %>%
        dplyr::filter(blockheight >= (blockheight_n - 3),
                      blockheight <= (blockheight_n - 1))
      if (blockheight_last[1, "datetime"] > blockheight_last[3, "datetime"]) {
        blockheight_last <-
          dplyr::bind_rows(blockheight_last[3,],
                           blockheight_last[2,],
                           blockheight_last[1,])
      }
      if (blockheight_last[1, "datetime"] > blockheight_last[2, "datetime"]) {
        blockheight_last <-
          dplyr::bind_rows(blockheight_last[2,],
                           blockheight_last[1,],
                           blockheight_last[3,])
      }
      if (blockheight_last[2, "datetime"] > blockheight_last[3, "datetime"]) {
        blockheight_last <-
          dplyr::bind_rows(blockheight_last[1,],
                           blockheight_last[3,],
                           blockheight_last[2,])
      }
      blockheight_last <-
        blockheight_last[2, "blockheight"] %>% as.integer()
      
      # calculate the sum of difficulty
      difficulty_sum <-
        epoch_currency_history %>%
        dplyr::filter(blockheight >= blockheight_first,
                      blockheight <= blockheight_last) %>%
        dplyr::summarise(difficulty = sum(1 / winning_rate)) %>%
        dplyr::pull(difficulty)
      
      # calculate the elapsed time
      datetime_first <-
        epoch_currency_history %>%
        dplyr::filter(blockheight == blockheight_first) %>%
        dplyr::pull(datetime) %>%
        median()
      datetime_last <-
        epoch_currency_history %>%
        dplyr::filter(blockheight == blockheight_last) %>%
        dplyr::pull(datetime) %>%
        median()
      elapsed_time <-
        difftime(datetime_last, datetime_first, units = "secs") %>%
        as.integer()
      
      # adjust the elapsed time
      elapsed_time <-
        max(min(elapsed_time,
                288 * target_spacing),
            72 * target_spacing)
      
      # update winning rate
      winning_rate_n <-
        elapsed_time / (target_spacing * difficulty_sum)
      
    }
    
    return(winning_rate_n)
  }

update_winning_rate <-
  function(epoch_currency_n,
           epoch_currency,
           daa,
           anchor = NULL) {
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # BTC
        winning_rate =
          ifelse(
            currency == "BTC",
            daa$BTC(
              epoch_n = epoch,
              currency_n = currency,
              blockheight_n = blockheight,
              datetime_n = datetime,
              winning_rate_n = winning_rate,
              generated_n = generated,
              epoch_currency = epoch_currency,
              anchor
            ),
            winning_rate
          ),
        # BCH
        winning_rate =
          ifelse(
            currency == "BCH",
            daa$BCH(
              epoch_n = epoch,
              currency_n = currency,
              blockheight_n = blockheight,
              datetime_n = datetime,
              winning_rate_n = winning_rate,
              generated_n = generated,
              epoch_currency = epoch_currency,
              anchor
            ),
            winning_rate
          ),
        # BSV
        winning_rate =
          ifelse(
            currency == "BSV",
            daa$BSV(
              epoch_n = epoch,
              currency_n = currency,
              blockheight_n = blockheight,
              datetime_n = datetime,
              winning_rate_n = winning_rate,
              generated_n = generated,
              epoch_currency = epoch_currency,
              anchor
            ),
            winning_rate
          )
      ) %>%
      dplyr::ungroup()
    
    return(epoch_currency_n)
  }

# update reward for simulation
update_reward_simulation <-
  function(epoch_currency_n,
           currency_base) {
    if (currency_base == "BTC") {
      epoch_currency_n %>%
        dplyr::mutate(reward = 50 / 2 ^ (blockheight %/% 210000))
      return(epoch_currency_n)
      
    }
    else if (currency_base == "BCH") {
      epoch_currency_n <-
        epoch_currency_n %>%
        dplyr::mutate(reward = dplyr::if_else(currency == "BCH",
                                              50 / 2 ^ (blockheight %/% 210000),
                                              12.5))
      return(epoch_currency_n)
    }
    else if (currency_base == "BSV") {
      epoch_currency_n <-
        epoch_currency_n %>%
        dplyr::mutate(reward = dplyr::if_else(currency == "BTC",
                                              12.5,
                                              50 / 2 ^ (blockheight %/% 210000)))
      return(epoch_currency_n)
    }
  }

# update reward
update_reward <-
  function(epoch_currency_n) {
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::mutate(reward = 50 / 2 ^ (blockheight %/% 210000))
    return(epoch_currency_n)
  }

# update an epoch
update_epoch_stochastic <-
  function(epoch_currency,
           daa,
           parameters,
           spec,
           electricity_cost,
           rate_estimate,
           algo,
           currency_base,
           fixed = NA) {
    # filter the latest epoch
    epoch_currency_n <-
      epoch_currency %>%
      dplyr::filter(epoch == max(epoch))
    
    # update winning rate by passing daa functions
    epoch_currency_n <-
      update_winning_rate(epoch_currency_n,
                          epoch_currency,
                          daa)
    
    # make baseline data for computing utility and choice probability
    base_epoch_currency_n <-
      make_base_epoch_curency(spec,
                              epoch_currency = epoch_currency_n,
                              currency_base,
                              rate_estimate = rate_estimate)
    
    # update exogenous state variables
    base_epoch_currency_n <-
      update_exogenous_stochastic(base_epoch_currency_n)
    
    # update hash rate
    epoch_currency_n <-
      update_hash_rate(
        epoch_currency_n,
        base_epoch_currency_n,
        spec,
        parameters,
        electricity_cost,
        algo,
        fixed = fixed
      )
    
    # update epoch time
    epoch_currency_n <-
      update_epoch_time_stochastic(epoch_currency_n)
    
    # simulated
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::mutate(simulated = TRUE)
    
    # bind new epoch
    epoch_currency <-
      dplyr::bind_rows(epoch_currency,
                       epoch_currency_n)
    return(epoch_currency)
  }

# simulate epoch
simulate_epoch_stochastic <-
  function(horizon,
           epoch_currency,
           daa,
           parameters,
           spec,
           electricity_cost,
           rate_estimate,
           algo,
           currency_base,
           fixed = NA) {
    epoch_currency <-
      epoch_currency %>%
      dplyr::mutate(simulated = FALSE)
    for (t in 1:horizon) {
      epoch_currency <-
        update_epoch_stochastic(
          epoch_currency,
          daa,
          parameters,
          spec,
          electricity_cost,
          rate_estimate,
          algo,
          currency_base,
          fixed = fixed
        )
    }
    epoch_currency <-
      epoch_currency %>%
      dplyr::arrange(epoch, currency)
    return(epoch_currency)
  }

update_winning_rate_bitcoin_cash_emergency_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           winning_rate_n,
           generated_n,
           epoch_currency,
           emergency_daa_start,
           emergency_daa_end,
           anchor = NULL) {
    if (generated_n) {
      # check datetime
      datetime_n <-
        epoch_currency %>%
        dplyr::filter(currency == currency_n,
                      blockheight == blockheight_n,
                      generated) %>%
        dplyr::filter(epoch <= epoch_n) %>%
        dplyr::pull(datetime)
      
      # check the date
      if (datetime_n >= emergency_daa_start &
          datetime_n <= emergency_daa_end)  {
        # check the currency history
        epoch_currency_history <-
          epoch_currency %>%
          dplyr::filter(currency == currency_n) %>%
          dplyr::filter(generated) %>%
          dplyr::arrange(blockheight) %>%
          dplyr::filter(epoch <= epoch_n)
        
        # slice relevant epoch
        datetime_1 <-
          epoch_currency_history %>%
          dplyr::filter(blockheight <= blockheight_n - 1,
                        blockheight >= blockheight_n - 11) %>%
          dplyr::pull(datetime) %>%
          median()
        datetime_2 <-
          epoch_currency_history %>%
          dplyr::filter(blockheight <= blockheight_n - 7,
                        blockheight >= blockheight_n - 17) %>%
          dplyr::pull(datetime) %>%
          median()
        interval_mtp <-
          difftime(datetime_1, datetime_2, units = "secs") %>%
          as.integer()
        
        # update if interval_mtp is more than 12 hours
        if (interval_mtp >= 12 * 3600) {
          winning_rate_n <- winning_rate_n * 5 / 4
        }
      }
    }
    return(winning_rate_n)
  }

# update winning rate
update_winning_rate_bch_actual <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # set dates
    blockheight_bch_start <- 478558
    emergency_daa_end <-
      as.POSIXct(1510600000, origin = "1970-01-01", tz = "UTC")
    
    blockheight_asert <- 661647
    blockheight_anchor <- blockheight_asert
    bits_anchor <- "1804dafe" %>% strtoi(., base = 16)
    # datetime_anchor <- as.POSIXct(1605447844, origin = "1970-01-01", tz = "UTC")
    datetime_anchor <- 1605447844
    
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bch_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BTC") %>%
        dplyr::filter(blockheight < blockheight_bch_start,
                      blockheight >= blockheight_bch_start - 2016) %>%
        dplyr::mutate(currency = "BCH")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply daa
    if (datetime_n <= emergency_daa_end) {
      # apply bitcoin daa
      winning_rate_n <-
        update_winning_rate_bitcoin_daa(
          epoch_n,
          currency_n,
          blockheight_n,
          generated_n,
          winning_rate_n,
          epoch_currency = epoch_currency_check
        )
      
      # apply emergency daa
      winning_rate_n <-
        update_winning_rate_bitcoin_cash_emergency_daa(
          epoch_n,
          currency_n,
          blockheight_n,
          winning_rate_n,
          generated_n,
          epoch_currency = epoch_currency_check,
          emergency_daa_start = blockheight_bch_start,
          emergency_daa_end = emergency_daa_end
        )
    } else if (datetime_n <= datetime_anchor) {
      # apply bitcoin cash daa
      winning_rate_n <-
        update_winning_rate_bitcoin_cash_daa(
          epoch_n,
          currency_n,
          blockheight_n,
          generated_n,
          winning_rate_n,
          epoch_currency = epoch_currency_check
        )
    } else {
      winning_rate_n <-
        update_winning_rate_bitcoin_cash_asert_daa(
          epoch_n,
          currency_n,
          blockheight_n,
          generated_n,
          winning_rate_n,
          epoch_currency = epoch_currency_check,
          blockheight_anchor = blockheight_anchor,
          datetime_anchor = datetime_anchor,
          bits_anchor = bits_anchor
        )
    }
    return(winning_rate_n)
  }

# update winning rate
update_winning_rate_bsv_actual <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # set dates
    blockheight_bsv_start <- 556766
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bsv_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BCH") %>%
        dplyr::filter(blockheight < blockheight_bsv_start,
                      blockheight >= blockheight_bsv_start - 2016) %>%
        dplyr::mutate(currency = "BSV")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply bitcoin cash daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_daa(epoch_n,
                                           currency_n,
                                           blockheight_n,
                                           generated_n,
                                           winning_rate_n,
                                           epoch_currency = epoch_currency_check)
    
    return(winning_rate_n)
  }

update_winning_rate_btc_actual <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           # not used
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    winning_rate_n <-
      update_winning_rate_bitcoin_daa(epoch_n,
                                      currency_n,
                                      blockheight_n,
                                      generated_n,
                                      winning_rate_n,
                                      epoch_currency)
    return(winning_rate_n)
  }

plot_winning_rate_fit <-
  function(epoch_currency_sub) {
    epoch_currency_fit <-
      epoch_currency_sub %>%
      dplyr::arrange(epoch) %>%
      dplyr::mutate(winning_rate = dplyr::lag(winning_rate, 1)) %>%
      dplyr::filter(!is.na(winning_rate)) %>%
      dplyr::group_split(epoch, .keep = TRUE) %>%
      purrr::map(.,
                 ~ update_winning_rate(epoch_currency_n = .,
                                       epoch_currency,
                                       daa)) %>%
      dplyr::bind_rows()
    
    epoch_currency_compare <-
      dplyr::bind_rows(
        epoch_currency_sub %>%
          dplyr::mutate(setting = "data"),
        epoch_currency_fit %>%
          dplyr::mutate(setting = "fit")
      ) %>%
      dplyr::select(blockheight, datetime, setting, winning_rate) %>%
      dplyr::mutate(log_winning_rate = log(winning_rate)) %>%
      dplyr::arrange(setting, blockheight) %>%
      dplyr::group_by(setting) %>%
      dplyr::mutate(
        changed = (winning_rate != dplyr::lag(winning_rate, 1)),
        changed = ifelse(is.na(changed), FALSE, changed)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(blockheight) %>%
      dplyr::mutate(changed = sum(changed * (setting == "data"))) %>%
      dplyr::ungroup()
    
    # make all plot
    all <- list()
    all$point <-
      epoch_currency_compare %>%
      dplyr::filter(!is.na(winning_rate)) %>%
      ggplot(aes(x = datetime,
                 y = log_winning_rate,
                 colour = setting)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic()
    
    all$scatter <-
      epoch_currency_compare %>%
      tidyr::pivot_wider(
        id_cols = c(blockheight, datetime),
        names_from = setting,
        values_from = log_winning_rate
      ) %>%
      dplyr::filter(!is.na(fit)) %>%
      dplyr::mutate(group = "all") %>%
      ggplot(aes(x = data,
                 y = fit,
                 colour = group)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic() +
      theme(legend.position = "none")
    
    all$log_difference <-
      epoch_currency_compare %>%
      tidyr::pivot_wider(
        id_cols = c(blockheight, datetime),
        names_from = setting,
        values_from = log_winning_rate
      ) %>%
      dplyr::filter(!is.na(fit)) %>%
      dplyr::mutate(group = "all") %>%
      dplyr::mutate(log_difference = data - fit) %>%
      ggplot(aes(x = datetime,
                 y = log_difference,
                 colour = group)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic() +
      theme(legend.position = "none")
    
    # make plot when changed
    changed <- list()
    changed$point <-
      epoch_currency_compare %>%
      dplyr::filter(!is.na(winning_rate)) %>%
      dplyr::filter(changed == 1) %>%
      ggplot(aes(x = datetime,
                 y = log_winning_rate,
                 colour = setting)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic()
    
    changed$scatter <-
      epoch_currency_compare %>%
      tidyr::pivot_wider(
        id_cols = c(blockheight, datetime, changed),
        names_from = setting,
        values_from = log_winning_rate
      ) %>%
      dplyr::filter(!is.na(fit)) %>%
      dplyr::filter(changed == 1) %>%
      dplyr::mutate(group = "all") %>%
      ggplot(aes(x = data,
                 y = fit,
                 colour = group)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic() +
      theme(legend.position = "none")
    
    changed$log_difference <-
      epoch_currency_compare %>%
      tidyr::pivot_wider(
        id_cols = c(blockheight, datetime, changed),
        names_from = setting,
        values_from = log_winning_rate
      ) %>%
      dplyr::filter(!is.na(fit)) %>%
      dplyr::filter(changed == 1) %>%
      dplyr::mutate(group = "all") %>%
      dplyr::mutate(log_difference = data - fit) %>%
      ggplot(aes(x = datetime,
                 y = log_difference,
                 colour = group)) +
      geom_point() +
      scale_color_viridis_d() +
      theme_classic() +
      theme(legend.position = "none")
    return(list(all = all,
                changed = changed))
  }

# simulate epoch list
simulate_epoch_list <-
  function(size,
           horizon,
           epoch_currency,
           daa,
           parameters,
           spec,
           electricity_cost,
           rate_estimate,
           algo,
           currency_base,
           fixed = NA) {
    epoch_currency_list <-
      foreach(i = 1:size,
              .packages = c("BlockChain", "magrittr", "foreach")) %dopar% {
                # tryCatch({
                set.seed(i)
                epoch_currency_added <-
                  simulate_epoch_stochastic(
                    horizon,
                    epoch_currency,
                    daa,
                    parameters,
                    spec,
                    electricity_cost,
                    rate_estimate,
                    algo,
                    currency_base,
                    fixed = fixed
                  )
                return(epoch_currency_added)
                # },
                # error = function (e) {
                #   epoch_currency_added <- NULL
                # })
              }
    return(epoch_currency_list)
  }

# update winning rate
update_winning_rate_bch_uses_bch_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # set dates
    blockheight_bch_start <- 478558
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bch_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BTC") %>%
        dplyr::filter(blockheight < blockheight_bch_start,
                      blockheight >= blockheight_bch_start - 2016) %>%
        dplyr::mutate(currency = "BCH")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply bitcoin cash daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_daa(epoch_n,
                                           currency_n,
                                           blockheight_n,
                                           generated_n,
                                           winning_rate_n,
                                           epoch_currency = epoch_currency_check)
    return(winning_rate_n)
  }

update_winning_rate_bch_uses_btc_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # set dates
    blockheight_bch_start <- 478558
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bch_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BTC") %>%
        dplyr::filter(blockheight < blockheight_bch_start,
                      blockheight >= blockheight_bch_start - 2016) %>%
        dplyr::mutate(currency = "BCH")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply bitcoin cash daa
    winning_rate_n <-
      update_winning_rate_bitcoin_daa(epoch_n,
                                      currency_n,
                                      blockheight_n,
                                      generated_n,
                                      winning_rate_n,
                                      epoch_currency = epoch_currency_check)
    return(winning_rate_n)
  }

# update winning rate
update_winning_rate_bsv_uses_btc_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # set dates
    blockheight_bsv_start <- 556766
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bsv_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BCH") %>%
        dplyr::filter(blockheight < blockheight_bsv_start,
                      blockheight >= blockheight_bsv_start - 2016) %>%
        dplyr::mutate(currency = "BSV")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply bitcoin cash daa
    winning_rate_n <-
      update_winning_rate_bitcoin_daa(epoch_n,
                                      currency_n,
                                      blockheight_n,
                                      generated_n,
                                      winning_rate_n,
                                      epoch_currency = epoch_currency_check)
    
    return(winning_rate_n)
  }

# Bitcoin Cash DAA 3 (ASERT)
# reference: https://upgradespecs.bitcoincashnode.org/2020-11-15-asert/#req-asert-target-computation-target-computation
# reference: http://www.cxyzjd.com/article/qq_33401821/111471812
next_target_asert3_2d <-
  function(blockheight_anchor,
           datetime_anchor,
           bits_anchor,
           blockheight_n,
           datetime_n) {
    # constants:
    ideal_block_time <- 600
    halflife <- 172800
    radix <- 65536
    max_bits <- 486604799
    max_target <- bits_to_target(max_bits)
    # precondition:
    target_anchor <- bits_to_target(bits_anchor)
    if ((blockheight_anchor <= blockheight_n &
         blockheight_anchor > 0) &
        (target_anchor > 0 & target_anchor <= max_target)) {
      time_delta <-
        difftime(datetime_n, datetime_anchor, units = "secs") %>%
        as.integer()
      height_delta <-
        blockheight_n - blockheight_anchor
      exponent <-
        ((time_delta - ideal_block_time * (height_delta + 1)) * radix) %/% halflife
      
      num_shifts <-
        exponent / (2 ** 16)
      exponent <-
        exponent - num_shifts * radix
      factor <-
        (195766423245049 * exponent +
           971821376 * exponent ** 2 +
           5127 * exponent ** 3 +
           2 ** 47) / (2 ** 48) + 65536
      next_target <- target_anchor * factor
      
      if (num_shifts < 0) {
        next_target <- next_target / (2 ** (-num_shifts))
      } else {
        next_target <- next_target * (2 ** num_shifts)
      }
      next_target <- next_target / (2 ** 16)
      
      if (next_target == 0) {
        return(target_to_bits(1))
      }
      if (next_target > max_target) {
        return(max_bits)
      }
      return(target_to_bits(next_target))
    } else {
      return(NA)
    }
  }

next_bits_asert3_2d_python <-
  function(blockheight_anchor,
           datetime_anchor,
           bits_anchor,
           blockheight_n,
           datetime_n) {
    time_diff <- datetime_n - datetime_anchor
    height_diff <-
      blockheight_n - blockheight_anchor
    bits <-
      asert$next_bits_aserti3_2d(bits_anchor, time_diff, height_diff)
    return(bits)
  }

update_winning_rate_bitcoin_cash_asert_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           winning_rate_n,
           generated_n,
           epoch_currency,
           blockheight_anchor,
           datetime_anchor,
           bits_anchor) {
    # is mod 0?
    if (generated_n) {
      # check the currency history
      datetime_n <-
        epoch_currency %>%
        dplyr::filter(epoch == epoch_n,
                      currency == currency_n) %>%
        dplyr::pull(datetime)
      datetime_n <- as.numeric(datetime_n)
      
      # apply the asert function
      bits_n <-
        next_bits_asert3_2d_python(blockheight_anchor,
                                   datetime_anchor,
                                   bits_anchor,
                                   blockheight_n,
                                   datetime_n)
      bits_n <- as.hexmode(bits_n)
      # update the winning rate
      winning_rate_n <- convert_bits(bits_n) / 2 ^ 256
      
      ## qichao's code
      # target_n <-
      #   next_target_asert3_2d(
      #   blockheight_anchor,
      #   datetime_anchor,
      #   bits_anchor,
      #   blockheight_n,
      #   datetime_n
      # )
      # winning_rate_n <- target_n / 2^256
      
    }
    return(winning_rate_n)
  }

convert_epoch_currency_list_to_epoch_df <-
  function(epoch_currency_list) {
    setting_path_currency_epoch_df <-
      epoch_currency_list %>%
      purrr::map(
        .,
        ~ purrr::set_names(., 1:length(.)) %>%
          dplyr::bind_rows(., .id = "path") %>%
          dplyr::arrange(path, epoch, currency) %>%
          dplyr::mutate(block_time_fit = 1 / (winning_rate * hash_rate)) %>%
          dplyr::group_by(path, epoch) %>%
          dplyr::mutate(hash_rate_share = hash_rate / sum(hash_rate)) %>%
          dplyr::ungroup()
      ) %>%
      dplyr::bind_rows(., .id = "setting") %>%
      dplyr::select(setting,
                    path,
                    epoch,
                    currency,
                    blockheight,
                    datetime,
                    dplyr::everything())
    return(setting_path_currency_epoch_df)
  }

convert_epoch_currency_list_to_blockheight_df <-
  function(epoch_currency_list) {
    setting_path_currency_blockheight_df <-
      epoch_currency_list %>%
      purrr::map(
        .,
        ~ purrr::set_names(., 1:length(.)) %>%
          dplyr::bind_rows(., .id = "path") %>%
          dplyr::filter(generated) %>%
          dplyr::arrange(path, currency, blockheight) %>%
          dplyr::mutate(block_time_fit = 1 / (winning_rate * hash_rate)) %>%
          dplyr::group_by(path, currency) %>%
          dplyr::mutate(
            block_time = difftime(datetime, dplyr::lag(datetime), units = "sec"),
            block_time = as.integer(block_time),
            block_time = ifelse(is.na(block_time), 0, block_time)
          ) %>%
          dplyr::ungroup()
      ) %>%
      dplyr::bind_rows(., .id = "setting") %>%
      dplyr::select(setting,
                    path,
                    currency,
                    blockheight,
                    datetime,
                    dplyr::everything())
    return(setting_path_currency_blockheight_df)
  }

plot_currency_variable_setting_datetime <-
  function(setting_path_currency_df,
           y_scale) {
    currency_list <-
      setting_path_currency_df %>%
      dplyr::distinct(currency) %>%
      dplyr::pull(currency)
    var_list <-
      c("block_time",
        "block_time_fit",
        "hash_rate",
        "hash_rate_share",
        "winning_rate")
    var_list <-
      var_list[var_list %in% colnames(setting_path_currency_df)]
    setting_list <-
      setting_path_currency_df %>%
      dplyr::distinct(setting) %>%
      dplyr::pull(setting)
    
    p_all <-
      foreach (currency_i = currency_list) %do% {
        p_currency <-
          foreach (var_j = var_list) %do% {
            if (y_scale == "log") {
              if (var_j == "block_time") {
                y_label <- paste("Log of", var_j, "(s)")
              } else if (var_j == "block_time_fit") {
                y_label <- paste("Log of", var_j, "(s)")
              } else if (var_j == "hash_rate") {
                y_label <- paste("Log of", var_j, "(h/s)")
              } else if (var_j == "winning_rate") {
                y_label <- paste("Log of", var_j)
              } else if (var_j == "reward_quoteusd") {
                y_label <- paste("Log of", var_j, "(USD)")
              } else if (var_j == "volume") {
                y_label <- paste("Log of", var_j)
              } else if (var_j == "hash_rate_share") {
                y_label <- paste("Log of", var_j)
              }
            } else if (y_scale == "level") {
              if (var_j == "block_time") {
                y_label <- paste(var_j, "(s)")
              } else if (var_j == "block_time_fit") {
                y_label <- paste(var_j, "(s)")
              } else if (var_j == "hash_rate") {
                y_label <- paste(var_j, "(h/s)")
              } else if (var_j == "winning_rate") {
                y_label <- paste(var_j)
              } else if (var_j == "reward_quoteusd") {
                y_label <- paste(var_j, "(USD)")
              } else if (var_j == "volume") {
                y_label <- paste(var_j)
              } else if (var_j == "hash_rate_share") {
                y_label <- paste(var_j)
              }
            }
            
            p_currency_var <-
              foreach (setting_k = setting_list) %do% {
                p_currency_var_setting <-
                  setting_path_currency_df %>%
                  dplyr::filter(simulated) %>%
                  dplyr::rename(target = dplyr::any_of(var_j)) %>%
                  dplyr::filter(setting == setting_k,
                                currency == currency_i)
                if (y_scale == "log") {
                  p_currency_var_setting <-
                    p_currency_var_setting %>%
                    dplyr::mutate(target = log(target))
                }
                p_currency_var_setting <-
                  p_currency_var_setting %>%
                  ggplot(aes(
                    x = datetime,
                    y = target,
                    color = path
                  )) +
                  geom_line() +
                  labs(x = "Datetime",
                       y = y_label,
                       color = "") +
                  scale_color_viridis_d() +
                  theme_classic() +
                  theme(legend.position = "none")
                
                if (grepl("block_time", var_j)) {
                  if (y_scale == "log") {
                    p_currency_var_setting <-
                      p_currency_var_setting +
                      geom_hline(yintercept = log(600),
                                 linetype = "dotted")
                  } else if (y_scale == "level") {
                    p_currency_var_setting <-
                      p_currency_var_setting +
                      geom_hline(yintercept = 600,
                                 linetype = "dotted")
                  }
                }
                
                return(p_currency_var_setting)
              }
            names(p_currency_var) <- setting_list
            return(p_currency_var)
          }
        names(p_currency) <- var_list
        return(p_currency)
      }
    names(p_all) <- currency_list
    return(p_all)
  }

summarize_blockheight_simulation_at_setting_currency_datehour <-
  function(setting_path_currency_blockheight_df) {
    setting_currency_datehour_df <-
      setting_path_currency_blockheight_df %>%
      dplyr::filter(simulated) %>%
      dplyr::mutate(block_time_fit = 1 / (winning_rate * hash_rate)) %>%
      dplyr::mutate(datehour = lubridate::floor_date(datetime, unit = "hour")) %>%
      # dplyr::group_by(setting, path) %>%
      # dplyr::mutate(datehour_last = max(datehour)) %>%
      # dplyr::ungroup() %>%
      # dplyr::mutate(datehour_last_min = min(datehour_last)) %>%
      # dplyr::filter(datehour <= datehour_last_min) %>%
      dplyr::group_by(setting, currency, path, datehour) %>%
      dplyr::summarise(dplyr::across(
        .cols = c(block_time, block_time_fit, hash_rate, winning_rate),
        .fns = mean
      )) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(setting, currency, datehour) %>%
      dplyr::summarise(dplyr::across(
        .cols = c(block_time, block_time_fit, hash_rate, winning_rate),
        .fns = list(
          mean = mean,
          sd = sd,
          iqr = ~ quantile(., 0.75) - quantile(., 0.25),
          range = ~ max(.) - min(.)
        ),
        .names = "{col}_{fn}"
      )) %>%
      dplyr::ungroup()
    return(setting_currency_datehour_df)
  }

summarize_epoch_simulation_at_setting_currency_datehour <-
  function(setting_path_currency_epoch_df) {
    setting_currency_datehour_df <-
      setting_path_currency_epoch_df %>%
      dplyr::filter(simulated) %>%
      dplyr::mutate(block_time_fit = 1 / (winning_rate * hash_rate)) %>%
      dplyr::group_by(setting, path, epoch) %>%
      dplyr::mutate(hash_rate_share = hash_rate / sum(hash_rate)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(datehour = lubridate::floor_date(datetime, unit = "hour")) %>%
      # dplyr::group_by(setting, path) %>%
      # dplyr::mutate(datehour_last = max(datehour)) %>%
      # dplyr::ungroup() %>%
      # dplyr::mutate(datehour_last_min = min(datehour_last)) %>%
      # dplyr::filter(datehour <= datehour_last_min) %>%
      dplyr::group_by(setting, currency, path, datehour) %>%
      dplyr::summarise(dplyr::across(
        .cols = c(
          block_time,
          block_time_fit,
          hash_rate,
          hash_rate_share,
          winning_rate
        ),
        .fns = mean
      )) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(setting, currency, datehour) %>%
      dplyr::summarise(dplyr::across(
        .cols = c(
          block_time,
          block_time_fit,
          hash_rate,
          hash_rate_share,
          winning_rate
        ),
        .fns = list(
          mean = mean,
          sd = sd,
          iqr = ~ quantile(., 0.75) - quantile(., 0.25),
          range = ~ max(.) - min(.)
        ),
        .names = "{col}_{fn}"
      )) %>%
      dplyr::ungroup()
    return(setting_currency_datehour_df)
  }

plot_setting_currency_datehour <-
  function(setting_currency_datehour_df,
           currency_to_plot) {
    var_list <-
      c("block_time", "block_time_fit", "hash_rate", "winning_rate")
    stat_list <- c("mean", "sd", "iqr", "range")
    p_all <-
      foreach (var = var_list) %do% {
        name <- gsub("_", " ", var)
        p_var <-
          foreach (stat = stat_list) %do% {
            if (var == "block_time") {
              y_label <- paste("Log of the", stat, "of", name, "(s)")
            } else if (var == "block_time_fit") {
              y_label <- paste("Log of the", stat, "of", name, "(s)")
            } else if (var == "hash_rate") {
              y_label <- paste("Log of the", stat, "of", name, "(h/s)")
            } else if (var == "winning_rate") {
              y_label <- paste("Log of the", stat, "of", name)
            }
            p <-
              setting_currency_datehour_df %>%
              dplyr::rename(target = dplyr::any_of(paste(var, stat, sep = "_"))) %>%
              dplyr::filter(currency == currency_to_plot) %>%
              dplyr::mutate(target = log(target)) %>%
              dplyr::mutate(setting = gsub("_", " ", setting)) %>%
              ggplot(aes(x = datehour,
                         y = target,
                         color = setting)) +
              geom_point() +
              labs(x = "Datehour",
                   y = y_label,
                   color = "") +
              scale_color_viridis_d() +
              theme_classic() +
              theme(legend.position = "bottom")
            return(p)
          }
        names(p_var) <- stat_list
        return(p_var)
      }
    names(p_all) <- var_list
    
    return(p_all)
  }

plot_expected_reward <-
  function (setting_path_currency_df) {
    setting_path_currency_df <-
      setting_path_currency_df %>%
      dplyr::filter(simulated)
    setting_list <-
      setting_path_currency_df %>%
      dplyr::distinct(setting) %>%
      dplyr::pull(setting)
    
    p <- foreach (i = 1:length(setting_list)) %do% {
      p_i <-
        setting_path_currency_df %>%
        dplyr::filter(setting == setting_list[i]) %>%
        dplyr::mutate(
          reward_winning_rate = reward_quoteusd * winning_rate,
          log_reward_winning_rate = log(reward_winning_rate)
        ) %>%
        ggplot(aes(x = datetime,
                   y = log_reward_winning_rate,
                   colour = currency)) +
        geom_line() +
        scale_colour_viridis_d() +
        labs(x = "Date",
             y = "Log of reward (USD) x wining rate",
             colour = "") +
        theme_classic() +
        theme(legend.position = "bottom")
      return(p_i)
    }
    names(p) <- setting_list
    return(p)
  }

make_supply_curve <-
  function (epoch_currency_n,
            currency_list,
            ratio_list,
            spec,
            parameters,
            electricity_cost,
            algo,
            rate_estimate) {
    epoch_currency_curve <-
      foreach (i = 1:length(ratio_list)) %do% {
        var_i <- names(ratio_list)[i]
        ratio_list_i <- ratio_list[[i]]
        epoch_currency_curve_var <-
          foreach (currency_j = currency_list) %do% {
            epoch_currency_curve_var_currency <-
              foreach (ratio_k = ratio_list_i) %do% {
                # modify the epoch
                epoch_currency_n_changed <-
                  epoch_currency_n %>%
                  dplyr::mutate(dplyr::across(
                    dplyr::matches(var_i),
                    ~ ifelse(currency == currency_j, . * ratio_k, .)
                  ))
                
                # make baseline data for computing utility and choice probability
                base_epoch_currency_n <-
                  make_base_epoch_curency(spec,
                                          epoch_currency = epoch_currency_n_changed,
                                          currency_base,
                                          rate_estimate = rate_estimate)
                
                # update hash rate
                epoch_currency_n_changed <-
                  update_hash_rate(
                    epoch_currency_n_changed,
                    base_epoch_currency_n,
                    spec,
                    parameters,
                    electricity_cost,
                    algo
                  )
                
                return(epoch_currency_n_changed)
              }
            epoch_currency_curve_var_currency <-
              epoch_currency_curve_var_currency %>%
              purrr::set_names(ratio_list_i) %>%
              dplyr::bind_rows(., .id = "ratio")
            return(epoch_currency_curve_var_currency)
          }
        epoch_currency_curve_var <-
          epoch_currency_curve_var %>%
          purrr::set_names(currency_list)
        return(epoch_currency_curve_var)
      }
    epoch_currency_curve <-
      epoch_currency_curve %>%
      purrr::set_names(names(ratio_list))
    return(epoch_currency_curve)
  }

plot_supply_curve <-
  function(supply_curve) {
    p <-
      foreach (i = 1:length(supply_curve)) %do% {
        var_i <- names(supply_curve)[i]
        p_i <-
          foreach (j = 1:length(names(supply_curve[[i]]))) %do% {
            currency_j <- names(supply_curve[[i]])[j]
            target_actual <-
              supply_curve[[i]][[j]] %>%
              dplyr::filter(currency == currency_j,
                            ratio == 1) %>%
              dplyr::rename(target = dplyr::any_of(var_i)) %>%
              dplyr::pull(target)
            p_ij <-
              supply_curve[[i]][[j]] %>%
              dplyr::rename(target = dplyr::any_of(var_i)) %>%
              dplyr::group_by(ratio) %>%
              dplyr::mutate(target = sum(target * (currency == currency_j))) %>%
              dplyr::ungroup() %>%
              ggplot(aes(
                x = log(target),
                y = log(hash_rate),
                color = currency
              )) +
              geom_line() +
              geom_vline(xintercept = log(target_actual),
                         linetype = "dashed") +
              scale_color_viridis_d() +
              theme_classic() +
              theme(legend.position = "bottom")
            return(p_ij)
          }
        names(p_i) <- names(supply_curve[[i]])
        return(p_i)
      }
    names(p) <- names(supply_curve)
    return(p)
  }

compute_loglikelihood_coxhazard <-
  function(beta,
           y,
           d,
           x) {
    df <-
      tibble::tibble(y = y,
                     d = d,
                     x_beta = x %*% beta %>% as.numeric()) %>%
      dplyr::mutate(i = 1:length(y)) %>%
      dplyr::arrange(-y)
    
    loglikelihood <-
      df %>%
      dplyr::mutate(theta_sum = cumsum(exp(x_beta))) %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(theta_sum = max(theta_sum)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(d == 1) %>%
      dplyr::summarise(value = sum(x_beta - log(theta_sum))) %>%
      as.numeric()
    
    return(loglikelihood)
    
  }

compute_loglikelihood_coxhazard_efron <-
  function(beta,
           y,
           d,
           x) {
    df <-
      tibble::tibble(y = y,
                     d = d,
                     x_beta = x %*% beta %>% as.numeric()) %>%
      dplyr::mutate(i = 1:length(y)) %>%
      dplyr::arrange(-y)
    
    loglikelihood <-
      df %>%
      dplyr::mutate(theta_sum_above = cumsum(exp(x_beta))) %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(theta_sum_above = max(theta_sum_above)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(theta_sum_tie = sum(d * exp(x_beta)),
                    m = sum(d)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(i) %>%
      dplyr::filter(d == 1) %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(l = 0:(length(y) - 1)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(value = sum(x_beta - log(theta_sum_above - l / m * theta_sum_tie))) %>%
      as.numeric()
    
    return(loglikelihood)
    
  }

compute_score_coxhazard_efron <-
  function(beta,
           y,
           d,
           x) {
    df <-
      data.frame(
        y = y,
        d = d,
        x_beta = x %*% beta %>% as.numeric(),
        x
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(i = 1:length(y)) %>%
      dplyr::arrange(-y)
    
    theta <-
      df %>%
      dplyr::mutate(theta_sum_above = cumsum(exp(x_beta))) %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(theta_sum_above = max(theta_sum_above)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(theta_sum_tie = sum(d * exp(x_beta)),
                    m = sum(d)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-y,-d,-x_beta)
    
    theta_x <-
      df %>%
      dplyr::mutate(theta = exp(x_beta)) %>%
      tidyr::pivot_longer(cols = colnames(x),
                          names_to = "variable",
                          values_to = "x") %>%
      dplyr::mutate(theta_x = theta * x) %>%
      dplyr::select(i, y, d, variable, x, theta_x) %>%
      dplyr::arrange(variable,-y)
    theta_x <-
      theta_x %>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(theta_x_sum_above = cumsum(theta_x)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(variable, y) %>%
      dplyr::mutate(theta_x_sum_above = max(theta_x_sum_above)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(variable, y) %>%
      dplyr::mutate(theta_x_sum_tie = sum(d * theta_x)) %>%
      dplyr::ungroup()
    
    score <-
      theta_x %>%
      dplyr::left_join(theta,
                       by = "i") %>%
      dplyr::filter(d == 1) %>%
      dplyr::group_by(variable, y) %>%
      dplyr::mutate(l = 0:(length(y) - 1)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = x - (theta_x_sum_above - l / m * theta_x_sum_tie) / (theta_sum_above - l / m * theta_sum_tie)
      ) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(value =  sum(value)) %>%
      dplyr::ungroup() %>%
      tibble::column_to_rownames("variable")
    
    score <- score[names(beta),]
    
    return(score)
    
  }

compute_loglikelihood_arrival_rate <-
  function(theta,
           winning_rate,
           epoch_time,
           generated,
           x_variable) {
    term <- log(winning_rate) + as.numeric(x_variable %*% theta)
    loglikelihood <- -epoch_time * exp(term) + generated * term
    loglikelihood <- sum(loglikelihood) %>% as.numeric()
    return(loglikelihood)
  }

compute_score_arrival_rate <-
  function(theta,
           winning_rate,
           epoch_time,
           generated,
           x_variable) {
    term <- log(winning_rate) + as.numeric(x_variable %*% theta)
    multiplier_1 <- -epoch_time * exp(term)
    multiplier_2 <- generated
    
    score <- x_variable
    
    score <-
      score * (multiplier_1 %*% t(rep(1, ncol(score)))) +
      score * (multiplier_2 %*% t(rep(1, ncol(score))))
    
    score <-
      apply(score, 2, sum)
    
    return(score)
  }

estimate_arrival_rate <-
  function(h_variable_list,
           algo_epoch_variables) {
    result_list <-
      h_variable_list %>%
      purrr::map(.,
                 function (h_variable) {
                   x_variable <-
                     model.matrix(object = h_variable,
                                  data = algo_epoch_variables)
                   colnames(x_variable) <-
                     colnames(x_variable) %>%
                     gsub("as.factor", "", .) %>%
                     gsub("[^[:alnum:]]", "_", .) %>%
                     gsub("^_", "", .) %>%
                     gsub("_$", "", .)
                   
                   theta <- rep(0, ncol(x_variable))
                   names(theta) <- colnames(x_variable)
                   
                   result <-
                     maxLik::maxLik(
                       start = theta,
                       logLik = compute_loglikelihood_arrival_rate_rcpp,
                       grad = compute_score_arrival_rate_rcpp,
                       method = "BFGS",
                       control = list(printLevel = 2),
                       winning_rate = algo_epoch_variables$winning_rate,
                       epoch_time = algo_epoch_variables$epoch_time,
                       generated = algo_epoch_variables$generated,
                       x_variable = x_variable
                     )
                   return(result)
                 })
    return(result_list)
  }


# for modelsummary to use rdrobust
tidy_rdrobust <-
  function(object, ...) {
    ret <- data.frame(
      term = row.names(object$coef),
      estimate = object$coef[, 1],
      std.error = object$se[, 1],
      statistic = object$z[, 1],
      p.value = object$pv[, 1],
      conf.low = object$ci[, 1],
      conf.high = object$ci[, 2]
    )
    row.names(ret) <- NULL
    return(ret)
  }

glance_rdrobust <-
  function(object, ...) {
    ret <- data.frame(
      nobs.left = object$N[1],
      nobs.right = object$N[2],
      nobs.effective.left = object$N_h[1],
      nobs.effective.right = object$N_h[2],
      h.left = object$bws["h", "left"],
      h.right = object$bws["h", "right"],
      b.left = object$bws["b", "left"],
      b.right = object$bws["b", "right"],
      cutoff = object$c,
      order.regression = object$q,
      order.bias = object$q,
      kernel = object$kernel,
      bwselect = object$bwselect
    )
    return(ret)
  }

perform_rdd_reward_change <-
  function(epoch_currency,
           target,
           base,
           cutoff,
           h,
           h_short,
           original,
           binselect,
           p,
           halving_line_l,
           halving_line_s,
           linetype_l,
           linetype_s,
           figure_width = 4,
           figure_height = 3,
           path = NULL,
           palette = scales::viridis_pal()(1),
           block = 144,
           target_before = "BCH",
           target_after = "BSV",
           target_line = TRUE
           ) {
    
    # filter data
    datetime_cutoff <-
      epoch_currency %>%
      dplyr::filter(currency == base) %>%
      dplyr::filter(generated) %>%
      dplyr::filter(blockheight == cutoff) %>%
      dplyr::pull(datetime)
    
    epoch_currency_target <-
      epoch_currency %>%
      dplyr::filter(currency == target) %>%
      dplyr::filter(generated) %>%
      dplyr::filter(
        datetime > datetime_cutoff - h,
        datetime < datetime_cutoff + h
      )
    
    epoch_currency_target_short <-
      epoch_currency %>%
      dplyr::filter(currency == target) %>%
      dplyr::filter(generated) %>%
      dplyr::filter(
        datetime > datetime_cutoff - h_short,
        datetime < datetime_cutoff + h_short
      )
    
    if (target_line == TRUE) {
      
      datetime_cutoff_before <-
        epoch_currency %>%
        dplyr::filter(currency == target_before) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(blockheight == cutoff) %>%
        dplyr::pull(datetime)
      
      datetime_cutoff_after <-
        epoch_currency %>%
        dplyr::filter(currency == target_after) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(blockheight == cutoff) %>%
        dplyr::pull(datetime)
      
      block_target_cutoff_before <-
        epoch_currency %>%
        dplyr::filter(currency == target_before) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(datetime < datetime_cutoff) %>%
        dplyr::summarise(blockheight = max(blockheight)) %>%
        dplyr::pull(blockheight)
      
      
      block_target_cutoff_after <-
        epoch_currency %>%
        dplyr::filter(currency == target_after) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(datetime < datetime_cutoff) %>%
        dplyr::summarise(blockheight = max(blockheight)) %>%
        dplyr::pull(blockheight)
      
      block_target_cutoff <- c(
        block_target_cutoff_before,
        block_target_cutoff_after
      )
    } else if (target_line == FALSE) {
      block_target_cutoff <-
        epoch_currency %>%
        dplyr::filter(currency == target) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(datetime < datetime_cutoff) %>%
        dplyr::summarise(blockheight = max(blockheight)) %>%
        dplyr::pull(blockheight) 
    } else {
      stop("Setting is invalid")
    }
    
    epoch_currency_target_block <-
      epoch_currency %>%
      dplyr::filter(currency == target) %>%
      dplyr::filter(generated) %>%
      dplyr::mutate(
        block_time_144_ma = zoo::rollmean(block_time, 144, fill = NA, align = "right")
      ) %>%
      dplyr::filter(
        blockheight > block_target_cutoff - block,
        blockheight < block_target_cutoff + block
      )
    
    # figure list
    figure <- list()
    
    # make graph of winning rate
    figure$winning_rate_long <-
    winning_rate_figure_long <-
      epoch_currency_target %>%
      ggplot(
        aes(
          x = datetime,
          y = winning_rate
        )
      ) +
      labs(
        x = "Datetime",
        y = "Winning rate"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(winning_rate_figure_long)
    
    figure$winning_rate_short <-
    winning_rate_figure_short <-
      epoch_currency_target_short %>%
      ggplot(
        aes(
          x = datetime,
          y = winning_rate
        )
      ) +
      labs(
        x = "Datetime",
        y = "Winning rate"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )

    plot(winning_rate_figure_short)
    
    figure$winning_rate_block <-
    winning_rate_figure_block <-
      epoch_currency_target_block %>%
      ggplot(
        aes(
          x = blockheight,
          y = winning_rate
        )
      ) +
      labs(
        x = "Blockheight",
        y = "Winning rate"
      ) +
      geom_vline(
        xintercept = block_target_cutoff,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(winning_rate_figure_block)
    
    # make graph of expected reward
    figure$expected_reward_long <-
    expected_reward_figure_long <-
      epoch_currency_target %>%
      dplyr::mutate(
        expected_reward = winning_rate * reward_quoteusd
      ) %>%
      ggplot(
        aes(
          x = datetime,
          y = expected_reward
        )
      ) +
      labs(
        x = "Datetime",
        y = "Expected reward (USD)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(expected_reward_figure_long)
    
    figure$expected_reward_short <-
    expected_reward_figure_short <-
      epoch_currency_target_short %>%
      dplyr::mutate(
        expected_reward = winning_rate * reward_quoteusd
      ) %>%
      ggplot(
        aes(
          x = datetime,
          y = expected_reward
        )
      ) +
      labs(
        x = "Datetime",
        y = "Expected reward (USD)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(expected_reward_figure_short)
    
    figure$expected_reward_block <-
    expected_reward_figure_block <-
      epoch_currency_target_block %>%
      dplyr::mutate(
        expected_reward = winning_rate * reward_quoteusd
      ) %>%
      ggplot(
        aes(
          x = blockheight,
          y = expected_reward
        )
      ) +
      labs(
        x = "Blockheight",
        y = "Expected reward (USD)"
      ) +
      geom_vline(
        xintercept = block_target_cutoff,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 1
      ) +
      # stat_summary_bin(
      #   fun.data = mean_cl_boot,
      #   color = palette[1]
      # ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(expected_reward_figure_block)
  
    # make graph of block time
    figure$block_time_long <-
    block_time_figure_long <-
      epoch_currency_target %>%
      ggplot(
        aes(
          x = datetime,
          y = block_time
        )
      ) +
      labs(
        x = "Datetime",
        y = "Block time (s)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 0.2
      ) +
      stat_summary_bin(
        fun.data = mean_cl_boot,
        color = palette[1]
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(block_time_figure_long)
    
    figure$block_time_short <-
    block_time_figure_short <-
      epoch_currency_target_short %>%
      ggplot(
        aes(
          x = datetime,
          y = block_time
        )
      ) +
      labs(
        x = "Datetime",
        y = "Block time (s)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 0.2
      ) +
      stat_summary_bin(
        fun.data = mean_cl_boot,
        color = palette[1]
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )

    plot(block_time_figure_short)
    
    figure$block_time_block <-
    block_time_figure_block <-
      epoch_currency_target_block %>%
      ggplot(
        aes(
          x = blockheight,
          y = block_time_144_ma
        )
      ) +
      labs(
        x = "Blockheight",
        y = "144-Moving average of block time (s)"
      ) +
      geom_vline(
        xintercept = block_target_cutoff,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_line(
        color = palette[1]
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.y = element_text(hjust = 0.9)
      )
    
    plot(block_time_figure_block)

    
    # make graph of hash rate
    figure$hash_rate_long <-
    log_of_hash_rate_figure_long <-
      epoch_currency_target %>%
      ggplot(
        aes(
          x = datetime,
          y = hash_rate %>% log()
        )
      ) +
      labs(
        x = "Datetime",
        y = "Log of hash rate (H/s)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 0.2
      ) +
      stat_summary_bin(
        fun.data = mean_cl_boot,
        color = palette[1]
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      )  +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(log_of_hash_rate_figure_long)
    
    figure$hash_rate_short <-
    log_of_hash_rate_figure_short <-
      epoch_currency_target_short %>%
      ggplot(
        aes(
          x = datetime,
          y = hash_rate %>% log()
        )
      ) +
      labs(
        x = "Datetime",
        y = "Log of hash rate (H/s)"
      ) +
      geom_vline(
        xintercept = halving_line_s,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_point(
        color = palette[1],
        shape = 20,
        alpha = 0.2
      ) +
      stat_summary_bin(
        fun.data = mean_cl_boot,
        color = palette[1]
      ) +
      theme_classic() +
      scale_x_continuous(
        labels = function(x)
          format(
            as.POSIXct(
              x,
              origin = "1970-1-1",
              tz = "UTC"
            ),
            format = "%Y-%m-%d"
          )
      ) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(log_of_hash_rate_figure_short)
 
    figure$hash_rate_block <-
    log_of_hash_rate_figure_block <-
      epoch_currency_target_block %>%
      ggplot(
        aes(
          x = blockheight,
          y = hash_rate %>% log()
        )
      ) +
      labs(
        x = "Blockheight",
        y = "Log of hash rate (H/s)"
      ) +
      geom_vline(
        xintercept = block_target_cutoff,
        size = 1,
        linetype = linetype_s,
        alpha = 0.5
      ) +
      geom_line(
        color = palette[1]
      ) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
    
    plot(log_of_hash_rate_figure_block)
    
    # rdd
    
    result <- list()
    
    ## block time with bwselect
    res <-
      tryCatch({
        res <-
          rdrobust::rdrobust(
            y = epoch_currency_target$block_time,
            x = epoch_currency_target$datetime %>% as.numeric(),
            c = datetime_cutoff %>% as.numeric(),
            bwselect = "msetwo",
            all = TRUE
          )
      }, error = function(e) {
        res <-
          rdrobust::rdrobust(
            y = epoch_currency_target$block_time,
            x = epoch_currency_target$datetime %>% as.numeric(),
            c = datetime_cutoff %>% as.numeric(),
            bwselect = "msetwo",
            p = 1,
            all = TRUE
          )
        return(res)
      })
    result$block_time <- res
    object <- res
    title <- "Block time (s): Optimal h"
    ret <-
      cbind(
        tidy_rdrobust(object),
        glance_rdrobust(object)
      )
    ret %>%
      modelsummary::datasummary_df(
        .,
        title = title
        ) %>%
      print()
    
    ## hash rate with bwselect
    res <-
      tryCatch({
        res <-
          rdrobust::rdrobust(
            y = epoch_currency_target$hash_rate %>% log(),
            x = epoch_currency_target$datetime %>% as.numeric(),
            c = datetime_cutoff %>% as.numeric(),
            bwselect = "msetwo",
            all = TRUE
          )
      }, error = function(e) {
        res <-
          rdrobust::rdrobust(
            y = epoch_currency_target$hash_rate %>% log(),
            x = epoch_currency_target$datetime %>% as.numeric(),
            c = datetime_cutoff %>% as.numeric(),
            bwselect = "msetwo",
            p = 2,
            all = TRUE
          )
        return(res)
      })
    result$hash_rate <- res
    object <- res
    title <- "Log of hash rate (h/s): Optimal h"
    ret <-
      cbind(tidy_rdrobust(object),
            glance_rdrobust(object))
    ret %>%
      modelsummary::datasummary_df(
        .,
        title = title
      ) %>%
      print()
    
    return(
      list(
        result = result,
        figure = figure
      )
    )
    
  }

make_table_rdd <-
  function(
    result_list,
    savename
  ) {
    
    result_estimate <-
      result_list %>%
      purrr::map_depth(
        .,
        .depth = 2,
        .f = tidy_rdrobust
      ) %>%
      purrr::map_depth(
        .,
        .depth = 2,
        .f = ~ dplyr::filter(., term == "Robust")
      ) %>%
      purrr::map(
        .,
        ~ dplyr::bind_rows(., .id = "y")
      ) %>%
      dplyr::bind_rows(., .id = "currency") %>%
      dplyr::select(
        currency, y, estimate, std.error
      ) %>%
      dplyr::mutate_if(
        is.numeric,
        ~ formatC(., digit = 3)
      ) %>%
      dplyr::mutate(
        std.error = paste0("(", std.error, ")")
      ) %>%
      tidyr::pivot_longer(
        cols = c(estimate, std.error)
      ) %>%
      tidyr::pivot_wider(
        id_cols = c(name),
        names_from = c(currency, y),
        values_from = value
      )
    
    result_setting <-
      result_list %>%
      purrr::map_depth(
        .,
        .depth = 2,
        .f = glance_rdrobust
      ) %>%
      purrr::map(
        .,
        ~ dplyr::bind_rows(., .id = "y")
      ) %>%
      dplyr::bind_rows(., .id = "currency") %>%
      dplyr::select(
        currency, y, nobs.left, nobs.right, h.left, h.right, b.left, b.right, order.regression, kernel, bwselect
      ) %>%
      dplyr::mutate_if(
        is.numeric,
        ~ formatC(., digit = 3)
      ) %>%
      tidyr::pivot_longer(
        cols = c(nobs.left, nobs.right, h.left, h.right, b.left, b.right, order.regression, kernel, bwselect)
      ) %>%
      tidyr::pivot_wider(
        id_cols = c(name),
        names_from = c(currency, y),
        values_from = value
      )
    
    result <-
      dplyr::bind_rows(
        result_estimate,
        result_setting
      )
    
    result %>%
      dplyr::select(
        name, 
        BTC_hash_rate, BCH_hash_rate, BSV_hash_rate
      ) %>%
      dplyr::mutate_all(
        .funs = Hmisc::capitalize
      ) %>%
      magrittr::set_colnames(
        c("", rep(c("BTC", "BCH", "BSV"), 1))
      ) %>%
      kbl(
        booktabs = TRUE,
        align = c("l", rep("c", ncol(result) - 1))
      ) %>%
      add_header_above(
        header = c(" " = 1, "Log of hash rate (H/s)" = 3)
      ) %>%
      row_spec(
        2, extra_latex_after = "\\midrule"
      ) %>%
      kable_styling() %>%
      print()
    
    result %>%
      dplyr::select(
        name, 
        BTC_hash_rate, BCH_hash_rate, BSV_hash_rate
      ) %>%
      dplyr::mutate_all(
        .funs = Hmisc::capitalize
      ) %>%
      magrittr::set_colnames(
        c("", rep(c("BTC", "BCH", "BSV"), 1))
      ) %>%
      kbl(
        booktabs = TRUE,
        align = c("l", rep("c", ncol(result) - 1)),
        format = "latex",
        linesep = ""
      ) %>%
      add_header_above(
        header = c(" " = 1, "Log of hash rate (H/s)" = 3)
      ) %>%
      row_spec(
        2, extra_latex_after = "\\midrule"
      ) %>%
      save_kable(
        file = here::here(savename)
      )
    return(NULL)
  }

# rdd with rdmulti
perform_multirdd_reward_change <-
  function(epoch_currency,
           target,
           basecutoff,
           h,
           h_short,
           original,
           binselect,
           p,
           figure = FALSE,
           figurename,
           figure_width = 16,
           figure_height = 9,
           path = NULL) {
    if (base == "BTC" || target == "BTC" || original == TRUE) {
      datetime_cutoff <-
        epoch_currency %>%
        dplyr::filter(currency == base) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(blockheight == cutoff) %>%
        dplyr::pull(datetime)
      
      epoch_currency_target <-
        epoch_currency %>%
        dplyr::filter(currency == target) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(datetime > datetime_cutoff - h,
                      datetime < datetime_cutoff + h)
      
      epoch_currency_target_short <-
        epoch_currency %>%
        dplyr::filter(currency == target) %>%
        dplyr::filter(generated) %>%
        dplyr::filter(datetime > datetime_cutoff - h_short,
                      datetime < datetime_cutoff + h_short)
      
      winning_rate_figure <-
        rdrobust::rdplot(
          y = epoch_currency_target$winning_rate,
          x = epoch_currency_target$datetime %>% as.numeric(),
          c = datetime_cutoff %>% as.numeric(),
          x.label = "Datetime",
          y.label = "Winning rate",
          p = p,
          title = "",
          binselect = binselect
        )
      
      
      long_winning_rate_figure <-
        winning_rate_figure$rdplot +
        ggplot2::scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x, origin = "1970-1-1"), format = "%Y-%m-%d")
        )
      
      plot(long_winning_rate_figure)
      
      
      rdplot_mean_bin_1 <-
        as.data.frame(winning_rate_figure$vars_bins$rdplot_mean_bin)
      
      colnames(rdplot_mean_bin_1) <- "rdplot_mean_bin"
      
      rdplot_mean_y_1 <-
        as.data.frame(winning_rate_figure$vars_bins$rdplot_mean_y)
      
      colnames(rdplot_mean_y_1) <- "rdplot_mean_y"
      
      df_scatter_1 <- cbind(rdplot_mean_bin_1,
                            rdplot_mean_y_1) %>%
        dplyr::filter(rdplot_mean_bin >= x_l,
                      rdplot_mean_bin <= x_u)
      
      winning_rate_result <- ggplot(df_scatter_1,
                                    aes(x = rdplot_mean_bin,
                                        y = rdplot_mean_y)) +
        geom_point(size = 1,
                   color = "darkblue") +
        geom_vline(
          xintercept = datetime_cutoff %>% as.numeric(),
          size = 1,
          linetype = "dashed",
          alpha = 0.5
        ) +
        labs(x = "Datetime",
             y = "Winning rate") +
        theme_bw() +
        scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x,
                              origin = "1970-1-1"),
                   format = "%Y-%m-%d")
        )
      
      plot(winning_rate_result)
      
      expected_reward_figure <-
        rdrobust::rdplot(
          y = epoch_currency_target$winning_rate * epoch_currency_target$reward_quoteusd,
          x = epoch_currency_target$datetime %>% as.numeric(),
          c = datetime_cutoff %>% as.numeric(),
          x.label = "Datetime",
          y.label = "Expected reward (USD)",
          p = p,
          title = "",
          binselect = binselect
        )
      
      long_expected_reward_figure <-
        expected_reward_figure$rdplot +
        ggplot2::scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x, origin = "1970-1-1"), format = "%Y-%m-%d")
        )
      
      plot(long_expected_reward_figure)
      
      
      
      rdplot_mean_bin_2 <-
        as.data.frame(expected_reward_figure$vars_bins$rdplot_mean_bin)
      
      colnames(rdplot_mean_bin_2) <- "rdplot_mean_bin"
      
      rdplot_mean_y_2 <-
        as.data.frame(expected_reward_figure$vars_bins$rdplot_mean_y)
      
      colnames(rdplot_mean_y_2) <- "rdplot_mean_y"
      
      df_scatter_2 <- cbind(rdplot_mean_bin_2,
                            rdplot_mean_y_2) %>%
        dplyr::filter(rdplot_mean_bin >= x_l,
                      rdplot_mean_bin <= x_u)
      
      expected_reward_result <- ggplot(df_scatter_2,
                                       aes(x = rdplot_mean_bin,
                                           y = rdplot_mean_y)) +
        geom_point(size = 1,
                   color = "darkblue") +
        geom_vline(
          xintercept = datetime_cutoff %>% as.numeric(),
          size = 1,
          linetype = "dashed",
          alpha = 0.5
        ) +
        labs(x = "Datetime",
             y = "Expected reward (USD)") +
        theme_bw() +
        scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x,
                              origin = "1970-1-1"),
                   format = "%Y-%m-%d")
        )
      
      plot(expected_reward_result)
      
      
      block_time_figure <-
        rdrobust::rdplot(
          y = epoch_currency_target_short$block_time,
          x = epoch_currency_target_short$datetime %>% as.numeric(),
          c = datetime_cutoff %>% as.numeric(),
          x.label = "Datetime",
          y.label = "Block time (s)",
          p = p,
          title = "",
          binselect = binselect
        )
      
      long_block_time_figure <-
        block_time_figure$rdplot +
        ggplot2::scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x, origin = "1970-1-1"), format = "%Y-%m-%d")
        )
      
      plot(long_block_time_figure)
      
      
      rdplot_mean_bin_3 <-
        as.data.frame(block_time_figure$vars_bins$rdplot_mean_bin)
      
      colnames(rdplot_mean_bin_3) <- "rdplot_mean_bin"
      
      rdplot_mean_y_3 <-
        as.data.frame(block_time_figure$vars_bins$rdplot_mean_y)
      
      colnames(rdplot_mean_y_3) <- "rdplot_mean_y"
      
      df_scatter_3 <- cbind(rdplot_mean_bin_3,
                            rdplot_mean_y_3) %>%
        dplyr::filter(rdplot_mean_bin >= x_l,
                      rdplot_mean_bin <= x_u)
      
      block_time_result <- ggplot(df_scatter_3,
                                  aes(x = rdplot_mean_bin,
                                      y = rdplot_mean_y)) +
        geom_point(size = 1,
                   color = "darkblue") +
        geom_vline(
          xintercept = datetime_cutoff %>% as.numeric(),
          size = 1,
          linetype = "dashed",
          alpha = 0.5
        ) +
        labs(x = "Datetime",
             y = "Block time (s)") +
        theme_bw() +
        scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x,
                              origin = "1970-1-1"),
                   format = "%Y-%m-%d")
        )
      
      plot(block_time_result)
      
      
      log_of_hash_rate_figure <-
        rdrobust::rdplot(
          y = epoch_currency_target_short$hash_rate %>% log(),
          x = epoch_currency_target_short$datetime %>% as.numeric(),
          c = datetime_cutoff %>% as.numeric(),
          x.label = "Datetime",
          y.label = "Log of hash rate (H/s)",
          p = p,
          title = "",
          binselect = binselect
        )
      
      long_log_of_hash_rate_figure <-
        log_of_hash_rate_figure$rdplot +
        ggplot2::scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x, origin = "1970-1-1"), format = "%Y-%m-%d")
        )
      
      plot(long_log_of_hash_rate_figure)
      
      
      rdplot_mean_bin_4 <-
        as.data.frame(log_of_hash_rate_figure$vars_bins$rdplot_mean_bin)
      
      colnames(rdplot_mean_bin_4) <- "rdplot_mean_bin"
      
      rdplot_mean_y_4 <-
        as.data.frame(log_of_hash_rate_figure$vars_bins$rdplot_mean_y)
      
      colnames(rdplot_mean_y_4) <- "rdplot_mean_y"
      
      df_scatter_4 <- cbind(rdplot_mean_bin_4,
                            rdplot_mean_y_4) %>%
        dplyr::filter(rdplot_mean_bin >= x_l,
                      rdplot_mean_bin <= x_u)
      
      log_of_hash_rate_result <- ggplot(df_scatter_4,
                                        aes(x = rdplot_mean_bin,
                                            y = rdplot_mean_y)) +
        geom_point(size = 1,
                   color = "darkblue") +
        geom_vline(
          xintercept = datetime_cutoff %>% as.numeric(),
          size = 1,
          linetype = "dashed",
          alpha = 0.5
        ) +
        labs(x = "Datetime",
             y = "Log of hash rate (H/s)") +
        theme_bw() +
        scale_x_continuous(
          labels = function(x)
            format(as.POSIXct(x,
                              origin = "1970-1-1"),
                   format = "%Y-%m-%d")
        )
      
      plot(log_of_hash_rate_result)
      
      res_1 <-
        tryCatch({
          res_1 <-
            rdrobust::rdrobust(
              y = epoch_currency_target$block_time,
              x = epoch_currency_target$datetime %>% as.numeric(),
              c = datetime_cutoff %>% as.numeric(),
              bwselect = "msetwo",
              all = TRUE
            )
        }, error = function(e) {
          res_1 <-
            rdrobust::rdrobust(
              y = epoch_currency_target$block_time,
              x = epoch_currency_target$datetime %>% as.numeric(),
              c = datetime_cutoff %>% as.numeric(),
              bwselect = "msetwo",
              p = 1,
              all = TRUE
            )
          return(res_1)
        })
      
      object <- res_1
      title <- "Block time (s)"
      ret <-
        cbind(tidy_rdrobust(object),
              glance_rdrobust(object))
      ret %>%
        modelsummary::datasummary_df(.,
                                     title = title) %>%
        print()
      
      res_2 <-
        tryCatch({
          res_2 <-
            rdrobust::rdrobust(
              y = epoch_currency_target$hash_rate %>% log(),
              x = epoch_currency_target$datetime %>% as.numeric(),
              c = datetime_cutoff %>% as.numeric(),
              bwselect = "msetwo",
              all = TRUE
            )
        }, error = function(e) {
          res_2 <-
            rdrobust::rdrobust(
              y = epoch_currency_target$hash_rate %>% log(),
              x = epoch_currency_target$datetime %>% as.numeric(),
              c = datetime_cutoff %>% as.numeric(),
              bwselect = "msetwo",
              p = 2,
              all = TRUE
            )
          return(res_2)
        })
      
      object <- res_2
      title <- "Log of hash rate (h/s)"
      ret <-
        cbind(tidy_rdrobust(object),
              glance_rdrobust(object))
      ret %>%
        modelsummary::datasummary_df(.,
                                     title = title) %>%
        print()
      
    } else if (base == "BCH" || base == "BSV") {
      if (target == "BCH" || target == "BSV") {
        datetime_cutoff_bch <-
          epoch_currency %>%
          dplyr::filter(currency == "BCH") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(blockheight == cutoff) %>%
          dplyr::pull(datetime)
        
        datetime_cutoff_bsv <-
          epoch_currency %>%
          dplyr::filter(currency == "BSV") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(blockheight == cutoff) %>%
          dplyr::pull(datetime)
        
        # for h
        epoch_currency_target_bch <-
          epoch_currency %>%
          dplyr::filter(currency == "BCH") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(datetime > datetime_cutoff_bch - h,
                        datetime < datetime_cutoff_bsv + h)
        
        epoch_currency_target_bsv <-
          epoch_currency %>%
          dplyr::filter(currency == "BSV") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(datetime > datetime_cutoff_bch - h,
                        datetime < datetime_cutoff_bsv + h)
        
        epoch_currency_target <-
          rbind(epoch_currency_target_bch,
                epoch_currency_target_bsv)
        
        # for h_short
        epoch_currency_target_bch_short <-
          epoch_currency %>%
          dplyr::filter(currency == "BCH") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(
            datetime > datetime_cutoff_bch - h_short,
            datetime < datetime_cutoff_bsv + h_short
          )
        
        epoch_currency_target_bsv_short <-
          epoch_currency %>%
          dplyr::filter(currency == "BSV") %>%
          dplyr::filter(generated) %>%
          dplyr::filter(
            datetime > datetime_cutoff_bch - h_short,
            datetime < datetime_cutoff_bsv + h_short
          )
        
        epoch_currency_target_short <-
          rbind(epoch_currency_target_bch_short,
                epoch_currency_target_bsv_short)
        
        
        # plot BCH and BSV at the same time
        
        rdmcplot_custom(
          Y = epoch_currency_target$winning_rate,
          X = epoch_currency_target$datetime %>% as.numeric(),
          C = c(datetime_cutoff_bch, datetime_cutoff_bsv) %>% as.numeric(),
          xlabel = "Datetime",
          ylabel = "Winning_rate"
          #binselectvec = binselect,
        )
        
        rdmcplot_custom(
          Y = epoch_currency_target$winning_rate * epoch_currency_target$reward_quoteusd,
          X = epoch_currency_target$datetime %>% as.numeric(),
          C = c(datetime_cutoff_bch, datetime_cutoff_bsv) %>% as.numeric(),
          xlabel = "Datetime",
          ylabel = "Expected reward (USD)"
          #binselectvec = binselect
        )
        
        rdmcplot_custom(
          Y = epoch_currency_target_short$block_time,
          X = epoch_currency_target_short$datetime %>% as.numeric(),
          C = c(datetime_cutoff_bch, datetime_cutoff_bsv) %>% as.numeric(),
          xlabel = "Datetime",
          ylabel = "Block time (s)"
          #binselectvec = binselect
        )
        
        rdmcplot_custom(
          Y = epoch_currency_target_short$hash_rate %>% log(),
          X = epoch_currency_target_short$datetime %>% as.numeric(),
          C = c(datetime_cutoff_bch, datetime_cutoff_bsv) %>% as.numeric(),
          xlabel = "Datetime",
          ylabel = "Log of hash rate (H/s)"
          #binselectvec = binselect
        )
      } else {
        stop("setting is invalid")
      }
    } else {
      stop("setting is invalid")
    }
    
    if (figure == TRUE) {
      ggsave(
        path = path,
        filename = figurename[1],
        plot = winning_rate_result,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[2],
        plot = long_winning_rate_figure,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[3],
        plot = expected_reward_result,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[4],
        plot = long_expected_reward_figure,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[5],
        plot = block_time_result,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[6],
        plot = long_block_time_figure,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[7],
        plot = log_of_hash_rate_result,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[8],
        plot = long_log_of_hash_rate_figure,
        width = figure_width,
        height = figure_height
      )
    }
    
  }


update_hash_rate_reduced <-
  function(epoch_currency_n,
           theta,
           h_variable) {
    # add empty currency
    currency_missing <-
      setdiff(c("BTC", "BCH", "BSV"),
              epoch_currency_n %>%
                dplyr::pull(currency) %>%
                unique())
    epoch <-
      epoch_currency_n %>%
      dplyr::pull(epoch) %>%
      max()
    epoch_currency_n <-
      dplyr::bind_rows(epoch_currency_n,
                       tibble::tibble(currency = currency_missing,
                                      epoch = epoch))
    
    # transform to wide format
    algo_epoch_variables_n <-
      epoch_currency_n %>%
      dplyr::mutate(reward_quoteusd_expected = winning_rate * reward_quoteusd) %>%
      tidyr::pivot_wider(
        id_cols = epoch,
        names_from = currency,
        values_from = c(winning_rate, reward_quoteusd_expected)
      )
    # join the wide reward
    algo_epoch_variables_n <-
      epoch_currency_n %>%
      dplyr::left_join(algo_epoch_variables_n,
                       by = "epoch")
    # make model matrix
    options(na.action = 'na.pass')
    x_variable <-
      model.matrix(object = h_variable,
                   data = algo_epoch_variables_n)
    x_variable[is.na(x_variable)] <- 0
    # compute the predicted log hash
    log_h <-
      x_variable %*% theta %>%
      as.numeric()
    # update the hash rate
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::mutate(hash_rate = exp(log_h)) %>%
      dplyr::filter(!(currency %in% currency_missing))
    return(epoch_currency_n)
  }

# update an epoch
update_epoch_stochastic_reduced <-
  function(epoch_currency,
           daa,
           theta,
           h_variable,
           currency_base,
           keep_reward,
           anchor = NULL) {
    # filter the last epoch
    epoch_currency_n <-
      epoch_currency %>%
      dplyr::filter(epoch == max(epoch))
    
    # update winning rate by passing daa functions
    epoch_currency_n <-
      update_winning_rate(epoch_currency_n,
                          epoch_currency,
                          daa,
                          anchor)
    
    # update reward
    if (keep_reward == TRUE) {
      epoch_currency_n <-
        update_reward_simulation(epoch_currency_n,
                                 currency_base)
    } else {
      epoch_currency_n <-
        update_reward(epoch_currency_n)
    }
    
    # update exogenous state variables
    epoch_currency_n <-
      update_exogenous_stochastic(base_epoch_currency_n = epoch_currency_n)
    
    # update hash rate
    epoch_currency_n <-
      update_hash_rate_reduced(epoch_currency_n,
                               theta,
                               h_variable)
    
    # update epoch time
    epoch_currency_n <-
      update_epoch_time_stochastic(epoch_currency_n)
    
    # simulated
    epoch_currency_n <-
      epoch_currency_n %>%
      dplyr::mutate(simulated = TRUE)
    
    # bind new epoch
    epoch_currency <-
      dplyr::bind_rows(epoch_currency,
                       epoch_currency_n)
    return(epoch_currency)
  }

# simulate epoch
simulate_epoch_stochastic_reduced <-
  function(epoch_currency,
           daa,
           theta,
           h_variable,
           horizon,
           currency_base,
           keep_reward,
           anchor = NULL) {
    epoch_currency <-
      epoch_currency %>%
      dplyr::mutate(simulated = FALSE)
    for (t in 1:horizon) {
      # update an epoch
      epoch_currency <-
        update_epoch_stochastic_reduced(epoch_currency,
                                        daa,
                                        theta,
                                        h_variable,
                                        currency_base,
                                        keep_reward,
                                        anchor)
    }
    # update block time
    epoch_currency <-
      epoch_currency %>%
      dplyr::group_by(currency, generated) %>%
      dplyr::arrange(currency, generated, datetime) %>%
      dplyr::mutate(block_time = difftime(datetime, dplyr::lag(datetime), unit = "secs") %>% as.numeric()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(block_time = ifelse(generated, block_time, NA))
    
    # arrange by epoch and currency
    epoch_currency <-
      epoch_currency %>%
      dplyr::arrange(epoch, currency)
    
    return(epoch_currency)
  }

grep2 <-
  function(x,
           y,
           ...) {
    unlist(lapply(x,
                  grep,
                  y,
                  ...))
  }

plot_around_halving <-
  function(epoch_currency,
           target,
           base,
           scenario,
           palette,
           palette_ideal,
           linestyle,
           figurename,
           fontsize = NULL,
           colorsize = NULL,
           linesize = NULL,
           legend = "bottom",
           nrow = NULL,
           figure = FALSE,
           figure_width = 4,
           figure_height = 3,
           legend_width = 5.0,
           legend_height = 0.5,
           begin = 0,
           end = 0.95,
           path = NULL) {
    epoch_currency_target <-
      epoch_currency %>%
      dplyr::filter(currency == target)
    
    p <- list()
    
    if (base == "BTC") {
      # wining rate

      p$winning_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(winning_rate),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of winning rate",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete=TRUE
          ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
          )
        )
      
      if (legend == "none") {
        
        p$legend <- ggpubr::get_legend(p$winning_rate)
        p$legend <- ggpubr::as_ggplot(p$legend)
        
        p$winning_rate <-
          p$winning_rate +
          theme(
            legend.position = "none"
          )
      }
      
      # expected reward
      p$expected_reward <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(winning_rate * reward_quoteusd),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of expected reward (USD)",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete = TRUE
        ) +
        theme_classic() +
        theme(
          legend.position = legend,
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
            )
          )
      
      # hash rate
      p$hash_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(hash_rate),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of hash rate (H/s)",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete = TRUE
        ) +
        theme_classic() +
        theme(
          legend.position = legend,
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
          )
        )
      
      # expected block time
      if (target == "BTC") {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = as.numeric(datetime_relative),
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line(size = linesize) +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               group = "Setting") +
          scale_color_viridis(
            begin = begin, 
            end = end,
            option = "D",
            discrete=TRUE
          ) +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          geom_hline(yintercept = 600, linetype = "dotted")
        
      } else {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = as.numeric(datetime_relative),
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line(size = linesize) +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               group = "Setting") +
          scale_color_viridis(
            begin = begin, 
            end = end,
            option = "D",
            discrete=TRUE
          ) +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title=element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          geom_hline(yintercept = 600, linetype = "dotted")
      }
      
      
      # blockheight
      if (target == "BTC") {
        
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              630000 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(group = "ideal",
                color = "black")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(
            values = palette_ideal,
            guide = guide_legend(override.aes = list(linetype = linestyle)
                                 )
            ) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               group = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title=element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) + 
          ylim(630000, 645173)
        

      } else if (target == "BCH") {
        
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          geom_function(
            fun = function(x)
              634702 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(color = "ideal")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(values = palette_ideal,
                             guide = guide_legend(override.aes = list(linetype = linestyle))) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title=element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) + 
          ylim(630000, 645173)
      
        
      } else if (target == "BSV") {
        
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              634459 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(group = "ideal")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(values = palette_ideal,
                             guide = guide_legend(override.aes = list(linetype = linestyle))) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          )
      
        
      }
    }
    else {
      
      # wining rate
      p$winning_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(winning_rate),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of winning rate",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete=TRUE
        ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
          )
        )
      
      if (legend == "none") {
        
        p$legend <- ggpubr::get_legend(p$winning_rate)
        p$legend <- ggpubr::as_ggplot(p$legend)
        
        p$winning_rate <-
          p$winning_rate +
          theme(
            legend.position = "none"
          )
      }
      
      # expected reward
      p$expected_reward <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(winning_rate * reward_quoteusd),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of expected reward (USD)",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete=TRUE
        ) +
        theme_classic() +
        theme(
          legend.position = legend,
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
          )
        )
      
      # hash rate
      p$hash_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = as.numeric(datetime_relative),
          y = log(hash_rate),
          color = setting
        )) +
        geom_line(size = linesize) +
        labs(x = "Days relative to the halving",
             y = "Log of hash rate (H/s)",
             group = "Setting") +
        scale_color_viridis(
          begin = begin, 
          end = end,
          option = "D",
          discrete=TRUE
        ) +
        theme_classic() +
        theme(
          legend.position = legend,
          legend.text = element_text(size = fontsize),
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(
            override.aes = list(size = colorsize),
            nrow = nrow
          )
        )
      
      # expected block time
      if (target == "BTC") {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = as.numeric(datetime_relative),
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line(size = linesize) +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               group = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          geom_hline(yintercept = 600, linetype = "dotted")
        
      } else {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = as.numeric(datetime_relative),
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line(size = linesize) +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               group = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          geom_hline(yintercept = 600, linetype = "dotted")
      }
      
      
      # blockheight
      if (target == "BTC") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              630000 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(color = "ideal")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(values = palette_ideal,
                             guide = guide_legend(override.aes = list(linetype = linestyle))) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          ylim(630000, 645173)
        
        if (legend == "none") {
          
          p$legend <- ggpubr::get_legend(p$blockheight)
          p$legend <- ggpubr::as_ggplot(p$legend)
          
          p$blockheight <-
            p$blockheight +
            theme(
              legend.position = "none"
            )
        }
        
      } else if (target == "BCH") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          geom_function(
            fun = function(x)
              634702 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(color = "ideal")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(values = palette_ideal,
                             guide = guide_legend(override.aes = list(linetype = linestyle))) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title = element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          ) +
          ylim(630000, 645173)
        
        if (legend == "none") {
          
          p$legend <- ggpubr::get_legend(p$blockheight)
          p$legend <- ggpubr::as_ggplot(p$legend)
          
          p$blockheight <-
            p$blockheight +
            theme(
              legend.position = "none"
            )
        }
        
      } else if (target == "BSV") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = as.numeric(datetime_relative),
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              634459 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(group = "ideal")
          ) +
          geom_line(size = linesize) +
          scale_color_manual(values = palette_ideal,
                             guide = guide_legend(override.aes = list(linetype = linestyle))) +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(
            legend.position = legend,
            legend.text = element_text(size = fontsize),
            legend.title=element_blank()
          ) +
          guides(
            color = guide_legend(
              override.aes = list(size = colorsize),
              nrow = nrow
            )
          )
        
        if (legend == "none") {
          
          p$legend <- ggpubr::get_legend(p$blockheight)
          p$legend <- ggpubr::as_ggplot(p$legend)
          
          p$blockheight <-
            p$blockheight +
            theme(
              legend.position = "none"
            )
        }
      }
      
    }
    
    if (figure == TRUE) {
      ggsave(
        path = path,
        filename = figurename[1],
        plot = p$winning_rate,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[2],
        plot = p$expected_reward,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[3],
        plot = p$hash_rate,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[4],
        plot = p$expected_block_time,
        width = figure_width,
        height = figure_height
      )
      
      ggsave(
        path = path,
        filename = figurename[5],
        plot = p$blockheight,
        width = figure_width,
        height = figure_height
      )
      
      if (legend == "none"){
        ggsave(
          path = path,
          filename = figurename[6],
          plot = p$legend,
          width = legend_width,
          height = legend_height
        ) 
      }
    }
    
    return(p)
  }

# plot around halving for asert daa regime

plot_around_halving_asert <-
  function(epoch_currency,
           target,
           base,
           scenario) {
    epoch_currency_target <-
      epoch_currency %>%
      dplyr::filter(currency == target)
    
    p <- list()
    
    if (base == "BTC") {
      if (target == "BTC") {
        # wining rate
        p$winning_rate <-
          epoch_currency_target %>%
          ggplot(aes(
            x = as.numeric(datetime_relative),
            y = log(winning_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of winning rate",
               group = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        #ylim(-52.7, -52)
        
        # expected reward
        p$expected_reward <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = log(winning_rate * reward_quoteusd),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of expected reward (USD)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        #ylim(-41.8, -41)
        
        # hash rate
        p$hash_rate <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = log(hash_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of hash rate (H/s)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        #ylim(45.5, 47)
        
        # expected block time
        if (target == "BTC") {
          p$expected_block_time <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = 1 / (winning_rate * hash_rate),
              color = setting
            )) +
            geom_line() +
            scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Expected block time (s)",
                 colour = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            geom_hline(yintercept = 600, linetype = "dotted")
        } else {
          p$expected_block_time <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = 1 / (winning_rate * hash_rate),
              color = setting
            )) +
            geom_line() +
            scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Expected block time (s)",
                 colour = "Setting") +
            theme_classic() +
            theme(legend.position = legend)
        }
        
        
        # blockheight
        if (target == "BTC") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            stat_function(
              fun = function(x)
                630000 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(color = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            ylim(630000, 645173)
          
        } else if (target == "BCH") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            geom_function(
              fun = function(x)
                634702 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(color = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            ylim(630000, 645173)
          
        } else if (target == "BSV") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            stat_function(
              fun = function(x)
                634459 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(colour = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend)
        }
      } else {
        # wining rate
        p$winning_rate <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = log(winning_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of winning rate",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        
        # expected reward
        p$expected_reward <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = log(winning_rate * reward_quoteusd),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of expected reward (USD)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        
        # hash rate
        p$hash_rate <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = log(hash_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Log of hash rate (H/s)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = legend)
        
        # expected block time
        if (target == "BTC") {
          p$expected_block_time <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = 1 / (winning_rate * hash_rate),
              color = setting
            )) +
            geom_line() +
            scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Expected block time (s)",
                 colour = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            geom_hline(yintercept = 600, linetype = "dotted")
        } else {
          p$expected_block_time <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = 1 / (winning_rate * hash_rate),
              color = setting
            )) +
            geom_line() +
            scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Expected block time (s)",
                 colour = "Setting") +
            theme_classic() +
            theme(legend.position = legend)
        }
        
        
        # blockheight
        if (target == "BTC") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            stat_function(
              fun = function(x)
                630000 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(color = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            #scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            ylim(630000, 645173)
          
        } else if (target == "BCH") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            geom_function(
              fun = function(x)
                634702 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(color = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            #scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend) +
            ylim(630000, 645173)
          
        } else if (target == "BSV") {
          p$blockheight <-
            epoch_currency_target %>%
            ggplot(aes(
              x = datetime_relative,
              y = blockheight,
              color = setting
            )) +
            stat_function(
              fun = function(x)
                634459 + 144 * x,
              alpha = 0.6,
              linetype = "dashed",
              size = 0.8,
              aes(colour = "ideal")
            ) +
            geom_line() +
            scale_color_manual(
              values = c(viridis::viridis(scenario),
                         "black"),
              guide = guide_legend(override.aes = list(linetype = c(
                rep("solid",
                    scenario),
                "dashed"
              )))
            ) +
            #scale_color_viridis_d() +
            labs(x = "Days relative to the halving",
                 y = "Block height",
                 color = "Setting") +
            theme_classic() +
            theme(legend.position = legend)
        }
      }
    } else {
      # wining rate
      p$winning_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = datetime_relative,
          y = log(winning_rate),
          color = setting
        )) +
        geom_line() +
        scale_color_viridis_d() +
        labs(x = "Days relative to the halving",
             y = "Log of winning rate",
             colour = "Setting") +
        theme_classic() +
        theme(legend.position = legend)
      
      # expected reward
      p$expected_reward <-
        epoch_currency_target %>%
        ggplot(aes(
          x = datetime_relative,
          y = log(winning_rate * reward_quoteusd),
          color = setting
        )) +
        geom_line() +
        scale_color_viridis_d() +
        labs(x = "Days relative to the halving",
             y = "Log of expected reward (USD)",
             colour = "Setting") +
        theme_classic() +
        theme(legend.position = "bottom")
      
      # hash rate
      p$hash_rate <-
        epoch_currency_target %>%
        ggplot(aes(
          x = datetime_relative,
          y = log(hash_rate),
          color = setting
        )) +
        geom_line() +
        scale_color_viridis_d() +
        labs(x = "Days relative to the halving",
             y = "Log of hash rate (H/s)",
             colour = "Setting") +
        theme_classic() +
        theme(legend.position = "bottom")
      
      # expected block time
      if (target == "BTC") {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = "bottom") +
          geom_hline(yintercept = 600, linetype = "dotted")
      } else {
        p$expected_block_time <-
          epoch_currency_target %>%
          ggplot(aes(
            x = datetime_relative,
            y = 1 / (winning_rate * hash_rate),
            color = setting
          )) +
          geom_line() +
          scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Expected block time (s)",
               colour = "Setting") +
          theme_classic() +
          theme(legend.position = "bottom")
      }
      
      
      # blockheight
      if (target == "BTC") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = datetime_relative,
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              630000 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(color = "ideal")
          ) +
          geom_line() +
          scale_color_manual(
            values = c(viridis::viridis(scenario),
                       "black"),
            guide = guide_legend(override.aes = list(linetype = c(
              rep("solid",
                  scenario),
              "dashed"
            )))
          ) +
          #scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(legend.position = "bottom") +
          ylim(630000, 645173)
        
      } else if (target == "BCH") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = datetime_relative,
                     y = blockheight,
                     color = setting)) +
          geom_function(
            fun = function(x)
              634702 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(color = "ideal")
          ) +
          geom_line() +
          scale_color_manual(
            values = c(viridis::viridis(scenario),
                       "black"),
            guide = guide_legend(override.aes = list(linetype = c(
              rep("solid",
                  scenario),
              "dashed"
            )))
          ) +
          #scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(legend.position = "bottom") +
          ylim(630000, 645173)
        
      } else if (target == "BSV") {
        p$blockheight <-
          epoch_currency_target %>%
          ggplot(aes(x = datetime_relative,
                     y = blockheight,
                     color = setting)) +
          stat_function(
            fun = function(x)
              634459 + 144 * x,
            alpha = 0.6,
            linetype = "dashed",
            size = 0.8,
            aes(colour = "ideal")
          ) +
          geom_line() +
          scale_color_manual(
            values = c(viridis::viridis(scenario),
                       "black"),
            guide = guide_legend(override.aes = list(linetype = c(
              rep("solid",
                  scenario),
              "dashed"
            )))
          ) +
          #scale_color_viridis_d() +
          labs(x = "Days relative to the halving",
               y = "Block height",
               color = "Setting") +
          theme_classic() +
          theme(legend.position = "bottom")
      }
      
    }
    
    return(p)
  }

# update winning rate
update_winning_rate_bch_uses_asert_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor) {
    # set dates
    blockheight_bch_start <- 478558
    emergency_daa_end <-
      as.POSIXct(1510600000, origin = "1970-01-01", tz = "UTC")
    
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bch_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BTC") %>%
        dplyr::filter(blockheight < blockheight_bch_start,
                      blockheight >= blockheight_bch_start - 2016) %>%
        dplyr::mutate(currency = "BCH")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_asert_daa(
        epoch_n,
        currency_n,
        blockheight_n,
        generated_n,
        winning_rate_n,
        epoch_currency = epoch_currency_check,
        blockheight_anchor = anchor$BCH$blockheight_anchor,
        datetime_anchor = anchor$BCH$datetime_anchor,
        bits_anchor = anchor$BCH$bits_anchor
      )
    
    return(winning_rate_n)
  }

# update winning rate
update_winning_rate_bsv_uses_asert_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor) {
    # set dates
    blockheight_bsv_start <- 556766
    epoch_currency_check <- epoch_currency
    
    # copy blockchain headers
    if (blockheight_n < blockheight_bsv_start + 2016) {
      epoch_currency_add <-
        epoch_currency %>%
        dplyr::filter(currency == "BCH") %>%
        dplyr::filter(blockheight < blockheight_bsv_start,
                      blockheight >= blockheight_bsv_start - 2016) %>%
        dplyr::mutate(currency = "BSV")
      epoch_currency_check <-
        epoch_currency_check %>%
        dplyr::bind_rows(epoch_currency_add) %>%
        dplyr::arrange(epoch)
      
    }
    # apply daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_asert_daa(
        epoch_n,
        currency_n,
        blockheight_n,
        generated_n,
        winning_rate_n,
        epoch_currency = epoch_currency_check,
        blockheight_anchor = anchor$BSV$blockheight_anchor,
        datetime_anchor = anchor$BSV$datetime_anchor,
        bits_anchor = anchor$BSV$bits_anchor
      )
    
    return(winning_rate_n)
  }

update_winning_rate_btc_uses_asert_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           # not used
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor) {
    # apply daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_asert_daa(
        epoch_n,
        currency_n,
        blockheight_n,
        generated_n,
        winning_rate_n,
        epoch_currency,
        blockheight_anchor = anchor$BTC$blockheight_anchor,
        datetime_anchor = anchor$BTC$datetime_anchor,
        bits_anchor = anchor$BTC$bits_anchor
      )
    
    return(winning_rate_n)
  }

update_winning_rate_btc_uses_bch_daa <-
  function(epoch_n,
           currency_n,
           blockheight_n,
           datetime_n,
           # not used
           generated_n,
           winning_rate_n,
           epoch_currency,
           anchor = NULL) {
    # apply bitcoin cash daa
    winning_rate_n <-
      update_winning_rate_bitcoin_cash_daa(epoch_n,
                                           currency_n,
                                           blockheight_n,
                                           generated_n,
                                           winning_rate_n,
                                           epoch_currency)
    return(winning_rate_n)
  }


# From rdmcplot library

rdmcplot_custom <-
  function(Y,
           X,
           C,
           nbinsmat = NULL,
           binselectvec = NULL,
           scalevec = NULL,
           supportmat = NULL,
           pvec = NULL,
           hmat = NULL,
           kernelvec = NULL,
           weightsvec = NULL,
           covs_mat = NULL,
           covs_list = NULL,
           covs_evalvec = NULL,
           covs_dropvec = NULL,
           ci = NULL,
           col_bins = NULL,
           pch_bins = NULL,
           col_poly = NULL,
           lty_poly = NULL,
           col_xline = NULL,
           lty_xline = NULL,
           nobins = FALSE,
           nopoly = FALSE,
           noxline = FALSE,
           nodraw = FALSE,
           xlabel = NULL,
           ylabel = NULL) {
    # Setup and error checking
    
    
    if (!is.numeric(C)) {
      stop('C has to be numeric')
    }
    if (max(C, na.rm = TRUE) >= max(X, na.rm = TRUE) |
        min(C, na.rm = TRUE) <= min(X, na.rm = TRUE)) {
      stop('cutoff variable outside range of running variable')
    }
    
    clist <- sort(unique(C))
    cnum <- length(clist)
    
    D <- as.numeric(X >= C)
    
    if (is.null(pvec)) {
      pvec = rep(4, cnum)
    }
    if (!is.null(hmat)) {
      if (is.null(dim(hmat))) {
        hmat <- matrix(hmat, nrow = cnum, ncol = 2)
      }
      haux <- hmat
    }  else{
      haux <- matrix(Inf, ncol = 2, nrow = cnum)
    }
    if (!is.null(nbinsmat)) {
      if (is.null(dim(nbinsmat))) {
        nbinsmat <- matrix(nbinsmat, nrow = cnum, ncol = 2)
      }
    }
    if (is.null(binselectvec))
      binselectvec <- rep('esmv', cnum)
    if (is.null(scalevec))
      scalevec <- rep(1, cnum)
    if (is.null(kernelvec))
      kernelvec <- rep('uni', cnum)
    if (is.null(covs_evalvec))
      covs_evalvec <- rep(0, cnum)
    if (is.null(covs_dropvec))
      covs_dropvec <- rep(TRUE, cnum)
    if (!is.null(covs_mat)) {
      covs_mat <- as.matrix(covs_mat)
      if (!is.null(covs_list)) {
        if (length(covs_list) != cnum)
          stop('Elements in covs_list should equal number of cutoffs')
      }
    }
    
    X0 <- matrix(NA, nrow = length(Y), ncol = cnum)
    X1 <- matrix(NA, nrow = length(Y), ncol = cnum)
    YHAT0 <- matrix(NA, nrow = length(Y), ncol = cnum)
    YHAT1 <- matrix(NA, nrow = length(Y), ncol = cnum)
    XMEAN <- matrix(NA, nrow = length(Y), ncol = cnum)
    YMEAN <- matrix(NA, nrow = length(Y), ncol = cnum)
    CI_l <- matrix(NA, nrow = length(Y), ncol = cnum)
    CI_r <- matrix(NA, nrow = length(Y), ncol = cnum)
    Cfail <- numeric()
    
    # Construct variables for plots
    
    
    count <- 1
    count_fail <- 0
    for (c in clist) {
      yc <- Y[C == c & X <= c + haux[count, 2] & X >= c - haux[count, 1]]
      xc <- X[C == c & X <= c + haux[count, 2] & X >= c - haux[count, 1]]
      dc <- D[C == c & X <= c + haux[count, 2] & X >= c - haux[count, 1]]
      yc0 <- yc[dc == 0]
      yc1 <- yc[dc == 1]
      xc0 <- xc[dc == 0]
      xc1 <- xc[dc == 1]
      
      if (!is.null(covs_mat)) {
        covs_mat_c <-
          covs_mat[C == c & X <= c + haux[count, 2] & X >= c - haux[count, 1], ]
        if (!is.null(covs_list)) {
          covs_aux <- covs_mat_c[, covs_list[[count]]]
        } else{
          covs_aux <- covs_mat_c
        }
      } else {
        covs_aux <- NULL
      }
      
      aux <- try(rdrobust::rdplot(
        yc,
        xc,
        c = c,
        nbins = nbinsmat[count, ],
        binselect = binselectvec[count],
        scale = scalevec[count],
        support = supportmat[count, ],
        p = pvec[count],
        h = hmat[count, ],
        kernel = kernelvec[count],
        weights = weightsvec[count],
        covs = covs_aux,
        covs_eval = covs_evalvec[count],
        covs_drop = covs_dropvec[count],
        ci = ci,
        hide = TRUE
      ),
      silent = TRUE)
      
      if (class(aux) != "try-error") {
        xmean <- aux$vars_bins[, 2]
        ymean <- aux$vars_bins[, 3]
        
        xmean <- xmean[!is.na(xmean)]
        ymean <- ymean[!is.na(ymean)]
        
        x0 <- aux$vars_poly[aux$vars_poly[, 1] < c, 1]
        yhat0 <- aux$vars_poly[aux$vars_poly[, 1] < c, 2]
        x1 <- aux$vars_poly[aux$vars_poly[, 1] > c, 1]
        yhat1 <- aux$vars_poly[aux$vars_poly[, 1] > c, 2]
        
        length(xmean) <- length(Y)
        length(ymean) <- length(Y)
        length(x0) <- length(Y)
        length(yhat0) <- length(Y)
        length(x1) <- length(Y)
        length(yhat1) <- length(Y)
        
        XMEAN[, count] <- xmean
        YMEAN[, count] <- ymean
        X0[, count] <- x0
        X1[, count] <- x1
        YHAT0[, count] <- yhat0
        YHAT1[, count] <- yhat1
        
        if (!is.null(ci)) {
          ci_l <- aux$vars_bins[, 8]
          ci_r <- aux$vars_bins[, 9]
          length(ci_l) <- length(Y)
          length(ci_r) <- length(Y)
          CI_l[, count] <- ci_l
          CI_r[, count] <- ci_r
        }
      } else{
        Cfail <- c(Cfail, c)
        count_fail <- count_fail + 1
      }
      
      count <- count + 1
      
    }
    
    Xmean <- data.frame(XMEAN)
    Xmean <- Xmean[1:max(colSums(!is.na(Xmean))), ]
    names(Xmean) <- paste0(rep("Xmean"), 1:cnum)
    Ymean <- data.frame(YMEAN)
    Ymean <- Ymean[1:max(colSums(!is.na(Ymean))), ]
    names(Ymean) <- paste0(rep("Ymean"), 1:cnum)
    X0    <- data.frame(X0)
    X0    <- X0[1:max(colSums(!is.na(X0))), ]
    names(X0) <- paste0(rep("X0_"), 1:cnum)
    X1    <- data.frame(X1)
    X1    <- X1[1:max(colSums(!is.na(X1))), ]
    names(X1) <- paste0(rep("X1_"), 1:cnum)
    Yhat0    <- data.frame(YHAT0)
    Yhat0    <- Yhat0[1:max(colSums(!is.na(Yhat0))), ]
    names(Yhat0) <- paste0(rep("Yhat0_"), 1:cnum)
    Yhat1    <- data.frame(YHAT1)
    Yhat1    <- Yhat1[1:max(colSums(!is.na(Yhat1))), ]
    names(Yhat1) <- paste0(rep("Yhat1_"), 1:cnum)
    if (!is.null(ci)) {
      CI_l <- data.frame(CI_l)
      CI_l <- CI_l[1:max(colSums(!is.na(CI_l))), ]
      names(CI_l) <- paste0(rep("CI_l_"), 1:cnum)
      CI_r <- data.frame(CI_r)
      CI_r <- CI_r[1:max(colSums(!is.na(CI_r))), ]
      names(CI_r) <- paste0(rep("CI_r_"), 1:cnum)
    }
    
    
    # Plots
    
    
    colorlist <-
      c(
        'darkblue',
        'darkred',
        'darkgreen',
        'darkorange',
        'gray50',
        'khaki4',
        'brown3',
        'blue',
        'darkgoldenrod4',
        'cyan4'
      )
    
    if (is.null(col_bins)) {
      col_bins <- colorlist
    }
    if (is.null(pch_bins)) {
      pch_bins <- rep(1, cnum)
    }
    if (is.null(col_poly)) {
      col_poly <- colorlist
    }
    if (is.null(lty_poly)) {
      lty_poly <- rep('solid', cnum)
    }
    if (is.null(col_xline)) {
      col_xline <- colorlist
    }
    if (is.null(lty_xline)) {
      lty_xline <- rep('dashed', cnum)
    }
    
    rdmc_plot <-
      ggplot() +
      theme_bw() +
      labs(x = xlabel, y = ylabel)
    
    if (nobins == FALSE) {
      for (c in 1:cnum) {
        rdmc_plot <-
          rdmc_plot + geom_point(
            aes_string(x = Xmean[, c], y = Ymean[, c]),
            col = col_bins[c],
            shape = pch_bins[c],
            na.rm = TRUE
          )
      }
    }
    if (!is.null(ci)) {
      for (c in 1:cnum) {
        rdmc_plot <-
          rdmc_plot + geom_errorbar(
            aes_string(
              x = Xmean[, c],
              ymin = CI_l[, c],
              ymax = CI_r[, c]
            ),
            col = col_bins[c],
            linetype = 1
          )
      }
    }
    if (nopoly == FALSE) {
      for (c in 1:cnum) {
        rdmc_plot <-
          rdmc_plot + geom_line(
            aes_string(x = X0[, c], y = Yhat0[, c]),
            col = col_poly[c],
            linetype = lty_poly[c],
            na.rm = TRUE
          ) +
          geom_line(
            aes_string(x = X1[, c], y = Yhat1[, c]),
            col = col_poly[c],
            linetype = lty_poly[c],
            na.rm = TRUE
          )
      }
    }
    if (noxline == FALSE) {
      for (c in 1:cnum) {
        rdmc_plot <-
          rdmc_plot + geom_vline(xintercept = clist[c],
                                 col = col_xline[c],
                                 linetype = lty_xline[c])
      }
    }
    
    if (nodraw == FALSE) {
      plot(rdmc_plot +
             ggplot2::scale_x_continuous(
               labels = function(x)
                 format(as.POSIXct(x, origin = "1970-1-1"), format = "%Y-%m-%d")
             ))
    }
    
    if (count_fail > 0) {
      warning("rdplot() could not run in one or more cutoffs.")
    }
    
    # Return values
    
    
    if (is.null(ci)) {
      output <- list(rdmc_plot = rdmc_plot)
    }  else {
      output <- list(
        clist = clist,
        cnum = cnum,
        X0 = X0,
        X1 = X1,
        Yhat0 = Yhat0,
        Yhat1 = Yhat1,
        Xmean = Xmean,
        Ymean = Ymean,
        rdmc_plot = rdmc_plot,
        CI_l = CI_l,
        CI_r = CI_r,
        cfail = Cfail
      )
    }
    
    
    return(output)
  }


rdplot_custom = function(y,
                         x,
                         c = 0,
                         p = 4,
                         nbins = NULL,
                         binselect = "esmv",
                         scale = NULL,
                         kernel = "uni",
                         weights = NULL,
                         h = NULL,
                         covs = NULL,
                         covs_eval = "mean",
                         covs_drop = TRUE,
                         ginv.tol = 1e-20,
                         support = NULL,
                         subset = NULL,
                         masspoints = "adjust",
                         hide = FALSE,
                         ci = NULL,
                         shade = FALSE,
                         title = NULL,
                         x.label = NULL,
                         y.label = NULL,
                         x.lim = NULL,
                         y.lim = NULL,
                         col.dots = NULL,
                         col.lines = NULL) {
  if (!is.null(subset)) {
    x <- x[subset]
    y <- y[subset]
  }
  na.ok <- complete.cases(x) & complete.cases(y)
  
  if (!is.null(covs)) {
    if (!is.null(subset))
      covs <- subset(covs, subset)
    na.ok <- na.ok & complete.cases(covs)
  }
  
  if (!is.null(weights)) {
    if (!is.null(subset))
      weights <- weights[subset]
    na.ok <- na.ok & complete.cases(weights) & weights >= 0
  }
  
  
  x <- x[na.ok]
  y <- y[na.ok]
  
  if (!is.null(covs))
    covs    = as.matrix(covs)[na.ok, , drop = FALSE]
  if (!is.null(weights))
    weights = as.matrix(weights[na.ok])
  
  x_min = min(x)
  x_max = max(x)
  x_l = x[x < c]
  x_r = x[x >= c]
  y_l = y[x < c]
  y_r = y[x >= c]
  
  if (!is.null(support)) {
    support_l = support[1]
    support_r = support[2]
    if (support_l < x_min)
      x_min = support_l
    if (support_r > x_max)
      x_max = support_r
  }
  
  range_l = c - x_min
  range_r = x_max - c
  
  n_l = length(x_l)
  n_r = length(x_r)
  n = n_l + n_r
  meth = "es"
  
  if (is.null(scale)) {
    scale = scale_l = scale_r = 1
  } else{
    if (length(scale) == 1)
      scale_l = scale_r = scale
    if (length(scale) == 2) {
      scale_l = scale[1]
      scale_r = scale[2]
    }
  }
  
  if (!is.null(nbins)) {
    if (length(nbins) == 1)
      nbins_l = nbins_r = nbins
    if (length(nbins) == 2) {
      nbins_l = nbins[1]
      nbins_r = nbins[2]
    }
  }
  
  if (is.null(h)) {
    h_l = range_l
    h_r = range_r
  } else{
    if (length(h) == 1)
      h_l = h_r = h
    if (length(h) == 2) {
      h_l = h[1]
      h_r = h[2]
    }
  }
  
  
  flag_no_ci <- FALSE
  if (is.null(ci)) {
    ci <- 95
    flag_no_ci <- TRUE
  }
  
  kernel_type = "Uniform"
  if (kernel == "epanechnikov" |
      kernel == "epa")
    kernel_type = "Epanechnikov"
  if (kernel == "triangular" |
      kernel == "tri")
    kernel_type = "Triangular"
  
  
  ### Mass Points
  if (is.null(masspoints))
    masspoints = FALSE
  mN = n
  M_l = n_l
  M_r = n_r
  if (masspoints == "check" | masspoints == "adjust") {
    X_uniq_l = sort(unique(x_l), decreasing = TRUE)
    X_uniq_r = unique(x_r)
    M_l = length(X_uniq_l)
    M_r = length(X_uniq_r)
    M = M_l + M_r
    mass_l = 1 - M_l / n_l
    mass_r = 1 - M_r / n_r
    if (mass_l >= 0.2 | mass_r >= 0.2) {
      print("Mass points detected in the running variable.")
      if (masspoints == "check")
        print("Try using option masspoints=adjust.")
      if (masspoints == "adjust") {
        if (binselect == "es")
          binselect = "espr"
        if (binselect == "esmv")
          binselect = "esmvpr"
        if (binselect == "qs")
          binselect = "qspr"
        if (binselect == "qsmv")
          binselect = "qsmvpr"
      }
    }
  }
  
  
  
  
  ############## COLLINEARITY
  covs_drop_coll = dZ = 0
  if (covs_drop == TRUE)
    covs_drop_coll = 1
  if (!is.null(covs)) {
    covs.names = colnames(covs)
    if (is.null(covs.names)) {
      covs.names = paste("z", 1:ncol(covs), sep = "")
      colnames(covs) = covs.names
    }
    covs = covs[, order(nchar(covs.names))]
    covs = as.matrix(covs)
    dZ = length(covs.names)
    covs.check = covs_drop_fun(covs)
    if (covs.check$ncovs < dZ & covs_drop == FALSE) {
      print(
        "Multicollinearity issue detected in covs. Please rescale and/or remove redundant covariates, or use covs_drop option."
      )
    }
    if (covs.check$ncovs < dZ & isTRUE(covs_drop)) {
      covs  <- as.matrix(covs.check$covs)
      dZ    <- covs.check$ncovs
      #covs_drop_coll <-1
      #print("Multicollinearity issue detected in covs. Redundant covariates dropped.")
    }
  }
  
  
  #####********************* ERRORS
  exit = 0
  if (c <= x_min | c >= x_max) {
    print("c should be set within the range of x")
    exit = 1
  }
  
  if (kernel != "uni" &
      kernel != "uniform" &
      kernel != "tri" &
      kernel != "triangular" &
      kernel != "epa" & kernel != "epanechnikov" & kernel != "") {
    print("kernel incorrectly specified")
    exit = 1
  }
  
  if (p < 0) {
    print("p should be a positive number")
    exit = 1
  }
  
  if (scale <= 0 | scale_l <= 0 | scale_r <= 0) {
    print("scale should be a positive number")
    exit = 1
  }
  
  p_ceiling = ceiling(p) / p
  
  if (p_ceiling != 1 & p > 0) {
    print("p should be an integer number")
    exit = 1
  }
  
  if (n < 20) {
    print("Not enough observations to perform bin calculations")
    exit = 1
  }
  
  if (exit > 0)
    stop()
  
  ### Polynomial curve (order = p) ##################################
  R_p_l = matrix(NA, n_l, p + 1)
  R_p_r = matrix(NA, n_r, p + 1)
  for (j in 1:(p + 1)) {
    R_p_l[, j] = (x_l - c) ^ (j - 1)
    R_p_r[, j] = (x_r - c) ^ (j - 1)
  }
  
  W_h_l = rdrobust_kweight(x_l, c, h_l, kernel)
  W_h_r = rdrobust_kweight(x_r, c, h_r, kernel)
  
  n_h_l = sum(W_h_l > 0)
  n_h_r = sum(W_h_r > 0)
  
  if (!is.null(weights)) {
    fw_l = weights[x < c]
    fw_r = weights[x >= c]
    W_h_l = fw_l * W_h_l
    W_h_r = fw_r * W_h_r
  }
  invG_p_l  = qrXXinv((sqrt(W_h_l) * R_p_l))
  invG_p_r  = qrXXinv((sqrt(W_h_r) * R_p_r))
  
  if (is.null(covs)) {
    gamma_p1_l = invG_p_l %*% crossprod(R_p_l * W_h_l, y_l)
    gamma_p1_r = invG_p_r %*% crossprod(R_p_r * W_h_r, y_r)
  } else {
    z_l  = covs[x < c, ]
    z_r  = covs[x >= c, ]
    D_l  = cbind(y_l, z_l)
    D_r = cbind(y_r, z_r)
    U_p_l = crossprod(R_p_l * W_h_l, D_l)
    U_p_r = crossprod(R_p_r * W_h_r, D_r)
    beta_p_l = invG_p_l %*% crossprod(R_p_l * W_h_l, D_l)
    beta_p_r = invG_p_r %*% crossprod(R_p_r * W_h_r, D_r)
    
    
    ZWD_p_l  = crossprod(z_l * W_h_l, D_l)
    ZWD_p_r  = crossprod(z_r * W_h_r, D_r)
    colsZ = 2:max(c(2 + dZ - 1, 2))
    UiGU_p_l =  crossprod(U_p_l[, colsZ], invG_p_l %*% U_p_l)
    UiGU_p_r =  crossprod(U_p_r[, colsZ], invG_p_r %*% U_p_r)
    ZWZ_p_l = ZWD_p_l[, colsZ] - UiGU_p_l[, colsZ]
    ZWZ_p_r = ZWD_p_r[, colsZ] - UiGU_p_r[, colsZ]
    ZWY_p_l = ZWD_p_l[, 1] - UiGU_p_l[, 1]
    ZWY_p_r = ZWD_p_r[, 1] - UiGU_p_r[, 1]
    ZWZ_p = ZWZ_p_r + ZWZ_p_l
    ZWY_p = ZWY_p_r + ZWY_p_l
    if (covs_drop_coll == 0)
      gamma_p = chol2inv(chol(ZWZ_p)) %*% ZWY_p
    if (covs_drop_coll == 1)
      gamma_p = ginv(ZWZ_p, tol = ginv.tol) %*% ZWY_p
    s_Y = c(1 ,-gamma_p[, 1])
    
    gamma_p1_l = t(s_Y %*% t(beta_p_l))
    gamma_p1_r = t(s_Y %*% t(beta_p_r))
  }
  
  
  ### Preparte data for polynomial curve plot ###
  
  
  nplot = 500
  x_plot_l = seq(c - h_l, c, length.out = nplot)
  x_plot_r = seq(c, c + h_r, length.out = nplot)
  rplot_l = matrix(NA, nplot, p + 1)
  rplot_r = matrix(NA, nplot, p + 1)
  for (j in 1:(p + 1)) {
    rplot_l[, j] = (x_plot_l - c) ^ (j - 1)
    rplot_r[, j] = (x_plot_r - c) ^ (j - 1)
  }
  
  y_hat_l = rplot_l %*% gamma_p1_l
  y_hat_r = rplot_r %*% gamma_p1_r
  
  if (!is.null(covs) & covs_eval == "mean") {
    gammaZ = colMeans(covs) %*% gamma_p
    y_hat_l = rplot_l %*% gamma_p1_l + c(gammaZ)
    y_hat_r = rplot_r %*% gamma_p1_r + c(gammaZ)
  }
  
  ### Optimal Bins (using polynomial order k) ###
  
  k = 4
  rk_l = matrix(NA, n_l, (k + 1))
  rk_r = matrix(NA, n_r, (k + 1))
  for (j in 1:(k + 1)) {
    rk_l[, j] = x_l ^ (j - 1)
    rk_r[, j] = x_r ^ (j - 1)
  }
  
  invG_k_l = try(qrXXinv(rk_l), silent = TRUE)
  invG_k_r = try(qrXXinv(rk_r), silent = TRUE)
  
  if (class(invG_k_l)[1] == "try-error" |
      class(invG_k_r)[1] == "try-error") {
    k = 3
    rk_l = matrix(NA, n_l, (k + 1))
    rk_r = matrix(NA, n_r, (k + 1))
    for (j in 1:(k + 1)) {
      rk_l[, j] = x_l ^ (j - 1)
      rk_r[, j] = x_r ^ (j - 1)
    }
    invG_k_l = try(qrXXinv(rk_l), silent = TRUE)
    invG_k_r = try(qrXXinv(rk_r), silent = TRUE)
  }
  
  if (class(invG_k_l)[1] == "try-error" |
      class(invG_k_r)[1] == "try-error") {
    k = 2
    rk_l = matrix(NA, n_l, (k + 1))
    rk_r = matrix(NA, n_r, (k + 1))
    for (j in 1:(k + 1)) {
      rk_l[, j] = x_l ^ (j - 1)
      rk_r[, j] = x_r ^ (j - 1)
    }
    invG_k_l = qrXXinv(rk_l)
    invG_k_r = qrXXinv(rk_r)
  }
  
  gamma_k1_l = invG_k_l %*% crossprod(rk_l, y_l)
  gamma_k2_l = invG_k_l %*% crossprod(rk_l, y_l ^ 2)
  gamma_k1_r = invG_k_r %*% crossprod(rk_r, y_r)
  gamma_k2_r = invG_k_r %*% crossprod(rk_r, y_r ^ 2)
  
  
  ### Bias w/sample
  mu0_k1_l = rk_l %*% gamma_k1_l
  mu0_k1_r = rk_r %*% gamma_k1_r
  mu0_k2_l = rk_l %*% gamma_k2_l
  mu0_k2_r = rk_r %*% gamma_k2_r
  drk_l = matrix(NA, n_l, k)
  drk_r = matrix(NA, n_r, k)
  for (j in 1:k) {
    drk_l[, j] = j * x_l ^ (j - 1)
    drk_r[, j] = j * x_r ^ (j - 1)
  }
  
  ind_l = order(x_l)
  ind_r = order(x_r)
  x_i_l = x_l[ind_l]
  y_i_l = y_l[ind_l]
  x_i_r = x_r[ind_r]
  y_i_r = y_r[ind_r]
  
  dxi_l = (x_i_l[2:length(x_i_l)] - x_i_l[1:(length(x_i_l) - 1)])
  dxi_r = (x_i_r[2:length(x_i_r)] - x_i_r[1:(length(x_i_r) - 1)])
  dyi_l = (y_i_l[2:length(y_i_l)] - y_i_l[1:(length(y_i_l) - 1)])
  dyi_r = (y_i_r[2:length(y_i_r)] - y_i_r[1:(length(y_i_r) - 1)])
  
  x_bar_i_l = (x_i_l[2:length(x_i_l)] + x_i_l[1:(length(x_i_l) - 1)]) /
    2
  x_bar_i_r = (x_i_r[2:length(x_i_r)] + x_i_r[1:(length(x_i_r) - 1)]) /
    2
  
  drk_i_l = matrix(NA, n_l - 1, k)
  rk_i_l  = matrix(NA, n_l - 1, (k + 1))
  drk_i_r = matrix(NA, n_r - 1, k)
  rk_i_r  = matrix(NA, n_r - 1, (k + 1))
  
  for (j in 1:(k + 1)) {
    rk_i_l[, j] = x_bar_i_l ^ (j - 1)
    rk_i_r[, j] = x_bar_i_r ^ (j - 1)
  }
  
  for (j in 1:k) {
    drk_i_l[, j] = j * x_bar_i_l ^ (j - 1)
    drk_i_r[, j] = j * x_bar_i_r ^ (j - 1)
  }
  
  mu1_i_hat_l = drk_i_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_i_hat_r = drk_i_r %*% (gamma_k1_r[2:(k + 1)])
  
  mu0_i_hat_l = rk_i_l %*% gamma_k1_l
  mu0_i_hat_r = rk_i_r %*% gamma_k1_r
  mu2_i_hat_l = rk_i_l %*% gamma_k2_l
  mu2_i_hat_r = rk_i_r %*% gamma_k2_r
  
  mu0_hat_l = rk_l %*% gamma_k1_l
  mu0_hat_r = rk_r %*% gamma_k1_r
  mu2_hat_l = rk_l %*% gamma_k2_l
  mu2_hat_r = rk_r %*% gamma_k2_r
  
  mu1_hat_l = drk_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_hat_r = drk_r %*% (gamma_k1_r[2:(k + 1)])
  
  mu1_i_hat_l = drk_i_l %*% (gamma_k1_l[2:(k + 1)])
  mu1_i_hat_r = drk_i_r %*% (gamma_k1_r[2:(k + 1)])
  
  var_y_l = var(y_l)
  var_y_r = var(y_r)
  
  sigma2_hat_l_bar = mu2_i_hat_l - mu0_i_hat_l ^ 2
  sigma2_hat_r_bar = mu2_i_hat_r - mu0_i_hat_r ^ 2
  ind_s2_l = sigma2_hat_l_bar < 0
  ind_s2_r = sigma2_hat_r_bar < 0
  sigma2_hat_l_bar[ind_s2_l] = var_y_l
  sigma2_hat_r_bar[ind_s2_r] = var_y_r
  
  sigma2_hat_l = mu2_hat_l - mu0_hat_l ^ 2
  sigma2_hat_r = mu2_hat_r - mu0_hat_r ^ 2
  ind_s2_l = sigma2_hat_l < 0
  ind_s2_r = sigma2_hat_r < 0
  sigma2_hat_l[ind_s2_l] = var_y_l
  sigma2_hat_r[ind_s2_r] = var_y_r
  
  
  J.fun = function(B, V) {
    ceiling((((2 * B) / V) * n) ^ (1 / 3))
  }
  
  
  B_es_hat_dw = c(((c - x_min) ^ 2 / (12 * n)) * sum(mu1_hat_l ^ 2), ((x_max -
                                                                         c) ^ 2 / (12 * n)) * sum(mu1_hat_r ^ 2))
  V_es_hat_dw = c((0.5 / (c - x_min)) * sum(dxi_l * dyi_l ^ 2), (0.5 / (x_max -
                                                                          c)) * sum(dxi_r * dyi_r ^ 2))
  V_es_chk_dw = c((1 / (c - x_min)) * sum(dxi_l * sigma2_hat_l_bar),
                  (1 / (x_max - c)) * sum(dxi_r * sigma2_hat_r_bar))
  J_es_hat_dw = J.fun(B_es_hat_dw, V_es_hat_dw)
  J_es_chk_dw = J.fun(B_es_hat_dw, V_es_chk_dw)
  
  B_qs_hat_dw = c((n_l ^ 2 / (24 * n)) * sum(dxi_l ^ 2 * mu1_i_hat_l ^ 2),
                  (n_r ^ 2 / (24 * n)) * sum(dxi_r ^ 2 * mu1_i_hat_r ^ 2))
  V_qs_hat_dw = c((1 / (2 * n_l)) * sum(dyi_l ^ 2), (1 / (2 * n_r)) * sum(dyi_r ^
                                                                            2))
  V_qs_chk_dw = c((1 / n_l) * sum(sigma2_hat_l), (1 / n_r) * sum(sigma2_hat_r))
  J_qs_hat_dw = J.fun(B_qs_hat_dw, V_qs_hat_dw)
  J_qs_chk_dw = J.fun(B_qs_hat_dw, V_qs_chk_dw)
  
  J_es_hat_mv  = c(ceiling((var_y_l / V_es_hat_dw[1]) * (n / log(n) ^ 2)), ceiling((var_y_r /
                                                                                      V_es_hat_dw[2]) * (n / log(n) ^ 2)))
  J_es_chk_mv  = c(ceiling((var_y_l / V_es_chk_dw[1]) * (n / log(n) ^ 2)), ceiling((var_y_r /
                                                                                      V_es_chk_dw[2]) * (n / log(n) ^ 2)))
  J_qs_hat_mv  = c(ceiling((var_y_l / V_qs_hat_dw[1]) * (n / log(n) ^ 2)), ceiling((var_y_r /
                                                                                      V_qs_hat_dw[2]) * (n / log(n) ^ 2)))
  J_qs_chk_mv  = c(ceiling((var_y_l / V_qs_chk_dw[1]) * (n / log(n) ^ 2)), ceiling((var_y_r /
                                                                                      V_qs_chk_dw[2]) * (n / log(n) ^ 2)))
  
  ##
  if (binselect == "es") {
    J_star_orig = J_es_hat_dw
    meth = "es"
    binselect_type = "IMSE-optimal evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV   = J_es_hat_mv
  }
  if (binselect == "espr") {
    J_star_orig = J_es_chk_dw
    meth = "es"
    binselect_type = "IMSE-optimal evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV   = J_es_chk_mv
  }
  if (binselect == "esmv") {
    J_star_orig = J_es_hat_mv
    meth = "es"
    binselect_type = "mimicking variance evenly-spaced method using spacings estimators"
    J_IMSE = J_es_hat_dw
    J_MV   = J_es_hat_mv
  }
  if (binselect == "esmvpr") {
    J_star_orig = J_es_chk_mv
    meth = "es"
    binselect_type = "mimicking variance evenly-spaced method using polynomial regression"
    J_IMSE = J_es_chk_dw
    J_MV   = J_es_chk_mv
  }
  if (binselect == "qs") {
    J_star_orig = J_qs_hat_dw
    meth = "qs"
    binselect_type = "IMSE-optimal quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV   = J_qs_hat_mv
  }
  if (binselect == "qspr") {
    J_star_orig = J_qs_chk_dw
    meth = "qs"
    binselect_type = "IMSE-optimal quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV   = J_qs_chk_mv
  }
  if (binselect == "qsmv") {
    J_star_orig = J_qs_hat_mv
    meth = "qs"
    binselect_type = "mimicking variance quantile-spaced method using spacings estimators"
    J_IMSE = J_qs_hat_dw
    J_MV   = J_qs_hat_mv
  }
  if (binselect == "qsmvpr") {
    J_star_orig = J_qs_chk_mv
    meth = "qs"
    binselect_type = "mimicking variance quantile-spaced method using polynomial regression"
    J_IMSE = J_qs_chk_dw
    J_MV   = J_qs_chk_mv
  }
  
  J_star_l = scale_l * J_star_orig[1]
  J_star_r = scale_r * J_star_orig[2]
  
  if (!is.null(nbins)) {
    J_star_l = nbins_l
    J_star_r = nbins_r
    binselect_type = "manually evenly spaced"
  }
  
  if (var_y_l == 0) {
    J_star_l = J_star_l_orig = 1
    print("Warning: not enough variability in the outcome variable below the threshold")
  }
  
  if (var_y_r == 0) {
    J_star_r = J_star_r_orig = 1
    print("Warning: not enough variability in the outcome variable above the threshold")
  }
  
  rscale_l = J_star_l / J_IMSE[1]
  rscale_r = J_star_r / J_IMSE[2]
  
  bin_x_l = rep(0, length(x_l))
  bin_x_r = rep(0, length(x_r))
  jump_l  = range_l / J_star_l
  jump_r = range_r / J_star_r
  
  
  if (meth == "es") {
    jumps_l = seq(x_min, c, jump_l)
    jumps_r = seq(c, x_max, jump_r)
    #binselect_type="Evenly-Spaced"
  }   else if (meth == "qs") {
    jumps_l = quantile(x_l, probs = seq(0, 1, 1 / J_star_l))
    jumps_r = quantile(x_r, probs = seq(0, 1, 1 / J_star_r))
    # binselect_type="Quantile-Spaced"
  }
  
  for (k in 1:(J_star_l - 1))
    bin_x_l[x_l >= jumps_l[k] & x_l < jumps_l[k + 1]] = -J_star_l + k - 1
  bin_x_l[x_l >= jumps_l[(J_star_l)]] = -1
  for (k in 1:(J_star_r - 1))
    bin_x_r[x_r >= jumps_r[k] & x_r < jumps_r[k + 1]] = k
  bin_x_r[x_r >= jumps_r[(J_star_r)]] = J_star_r
  
  rdplot_mean_bin_l = rdplot_mean_x_l = rdplot_mean_y_l = rep(0, J_star_l)
  rdplot_mean_bin_r = rdplot_mean_x_r = rdplot_mean_y_r = rep(0, J_star_r)
  
  if (!is.null(covs) & covs_eval == "mean") {
    covs_model_l = lm(y_l ~ z_l + factor(bin_x_l))
    covs_model_r = lm(y_r ~ z_r + factor(bin_x_r))
    yhatZ_l = predict(covs_model_l)
    yhatZ_r = predict(covs_model_r)
    
  }
  
  
  for (k in 1:(J_star_l)) {
    rdplot_mean_bin_l[k]    = mean(c(jumps_l[k], jumps_l[k + 1]))
    rdplot_mean_x_l[k]      = mean(x_l[bin_x_l == -k])
    rdplot_mean_y_l[k]      = mean(y_l[bin_x_l == -k])
    if (!is.null(covs) &
        covs_eval == "mean")
      rdplot_mean_y_l[k] = mean(yhatZ_l[bin_x_l == -k])
  }
  
  rdplot_mean_y_l = rev(rdplot_mean_y_l)
  rdplot_mean_x_l = rev(rdplot_mean_x_l)
  
  for (k in 1:(J_star_r)) {
    rdplot_mean_bin_r[k]  = mean(c(jumps_r[k], jumps_r[k + 1]))
    rdplot_mean_x_r[k]    = mean(x_r[bin_x_r == k])
    rdplot_mean_y_r[k]    = mean(y_r[bin_x_r == k])
    if (!is.null(covs) &
        covs_eval == "mean")
      rdplot_mean_y_r[k] = mean(yhatZ_r[bin_x_r == k])
  }
  
  rdplot_mean_bin_l[J_star_l] = mean(c(jumps_l[J_star_l], c))
  rdplot_mean_bin_r[J_star_r] = mean(c(jumps_r[J_star_r], x_max))
  
  bin_x = c(bin_x_l, bin_x_r)
  rdplot_mean_bin = c(rdplot_mean_bin_l, rdplot_mean_bin_r)
  rdplot_mean_x   = c(rdplot_mean_x_l,   rdplot_mean_x_r)
  rdplot_mean_y   = c(rdplot_mean_y_l,   rdplot_mean_y_r)
  
  rdplot_sd_y_l = rdplot_N_l = rdplot_sd_y_r = rdplot_N_r = 0
  for (j in 1:(J_star_l)) {
    rdplot_sd_y_l[j] =     sd(y_l[bin_x_l == -j])
    rdplot_N_l[j]    = length(y_l[bin_x_l == -j])
  }
  
  for (j in 1:(J_star_r)) {
    rdplot_sd_y_r[j] =     sd(y_r[bin_x_r == j])
    rdplot_N_r[j]    = length(y_r[bin_x_r == j])
  }
  
  rdplot_sd_y_l[is.na(rdplot_sd_y_l)] = 0
  rdplot_sd_y_r[is.na(rdplot_sd_y_r)] = 0
  rdplot_sd_y = c(rev(rdplot_sd_y_l), rdplot_sd_y_r)
  rdplot_N = c(rev(rdplot_N_l), rdplot_N_r)
  quant = -qt((1 - (ci / 100)) / 2, pmax(rdplot_N - 1, 1))
  rdplot_se_y <- rdplot_sd_y / sqrt(rdplot_N)
  rdplot_cil_bin = rdplot_mean_y - quant * rdplot_se_y
  rdplot_cir_bin = rdplot_mean_y + quant * rdplot_se_y
  temp_plot = NULL
  
  if (hide == "FALSE") {
    if (is.null(col.lines))
      col.lines = "red"
    if (is.null(col.dots))
      col.dots  = "darkblue"
    #if (is.null(type.dots)) type.dots = 20
    if (is.null(title))
      title = "RD Plot"
    if (is.null(x.label))
      x.label = "X axis"
    if (is.null(y.label))
      y.label = "Y axis"
    #if (is.null(x.lim)) x.lim=c(min(x_l),max(x_r))
    #if (is.null(y.lim)) y.lim=c(min(c(y_l,y_r)),max(c(y_l,y_r)))
    #if (is.null(y.lim)) y.lim=c(min(rdplot_mean_y),max(rdplot_mean_y))
    
    data_bins <-
      data.frame(rdplot_mean_bin,
                 rdplot_mean_y,
                 rdplot_cil_bin,
                 rdplot_cir_bin)
    data_poly <- data.frame(x_plot_l, y_hat_l, x_plot_r, y_hat_r)
    
    temp_plot <- ggplot() + theme_bw() +
      geom_point(
        data = data_bins,
        aes(x = rdplot_mean_bin, y = rdplot_mean_y),
        col = col.dots,
        na.rm = TRUE
      ) +
      geom_line(
        data = data_poly,
        aes(x = x_plot_l, y = y_hat_l),
        col = col.lines,
        na.rm = TRUE
      ) +
      geom_line(
        data = data_poly,
        aes(x = x_plot_r, y = y_hat_r),
        col = col.lines,
        na.rm = TRUE
      )
    
    if (flag_no_ci == FALSE)
      temp_plot <- temp_plot +
      geom_errorbar(
        data = data_bins,
        aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, ymax = rdplot_cir_bin),
        linetype = 1
      )
    if (shade == TRUE) {
      temp_plot <- temp_plot +
        geom_ribbon(data = data_bins,
                    aes(
                      x = rdplot_mean_bin,
                      ymin = rdplot_cil_bin,
                      ymax = rdplot_cir_bin
                    ))
    }
    
    temp_plot <-
      temp_plot + labs(x = x.label, y = y.label) + ggtitle(title) +
      coord_cartesian(xlim = x.lim, ylim = y.lim) +
      theme(legend.position = "None") +
      geom_vline(xintercept = c, size = 0.5)
    print(temp_plot)
  }
  
  cutoffs = c(jumps_l, jumps_r[2:length(jumps_r)])
  rdplot_min_bin = cutoffs[1:(length(cutoffs) - 1)]
  rdplot_max_bin = cutoffs[2:length(cutoffs)]
  
  bin_length = rdplot_max_bin - rdplot_min_bin
  bin_avg_l =   mean(bin_length[1:J_star_l])
  bin_med_l = median(bin_length[1:J_star_l])
  
  bin_avg_r =   mean(bin_length[(J_star_l + 1):length(bin_length)])
  bin_med_r = median(bin_length[(J_star_l + 1):length(bin_length)])
  
  vars_bins = data.frame(
    "rdplot_mean_bin" = rdplot_mean_bin,
    "rdplot_mean_x" = rdplot_mean_x,
    "rdplot_mean_y" = rdplot_mean_y,
    "rdplot_min_bin" = rdplot_min_bin,
    "rdplot_max_bin" = rdplot_max_bin,
    "rdplot_se_y" = rdplot_se_y,
    "rdplot_N" = rdplot_N,
    "rdplot_ci_l" = rdplot_cil_bin,
    "rdplot_ci_r" = rdplot_cir_bin
  )
  vars_poly = data.frame(
    "rdplot_x" = c(x_plot_l, x_plot_r),
    "rdplot_y" = c(y_hat_l, y_hat_r)
  )
  
  coef = cbind(gamma_p1_l, gamma_p1_r)
  colnames(coef) = c("Left", "Right")
  out = list(
    coef = coef,
    rdplot = temp_plot,
    vars_bins = vars_bins,
    vars_poly = vars_poly,
    J = c(J_star_l, J_star_r),
    J_IMSE = J_IMSE,
    J_MV = J_MV,
    scale = c(scale_l, scale_r),
    rscale = c(rscale_l, rscale_r),
    bin_avg = c(bin_avg_l, bin_avg_r),
    bin_med = c(bin_med_l, bin_med_r),
    p = p,
    c = c,
    h = c(h_l, h_r),
    N = c(n_l, n_r),
    N_h = c(n_h_l, n_h_r),
    binselect = binselect_type,
    kernel = kernel_type
  )
  
  out$call <- match.call()
  class(out) <- "rdplot"
  return(invisible(out))
}

print.rdplot <- function(x, ...) {
  cat("Call: rdplot\n\n")
  
  cat(paste(
    "Number of Obs.           ",
    format(x$N[1] + x$N[2], width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Kernel                   ",
    format(x$kernel,      width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat("\n")
  cat(paste(
    "Number of Obs.           ",
    format(x$N[1],    width = 10, justify = "right"),
    "      ",
    format(x$N[2],     width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Eff. Number of Obs.      ",
    format(x$N_h[1],  width = 10, justify = "right"),
    "      ",
    format(x$N_h[2],   width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Order poly. fit (p)      ",
    format(x$p,       width = 10, justify = "right"),
    "      ",
    format(x$p,        width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "BW poly. fit (h)         ",
    format(sprintf("%10.3f", x$h[1])),
    "      ",
    format(sprintf("%10.3f", x$h[2])),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Number of bins scale     ",
    format(sprintf("%10.3f", x$scale[1])),
    "      ",
    format(sprintf("%10.3f", x$scale[2])),
    "\n",
    sep = ""
  ))
  cat("\n")
}

summary.rdplot <- function(object, ...) {
  x    <- object
  args <- list(...)
  
  cat("Call: rdplot\n\n")
  
  cat(paste(
    "Number of Obs.           ",
    format(x$N[1] + x$N[2], width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Kernel                   ",
    format(x$kernel,      width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat("\n")
  cat(paste(
    "Number of Obs.           ",
    format(x$N[1],     width = 10, justify = "right"),
    "      ",
    format(x$N[2],     width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Eff. Number of Obs.      ",
    format(x$N_h[1],   width = 10, justify = "right"),
    "      ",
    format(x$N_h[2],   width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Order poly. fit (p)      ",
    format(x$p,        width = 10, justify = "right"),
    "      ",
    format(x$p,        width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "BW poly. fit (h)         ",
    format(sprintf("%10.3f", x$h[1])),
    "      ",
    format(sprintf("%10.3f", x$h[2])),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Number of bins scale     ",
    format(sprintf("%10.0f", x$scale[1])),
    "      ",
    format(sprintf("%10.0f", x$scale[2])),
    "\n",
    sep = ""
  ))
  cat("\n")
  cat(paste(
    "Bins Selected            ",
    format(x$J[1],       width = 10, justify = "right"),
    "      ",
    format(x$J[2], width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Average Bin Length       ",
    format(sprintf("%10.3f", x$bin_avg[1])),
    "      ",
    format(sprintf("%10.3f", x$bin_avg[2])),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Median Bin Length        ",
    format(sprintf("%10.3f", x$bin_med[1])),
    "      ",
    format(sprintf("%10.3f", x$bin_med[2])),
    "\n",
    sep = ""
  ))
  cat("\n")
  cat(paste(
    "IMSE-optimal bins        ",
    format(x$J_IMSE[1], width = 10, justify = "right"),
    "      ",
    format(x$J_IMSE[2], width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat(paste(
    "Mimicking Variance bins  ",
    format(x$J_MV[1],   width = 10, justify = "right"),
    "      ",
    format(x$J_MV[2],   width = 10, justify = "right"),
    "\n",
    sep = ""
  ))
  cat("\n")
  cat(paste("Relative to IMSE-optimal:",   "\n", sep = ""))
  cat(paste(
    "Implied scale            ",
    format(sprintf("%10.3f", x$rscale[1])),
    "      ",
    format(sprintf("%10.3f", x$rscale[2])),
    "\n",
    sep = ""
  ))
  cat(paste(
    "WIMSE variance weight    ",
    format(sprintf("%10.3f", 1 / (1 + x$rscale[1] ^ 3))),
    "      ",
    format(sprintf("%10.3f", 1 / (1 + x$rscale[2] ^ 3))),
    "\n",
    sep = ""
  ))
  cat(paste(
    "WIMSE bias weight        ",
    format(sprintf("%10.3f", x$rscale[1] ^ 3 / (1 + x$rscale[1] ^ 3))),
    "      ",
    format(sprintf("%10.3f", x$rscale[2] ^ 3 / (1 + x$rscale[2] ^ 3))),
    "\n",
    sep = ""
  ))
  cat("\n")
}

calculate_spec <-
  function(
    epoch_currency_simulated,
    profile,
    basecutoff,
    threshold,
    spec_representative
  ) {
    
    datetime_cutoff <-
      epoch_currency_simulated %>%
      dplyr::filter(currency == base) %>%
      dplyr::filter(generated) %>%
      dplyr::filter(blockheight == cutoff) %>%
      dplyr::select(setting, datetime) %>%
      dplyr::rename(datetime_cutoff = datetime)
    
    epoch_currency <-
      epoch_currency_simulated %>%
      dplyr::filter(simulated) %>%
      dplyr::left_join(
        datetime_cutoff,
        by = "setting"
      ) %>%
      dplyr::mutate(datetime_relative = difftime(datetime, datetime_cutoff, unit = "days")) %>%
      dplyr::filter(datetime_relative < 60, datetime_relative >= 0)
    
    
    security <-
      epoch_currency %>%
      dplyr::group_by(setting, currency) %>%
      dplyr::mutate(
        operation_electricity = sum(epoch_time * hash_rate) / sum(epoch_time),
        operation_electricity = operation_electricity  
      ) %>%
      dplyr::arrange(hash_rate) %>%
      dplyr::mutate(time_cumulative = cumsum(epoch_time)) %>%
      dplyr::mutate(time_cumulative = time_cumulative / max(time_cumulative)) %>%
      dplyr::filter(time_cumulative >= threshold) %>%
      dplyr::filter(time_cumulative == min(time_cumulative)) %>%
      dplyr::mutate(
        attack_electricity = hash_rate 
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(setting, currency, operation_electricity, attack_electricity) %>%
      dplyr::mutate(SpEC = attack_electricity / operation_electricity)
    
    security <-
      security %>%
      dplyr::left_join(
        profile,
        by = "setting"
      ) %>%
      dplyr::select(setting, BTC, BCH, BSV, currency, dplyr::everything())
    
    security <-
      security %>%
      dplyr::group_by(currency) %>%
      dplyr::mutate(SpEC_actual = sum(SpEC * (setting == "actual"))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(operation_electricity_saving = operation_electricity * (1 - SpEC_actual / SpEC)) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("electricity"),
          .fns = ~ . * spec_representative$efficiency
        )
      )
    
    return(security)
  }

patch_simulation <- 
  function(
    iter,
    currency_list,
    scenario_list
  ) {
    foreach (
      i = 1:length(currency_list),
      .packages = c(
        "BlockChain",
        "dplyr",
        "magrittr",
        "doParallel",
        "foreach")
    ) %dopar% {
      currency_i <- currency_list[i]
      foreach (
        j = 1:length(scenario_list),
        .packages = c(
          "BlockChain",
          "dplyr",
          "magrittr",
          "doParallel",
          "foreach")
      ) %dopar% {
        scenario_j <- scenario_list[j]
        
        if (currency_i == "BTC"){
          
          savename_list <-
            list.files(
              path = "output/10_1_simulate_reduced_btc_halving",
              pattern = paste0("epoch_currency_btc_halving_", scenario_j),
              full.names = TRUE
            )
          
          epoch_currency_simulated <-
            savename_list %>%
            purrr::map(readRDS) %>%
            magrittr::set_names(
              savename_list %>%
                gsub(paste0("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_", scenario_j, "_"), "", .) %>%
                gsub(".rds", "", .)
            ) %>%
            dplyr::bind_rows(.id = "trial")
          
          epoch_currency_simulated <-
            epoch_currency_simulated %>% 
            dplyr::group_by(trial) %>% 
            dplyr::mutate(setting = scenario_j)
          
          foreach (
            i = 1:iter,
            .packages = c(
              "BlockChain",
              "dplyr",
              "magrittr",
              "doParallel",
              "foreach")
          ) %dopar% {
            epoch_currency_simulated_subset <- 
              subset(
                epoch_currency_simulated, 
                subset = trial == i)
            
            saveRDS(
              epoch_currency_simulated_subset,
              file = paste0("output/10_1_simulate_reduced_btc_halving/epoch_currency_btc_halving_", scenario_j, "_", i, ".rds")
            ) 
            
          }
          
        } else if (currency_i == "BCH") {
          
          savename_list <-
            list.files(
              path = "output/10_2_simulate_reduced_bch_halving",
              pattern = paste0("epoch_currency_bch_halving_", scenario_j),
              full.names = TRUE
            )
          
          epoch_currency_simulated <-
            savename_list %>%
            purrr::map(readRDS) %>%
            magrittr::set_names(
              savename_list %>%
                gsub(paste0("output/10_2_simulate_reduced_bch_halving/epoch_currency_bch_halving_", scenario_j, "_"), "", .) %>%
                gsub(".rds", "", .)
            ) %>%
            dplyr::bind_rows(.id = "trial")
          
          epoch_currency_simulated <-
            epoch_currency_simulated %>% 
            dplyr::group_by(trial) %>% 
            dplyr::mutate(setting = scenario_j)
          
          foreach (
            i = 1:iter,
            .packages = c(
              "BlockChain",
              "dplyr",
              "magrittr",
              "doParallel",
              "foreach")
          ) %dopar% {
            epoch_currency_simulated_subset <- 
              subset(
                epoch_currency_simulated, 
                subset = trial == i)
            
            saveRDS(
              epoch_currency_simulated_subset,
              file = paste0("output/10_2_simulate_reduced_bch_halving/epoch_currency_bch_halving_", scenario_j, "_", i, ".rds")
            )
          }
          
        } else if (currency_i == "BSV") {
          savename_list <-
            list.files(
              path = "output/10_3_simulate_reduced_bsv_halving",
              pattern = paste0("epoch_currency_bsv_halving_", scenario_j),
              full.names = TRUE
            )
          
          epoch_currency_simulated <-
            savename_list %>%
            purrr::map(readRDS) %>%
            magrittr::set_names(
              savename_list %>%
                gsub(paste0("output/10_3_simulate_reduced_bsv_halving/epoch_currency_bsv_halving_", scenario_j, "_"), "", .) %>%
                gsub(".rds", "", .)
            ) %>%
            dplyr::bind_rows(.id = "trial")
          
          epoch_currency_simulated <-
            epoch_currency_simulated %>% 
            dplyr::group_by(trial) %>% 
            dplyr::mutate(setting = scenario_j)
          
          foreach (
            i = 1:iter,
            .packages = c(
              "BlockChain",
              "dplyr",
              "magrittr",
              "doParallel",
              "foreach")
          ) %dopar% {
            epoch_currency_simulated_subset <- 
              subset(
                epoch_currency_simulated, 
                subset = trial == i)
            
            saveRDS(
              epoch_currency_simulated_subset,
              file = paste0("output/10_3_simulate_reduced_bsv_halving/epoch_currency_bsv_halving_", scenario_j, "_", i, ".rds")
            ) 
          }
          
        } else {
          stop("Setting is invalid")
        }
        
      }
    }
  }
