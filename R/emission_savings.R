emission_savings <- function(time, baseline_co2, baseline_co2sd, co2, co2sd, ch4, ch4sd, type = c("fe", "eq"), runs = 1000) {

  #time vector
  time <- seq(0, time, by = 1)

  #initialize matrices to store output
  net_out <- matrix(NA, length(time), runs)
  ch4_out <- matrix(NA, length(time), runs)
  co2_out <- matrix(NA, length(time), runs)
  co2base_out <- matrix(NA, length(time), runs)
  savings_out <- matrix(NA, length(time), runs)

  #conversion factors
  co2m <- 44/12
  ch4m <- 16/12

  #simulate fluxes a number of times and store output
  for (i in 1:runs) {

    #simulate co2 and ch4 flux times series
    co2_simulated <- c(0, rnorm(length(time)-1, mean = co2*co2m, sd = co2sd*co2m))
    ch4_simulated <- c(0, rnorm(length(time)-1, mean = ch4*ch4m, sd = ch4sd*ch4m))
    co2base_simulated <- c(rnorm(length(time), mean = baseline_co2*ch4m, sd = baseline_co2sd*ch4m))

    if (type == "fe") {
      #ch4 flux difference for GWP* calculation
      ch4_diff <- c(0, diff(ch4_simulated))

      #cumulative emissions for component fluxes and overall co2e
      co2_cum <- cumsum(co2_simulated)
      co2base_cum <- cumsum(co2base_simulated)
      ch4_cum <- cumsum(ch4_diff*100*28)
      net_cum <- co2_cum + ch4_cum
      net_savings <- net_cum - co2base_cum

    } else if (type == "eq") {
      #proceed using standard GWP
      #cumulative emissions for component fluxes and overall co2e
      co2_cum <- cumsum(co2_simulated)
      co2base_cum <- cumsum(co2base_simulated)
      ch4_cum <- cumsum(ch4_simulated*28)
      net_cum <- co2_cum + ch4_cum
      net_savings <- net_cum - co2base_cum

    } else {
      stop("Warning: type not assigned correctly")
    }

    #save runs to matrix
    net_out[, i] <- net_cum
    ch4_out[, i] <- ch4_cum
    co2_out[, i] <- co2_cum
    co2base_out[, i] <- co2base_cum
    savings_out[, i] <- net_savings

  }

  #store final means and sd in single dataset
  net_mean <- apply(net_out, 1, mean)
  net_sd <- apply(net_out, 1, sd)
  output_df <- data.frame(net_mean, net_sd)
  output_df$time <- time
  output_df$co2_cum <- apply(co2_out, 1, mean)
  output_df$co2_sd <- apply(co2_out, 1, sd)
  output_df$ch4_cum <- apply(ch4_out, 1, mean)
  output_df$ch4_sd <- apply(ch4_out, 1, sd)
  output_df$co2base_cum <- apply(co2base_out, 1, mean)
  output_df$co2base_sd <- apply(co2base_out, 1, sd)
  output_df$savings_cum <- apply(savings_out, 1, mean)
  output_df$savings_sd <- apply(savings_out, 1, sd)

  output_df

}
