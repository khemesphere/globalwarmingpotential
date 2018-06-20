co2e_model <- function(time1, time2,
                       pre_co2, pre_co2sd, pre_ch4, pre_ch4sd,
                       pst_co2, pst_co2sd, pst_ch4, pst_ch4sd,
                       runs = 1000) {

  #time vector
  time <- seq(time1, time2, by = 1)

  #initialize matrices to store output
  net_out <- matrix(NA, length(time), runs)
  ch4_out <- matrix(NA, length(time), runs)
  co2_out <- matrix(NA, length(time), runs)

  #conversion factors
  co2m <- 44/12
  ch4m <- 16/12

  #simulate fluxes a number of times and store output
  for (i in 1:runs) {

    #simulate co2 and ch4 flux times series
    co2 <- c(rnorm(abs(time1), mean = pre_co2*co2m, sd = pre_co2sd*co2m),
             (262.7+16.3)*co2m, (-562.4+27.7)*co2m, (-553.6+34.6)*co2m, (-442.1+49.3)*co2m,
             rnorm((time2-3), mean = -pst_co2*co2m, sd = pst_co2*co2m))


    ch4 <- c(rnorm(abs(time1), mean = pre_ch4*ch4m, sd = pre_ch4sd*ch4m),
                  0*ch4m, 27.7*ch4m, 34.6*ch4m, 49.3*ch4m,
                  rnorm((time2-3), mean = pst_ch4*ch4m, sd = pst_ch4sd*ch4m))

    #ch4 flux difference for GWP* calculation
    ch4_diff <- c(0, diff(ch4))

    #cumulative emissions for component fluxes and overall co2e
    co2_cum <- cumsum(co2)
    ch4_cum <- cumsum(ch4_diff*100*28)
    net_cum <- co2_cum + ch4_cum

    #save runs to matrix
    net_out[, i] <- net_cum
    ch4_out[, i] <- ch4_cum
    co2_out[, i] <- co2_cum

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

  output_df

}
