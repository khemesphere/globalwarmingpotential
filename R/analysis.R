# group of runs for co2e model
library(tidyverse)

#land use scenarios
pasture <- co2e_model(time1 = -150, time2 = 400, pre_co2 = 305.7, pre_co2sd = 78.7, pre_ch4 = 9.1, pre_ch4sd = 4.0,
                    pst_co2 = 449.4, pst_co2sd = 206.0, pst_ch4 = 44.9, pst_ch4sd = 8.6, runs = 1000)


corn <- co2e_model(time1 = -150, time2 = 400, pre_co2 = 826.2, pre_co2sd = 163, pre_ch4 = 2.2, pre_ch4sd = 1.4,
                      pst_co2 = 449.4, pst_co2sd = 206.0, pst_ch4 = 44.9, pst_ch4sd = 8.6, runs = 1000)

wetland <- co2e_model(time1 = 0, time2 = 150, pre_co2 = 305.7, pre_co2sd = 78.7, pre_ch4 = 9.1, pre_ch4sd = 4.0,
                      pst_co2 = 449.4, pst_co2sd = 206.0, pst_ch4 = 44.9, pst_ch4sd = 8.6, runs = 1000)

a <- ggplot(wetland) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = time, ymin = (ch4_cum - ch4_sd)/1000, ymax = (ch4_cum + ch4_sd)/1000), fill = "salmon 1", alpha = 0.5) +
  geom_line(aes(x = time, y = ch4_cum/1000), color = "red") +
  geom_ribbon(aes(x = time, ymin = (co2_cum - co2_sd)/1000, ymax = (co2_cum + co2_sd)/1000), fill = "lightgreen", alpha = 0.5) +
  geom_line(aes(x = time, y = co2_cum/1000), color = "darkgreen") +
  geom_ribbon(aes(x = time, ymin = (net_mean - net_sd)/1000, ymax = (net_mean + net_sd)/1000), fill = "light grey" , alpha = 0.5) +
  geom_line(aes(x = time, y = net_mean/1000)) +
  scale_y_continuous(limits = c(-250, 250)) +
  ylab("Cumulative CO2eq. emissions (kg m-2)") + xlab("Time since restoration (years)") + ggtitle("Wetland") +
  theme_bw()


# plot em
b <- ggplot(pasture) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = time, ymin = (ch4_cum - ch4_sd)/1000, ymax = (ch4_cum + ch4_sd)/1000), fill = "salmon 1", alpha = 0.5) +
  geom_line(aes(x = time, y = ch4_cum/1000), color = "red") +
  geom_ribbon(aes(x = time, ymin = (co2_cum - co2_sd)/1000, ymax = (co2_cum + co2_sd)/1000), fill = "lightgreen", alpha = 0.5) +
  geom_line(aes(x = time, y = co2_cum/1000), color = "darkgreen") +
  geom_ribbon(aes(x = time, ymin = (net_mean - net_sd)/1000, ymax = (net_mean + net_sd)/1000), fill = "light grey" , alpha = 0.5) +
  geom_line(aes(x = time, y = net_mean/1000)) +
  scale_y_continuous(limits = c(-250, 700)) +
  ylab("Cumulative CO2eq. emissions (kg m-2)") + xlab("Time since restoration (years)") + ggtitle("Pasture") +
  theme_bw()

c <- ggplot(corn) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(x = time, ymin = (ch4_cum - ch4_sd)/1000, ymax = (ch4_cum + ch4_sd)/1000), fill = "salmon 1", alpha = 0.5) +
  geom_line(aes(x = time, y = ch4_cum/1000), color = "red") +
  geom_ribbon(aes(x = time, ymin = (co2_cum - co2_sd)/1000, ymax = (co2_cum + co2_sd)/1000), fill = "lightgreen", alpha = 0.5) +
  geom_line(aes(x = time, y = co2_cum/1000), color = "darkgreen") +
  geom_ribbon(aes(x = time, ymin = (net_mean - net_sd)/1000, ymax = (net_mean + net_sd)/1000), fill = "light grey" , alpha = 0.5) +
  geom_line(aes(x = time, y = net_mean/1000)) +
  scale_y_continuous(limits = c(-250, 700)) +
  ylab("Cumulative CO2eq. emissions (kg m-2)") + xlab("Time since restoration (years)") + ggtitle("Corn") +
  theme_bw()

grid.arrange(a, b, c, nrow = 1)
