# #seen from
# #https://speakerdeck.com/lionelhenry/selecting-and-doing-with-tidy-eval?slide=31
#
# summary_functions <-  list(
#   ~ mean(., na.rm = TRUE),
#   ~ sd(., na.rm = TRUE)
# )
#
#
# # perhaps one for MEDIAN, IQR
#
# summary_functions <-  list(
#   ~ quantile(.,1/4, na.rm = TRUE),
#   ~ median(., na.rm = TRUE),
#   ~ IQR(., na.rm = TRUE)
# )
#
# library(dplyr)
#
# dt1 %>%
#   summarise_at(
#     vars(height:mass),
#     summary_functions
#   )
#
# quantile(dt1$height, na.rm = T)
#
# dt1 =starwars
#
# IQR
#
# IQR(mtcars$mpg)
#
# # SOS. we have the cplumns 25% . needto remove??
# quantile(mtcars$mpg, 1/4, na.rm = TRUE)
#
# quantile(x <- rnorm(1001)) # Extremes & Quartiles by default
# quantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)
