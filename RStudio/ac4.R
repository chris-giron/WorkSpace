require(ggplot2);library(plyr);library(robustHD);library(MASS);library(reshape2)
data("quakes")

ggplot(quakes, aes(x = long, y = lat, color = depth)) +
  geom_point(size = 3) +
  xlim(165,190) + ylim(-40,-10) +
  scale_colour_gradient(low = 'red', high = 'blue')

mean(quakes$lat > -27 & quakes$lat < -17 & quakes$long > 178 & quakes$long < 183)

deep_ind = (quakes$depth >= 500)
deep = quakes[deep_ind,]

mean(deep$lat > -27 & deep$lat < -17 & deep$long > 178 & deep$long < 183)

# ggplot(deep, aes(x = long, y = lat, color = depth)) +
#   geom_point(size = 3) +
#   xlim(165,190) + ylim(-40,-10) +
#   scale_colour_gradient(low = 'red', high = 'blue')
