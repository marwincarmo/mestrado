library(dplyr)
library(ggplot2)
library(descr)

dat <- haven::read_dta("classes/MPR5740/MOTOCOBR.DTA")

## 2

CrossTable(dat$tmc, dat$fun, chisq = TRUE,fisher = TRUE)


x <- rnorm(1000)
y <- -(x^2)
sin(x)
plot(x, y)
cor(x,y)

tibble::tibble(x = x, y = y) |> 
  ggplot(aes(x = x, y = y)) + 
  geom_point(alpha = .5, color = "darkblue", size = 2.8) +
  theme_bw(12)

dat <- nycflights13::flights |> 
  filter(air_time < 400) |> 
  sample_n(1000)

dat |> 
  ggplot(aes(x = air_time, y = distance)) +
  geom_point(alpha = .5, color = "darkblue") +
  theme_bw(14) +
  xlab("Tempo de vôo") +
  ylab("Distância")
