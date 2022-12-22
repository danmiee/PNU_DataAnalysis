install.packages("ggvis")
library(dplyr)
library(tidyr)
library(ggvis)
mtcars
attach(mtcars)
plot(mpg, wt)


mtcars %>% 
  ggvis(~mpg, ~wt, fill=~cyl) %>% 
  layer_points() %>% layer_smooths() %>%
  add_axis("x", title ="MPG", values=c(10:35)) %>%
  add_axis("y", title = "WT", subdivide = 4)
