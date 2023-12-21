library(tidyverse)
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = 'pink', fill = 'pink', shape = 24)
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")
ggplot(mpg, aes(x = displ, y = hwy, color = displ < 5)) +
  geom_poin()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth(aes(color = drv), show.legend = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = 0)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(group = drv), se = 0)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(group = drv, color = drv), se = 0)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(se = 0)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv)) +
  geom_smooth(aes(linetype = drv), se = 0)

gplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(fill = drv), color = 'white',
             pch = 21, size = 5)