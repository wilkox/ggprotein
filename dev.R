load_all()
library(ggplot2)
ggplot2::ggplot(data.frame(start = c(0, 100, 300), end = c(50, 200, 350)),
                aes(xmin = start, xmax = end, y = 1)) +
  geom_helix(inherit.aes = TRUE)
