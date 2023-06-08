library(dplyr)
library(tibble)
library(ggplot2)
library(ggfx)

set.seed(5) #adjusting the randomness of the plots
polar <- tibble(
  arc_start = runif(200),
  arc_end = arc_start + runif(200, min = -.2, max = .2),
  radius = runif(200),
  shade = runif(200), 
  size = runif(200)
)

base <- ggplot(
  data = polar, 
  mapping = aes(
    x = arc_start, 
    y = radius,
    xend = arc_end, 
    yend = radius, 
    colour = shade, 
    size = size
  )
) + 
  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep) +
  scale_x_continuous(limits = c(0, 1), oob = scales::oob_keep) + 
  scale_colour_viridis_c(option = "pink") + #adjusting the scale color
  guides(colour = guide_none(), size = guide_none()) + 
  scale_size(range = c(0, 10)) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "blue")) #adjusting the background

plot(base + with_dither(geom_segment())) #using Floyd-Steinberg dithering
