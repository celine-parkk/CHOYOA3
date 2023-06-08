library(ggplot2)
library(tibble)
library(dplyr)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] #adjusting the colors to create colors from canva
}

sample_data <- function(seed = NULL, n = 100){ #creating sample data
  if(!is.null(seed)) set.seed(seed)
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2), #adjusting the randomness
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n),
    shape = factor(sample(0:22, size = n, replace = TRUE))
  )
}

polar_styled_plot <- function(data = NULL, palette) {
  ggplot(
    data = data, #creating ggplots from the data
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) + 
    coord_polar(clip = "off") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + #getting colors from the palette
    scale_size(range = c(0, 10)) + 
    theme_void() + 
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}

#1.
dat <- sample_data(n = 100, seed = 1)  #calling data with sample_canva
pal <- sample_canva(seed = 1)

#2. 
polar_styled_plot(data = dat, palette = pal) + geom_segment()
polar_styled_plot(data = dat, palette = pal) + geom_path() #initializing plot with polar_styled_plot
polar_styled_plot(data = dat, palette = pal) + geom_point()

#3. 
library(dplyr) #adding ggplot geoms

dat1 <- sample_data(n = 2000, seed = 123) 
dat2 <- sample_data(n = 100, seed = 456) |>  
  mutate(y0 = .3 + y0 * .6, y1 = .3)

polar_styled_plot(palette = sample_canva(seed = 7)) + 
  geom_segment(
    data = dat1 |> mutate(size = size * 3)
  ) + 
  geom_line(
    data = dat2 |> mutate(size = size / 5), #adding ggplot2 geoms- creating lines
    lineend = "round", 
    colour = "white"
  ) +
  geom_segment(
    data = dat2 |> mutate(size = size / 40), #adding ggplot 2 geoms- creating segments
    lineend = "square", #creating squares for the end of the lines
    colour = "#222222"
  ) +
  geom_point(
    data = dat2 |> mutate(size = size * 2), #adding ggplot 2 geoms- creating points
    colour = "#222222"
  )


#2. 
my_style_plot <- function(data = NULL, palette) {
  ggplot(
    data = data,
    mapping = aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = gradient, #creating a gradient color
      size = size
    )) + 
    coord_polar(clip = "o") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) +
    scale_x_continuous(
      expand = c(0, 0), 
      limits = c(0, 1), 
      oob = scales::oob_keep
    ) + 
    scale_colour_gradientn(colours = palette) + 
    scale_size(range = c(0, 10)) + 
    theme_void() + 
    guides(
      colour = guide_none(),
      size = guide_none(),
      fill = guide_none(),
      shape = guide_none()
    )
}

dat <- sample_data(n = 50, seed = 4) #creating 50 elements, adjusting the randomness
pal <- sample_canva(seed = 6) #adjusting the randomness of the colors


polar_styled_plot(data = dat, palette = pal) + geom_jitter() + geom_line() #creating jitter points and line points to create a figure 


