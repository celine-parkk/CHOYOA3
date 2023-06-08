library(ggthemes)
library(scales)
library(tidyverse)


# the original function from the first session
sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

# the extended function used in later sessions
sample_canva2 <- function(seed = NULL, n = 4) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]] |>
    (\(x) colorRampPalette(x)(n))()  
}

show_col(sample_canva())


#2. 

sample_named_colours <- function(n) {
  all <- colors(distinct = TRUE)  #randomizing all the colors
  sample_colours <- sample(all, n, replace = TRUE)  #replacing the n colors
  return(sample_colours) #returning sample colors to randomize the colors
}
show_col(sample_named_colours())

polar_art <- function(seed, n) {
  
  # set the state of the random number generator
  set.seed(seed)
  
  # data frame containing random values for 
  # aesthetics we might want to use in the art
  dat <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n), 
    size = runif(n)
  )
  
  # plot segments in various colours, using 
  # polar coordinates and a gradient palette
  dat |> 
    ggplot(aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size
    )) +
    geom_line(show.legend = FALSE) + #editing it so that the plot creates lines instead of circles
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_colour_gradientn(colours = sample_named_colours()) + #changing to sample_named_coloours instead of palette
    scale_size(range = c(0, 20)) + #changing the scale size to make the elements bigger
    theme_void()
}

polar_art(
  seed = 2, 
  n = 50
)

#3. 

sample_canva <- function(n) {
  all_colours <- unlist(ggthemes::canva_palettes)  #calling vector of 600 colors
  sampled_colours <- sample(all_colours, n) #sampling n colors without replacement
  return(sampled_colours)
}
show_col(sample_canva())

