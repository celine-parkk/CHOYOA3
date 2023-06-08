library(dplyr)
library(tibble)
library(ggplot2)
library(ggforce)
library(flametree)
library(ggfx)

tree <- flametree_grow(
  seed = 10, #adjusting the shape of the tree
  time = 12, #adjusting the size of the tree
  angle = c(-15, 15, 30)
)

leaf <- tree |> filter(id_leaf == TRUE)

base <- ggplot() + 
  scale_size_identity() + 
  theme_void() + 
  coord_equal()

leaves <- geom_point(
  mapping = aes(coord_x, coord_y),
  data = leaf, 
  size = 1.3, 
  stroke = 0, 
  colour = "pink" #adjusting the leaves color
)

trunk <- geom_bezier(
  mapping = aes(coord_x, coord_y, group = id_pathtree, size = seg_wid),
  data = tree, 
  lineend = "round", 
  colour = "brown", #adjusting the color of the base
  show.legend = FALSE
)

plot(
  base +   
    trunk + 
    with_inner_glow(leaves, colour = "#555555", sigma = 5, expand = 5) #creating inner glow for the tree
) #expanding each point by 5 pixels

