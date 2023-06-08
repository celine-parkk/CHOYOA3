library(dplyr)
library(tibble)
library(ggplot2)
library(ggforce)
library(flametree)

tree <- flametree_grow(
  seed = 6, #adjusting the seed to change the shape of the tree
  time = 12, #adjusting the time to make the tree bigger
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
  colour = "pink" #adjusting the color of the leaves
)

trunk <- geom_bezier(
  mapping = aes(coord_x, coord_y, group = id_pathtree, size = seg_wid),
  data = tree, 
  lineend = "round", 
  colour = "brown", #changing the color of the trunk to brown
  show.legend = FALSE
)

plot(base + trunk + leaves)
