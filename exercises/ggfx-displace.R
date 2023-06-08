
library(ggplot2)
library(ggfx)

polygon_layer <- function(x, y, fill = "white", alpha = .3) { #adjusting the transparency
  geom_polygon(aes(x, y), fill = fill, alpha = alpha)
}
poly1 <- polygon_layer(x = c(3, 0, 0), y = c(0, 0, 1)) #creating more layers of the polygon
poly2 <- polygon_layer(x = c(0, 1, 1), y = c(0, 0, 1)) 
poly3 <- polygon_layer(x = c(.3, 2, 1), y = c(0, 0, .7))
poly4 <- polygon_layer(x = c(0, 0, .7), y = c(.3, 1, 1))

base <- ggplot() + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_void() + 
  theme(panel.background = element_rect(fill = "#333333"))

text <- geom_text(
  mapping = aes(0.5, 0.5, label = "CELINE"), #making my name be displaced
  size = 60, 
  colour = "gray", #adjusting the color of the text
  fontface = "bold"
)

plot(
  base + 
    as_group(poly1, poly2, poly3, poly4, id = "polygons", include = TRUE) +
    as_reference("polygons", id = "displacement_map") + 
    with_displacement(
      text,
      x_map = ch_alpha("displacement_map"),
      y_map = ch_alpha("displacement_map"), 
      x_scale = 150,
      y_scale = -150
    )
)
