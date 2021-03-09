# set up file for graphs and exporting figures

# packages
library(ggplot2)
library(tikzDevice)
library(extrafont)
library(RColorBrewer)
library(TeachingDemos)

graphs <- list()

# palettes
graphs$palettes <- list()
graphs$palettes[["RBG"]] <- c('#e31a1c', '#41b6c4','#c2e699') # red-blue-green
graphs$palettes[["BlueRed"]] <- c('#41b6c4','#e31a1c') # blue-red


# line size for graphs
graphs$linesize <- 1

graphs$dims <- data.frame("width" = rep(NA, 3), "height" = rep(NA, 3))

# width and heights of graphs for 3 graphs in a row
graphs$dims$width[3] <- 2
graphs$dims$height[3] <- 3/4*graphs$dims$width[3]

# width and heights of graphs for 2 graphs in a row
graphs$dims$width[2] <- 3/2*graphs$dims$width[3]
graphs$dims$height[2] <- 3/2*graphs$dims$height[3]

# width and heights of graphs for 1 graph in a row
graphs$dims$width[1] <- 3/2*graphs$dims$width[2]
graphs$dims$height[1] <- 3/2*graphs$dims$height[2]

# set plot theme
theme_set(
  theme_classic(
    base_size = 10
  )
)

theme_update(
  legend.position = "bottom",
  legend.background = element_rect(colour = '#252525', size = 0.25*graphs$linesize),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
)

# set default palettes for colors and fills
scale_fill_discrete <- function(...) {
  ggsci::scale_fill_jco(name = NULL)
}

scale_colour_discrete <- function(...) {
  ggsci::scale_color_jco(name = NULL)
}
