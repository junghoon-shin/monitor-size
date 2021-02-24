library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)
library(magrittr)
library(scales)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

get_size = function(diagnoal, horizontal, vertical) {
  m = sqrt(diagnoal^2 / (horizontal^2 + vertical^2))
  c(width = horizontal * m, height = vertical * m)
}

d = c(49, 43, 34, 32, 27)
h = c(32, 16, 21, 16, 16)
v = c(9, 9, 9, 9, 9)

sizetbl = pmap_dfr(list(d, h, v), ~{
  size = get_size(..1, ..2, ..3)
  tibble(diagonal = ..1, ratio = str_c(..2, ":", ..3), width = size[1], height = size[2])
})

maxdim = max(sizetbl$width, sizetbl$height) + 1

sizetbl %<>% mutate(width = width / maxdim, height = height / maxdim)

base_asp = 0.2/max(sizetbl$height)

sizetbl %<>% mutate(height = height * base_asp)

g = gList(rectGrob(y = unit(5:1/5 - 0.1, "npc"), width = sizetbl$width, height = sizetbl$height, gp = gpar(fill = "#41b6c4", col = "black")),
          textGrob(str_c(d, "''", " (", h, ":", v, ")"), y = unit(5:1/5 - 0.1, "npc"), gp = gpar(fontsize = 10)))

save_plot("monitor_size.tiff", as_ggplot(g), base_asp = base_asp)