theme_invisible <- function(base_size = 12) {
  structure(list(
    axis.line = theme_void,
    axis.text.x = theme_text(
      colour = NA, size = base_size * 0.8,
      lineheight = 0.9, vjust = 1
    ),
    axis.text.y = theme_text(
      colour = NA, size = base_size * 0.8,
      lineheight = 0.9, hjust = 1
    ),
    axis.ticks = theme_segment(colour = NA, size = 0.2),
    axis.title.x = theme_text(colour = NA, size = base_size, vjust = 1),
    axis.title.y = theme_text(
      colour = NA, size = base_size, angle = 90,
      vjust = 0.5
    ),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),

    legend.background = theme_rect(colour = NA),
    legend.key = theme_rect(colour = NA),
    legend.key.size = unit(1.2, "lines"),
    legend.text = theme_text(colour = NA, size = base_size * 0.8),
    legend.title = theme_text(
      colour = NA, size = base_size * 0.8,
      face = "bold", hjust = 0
    ),
    legend.position = "right",

    panel.background = theme_rect(fill = NA, colour = NA),
    panel.border = theme_rect(fill = NA, colour = NA),
    panel.grid.major = theme_line(colour = NA, size = 0.2),
    panel.grid.minor = theme_line(colour = NA, size = 0.5),
    panel.margin = unit(0.25, "lines"),

    strip.background = theme_rect(fill = NA, colour = NA),
    strip.text.x = theme_text(colour = NA, size = base_size * 0.8),
    strip.text.y = theme_text(
      colour = NA, size = base_size * 0.8,
      angle = -90
    ),

    plot.background = theme_rect(colour = NA),
    plot.title = theme_text(colour = NA, size = base_size * 1.2),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}
