library(tidyverse)
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )


theme_estat <- function(...) 
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )




ggplot(banco_final) +
  aes(x = imdb, y = engagement) +
geom_point(colour = "#A11D21", size = 3) +
labs(
x = "Nota IMDB",
y = "Engajamento"
) +
theme_estat()
ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
