# 
project_pal <- c("#bbd9eb", "#9585AB", "#864C67", "#073552")

library(ggplot2)
project_theme <- 
  ggthemes::theme_tufte(ticks = FALSE) +
  # get open sans: https://cran.r-project.org/web/packages/gfonts/vignettes/gfonts.html
  ggplot2::theme(text = element_text(family = "sans"),
                 plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5),
                 plot.background = element_rect(fill = project_pal[1], color = project_pal[1]))

                 #"#bbd9eb")
