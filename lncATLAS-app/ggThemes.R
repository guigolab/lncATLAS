library(grid)
library(ggthemes)
library(RColorBrewer)

geneID <- "ENSG00000251562"


text_sizes <- theme(
  plot.title = element_text(face = "bold",
              size = rel(1.75), hjust = 0.5),
  axis.title = element_text(size = rel(1.5)),
  axis.text.y = element_text(size = rel(1.5)),
  axis.text.x = element_text(size = rel(1.4)),
  strip.text = element_text(size = rel(1.5)),
  legend.text = element_text(size = rel(1.2)),
  legend.title = element_text(face="italic", size = rel(1.2))
  )


theme_lncatlas_Ratio <- function(base_size = 15) {
  (theme_foundation(base_size=base_size)
  + theme(
    text = element_text(),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    panel.grid.major.y = element_line(colour="#e6e6e6", size = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(colour="#808080"),
    axis.line.x = element_line(colour="#808080"),
    axis.title.y = element_text(angle=90,vjust =2),
    axis.title.x = element_text(vjust = -0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "cm"),
    legend.margin = unit(0.2, "cm")
    )
  + text_sizes)
}





theme_lncatlas_2Ddistr <- function(base_size = 15, base_family="helvetica") {
  (theme_foundation(base_size=base_size)
   + theme(
     text = element_text(),
     panel.background = element_rect(colour = NA),
     plot.background = element_rect(colour = NA),
     panel.border = element_rect(colour = NA),
     panel.grid.major.y = element_line(colour="#e6e6e6", size = 0.5),
     panel.grid.major.x = element_line(colour="#e6e6e6", size = 0.5),
     panel.grid.minor = element_blank(),
     axis.line.y = element_line(colour="#808080"),
     axis.line.x = element_line(colour="#808080"),
     axis.title.y = element_text(angle=90,vjust =2),
     axis.title.x = element_text(vjust = -0.5),
     axis.ticks = element_blank(),
     legend.position = "bottom",
     legend.direction = "vertical",
     legend.key.size = unit(0.5, "cm"),
     legend.margin = unit(0.2, "cm")
   )
   + text_sizes)
}


theme_lncatlas_distr <- function(base_size = 15, base_family="helvetica") {
  (theme_foundation(base_size=base_size)
   + theme(
     text = element_text(),
     panel.background = element_rect(colour = NA),
     plot.background = element_rect(colour = NA),
     panel.border = element_rect(colour = NA),
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank(),
     axis.line.y = element_line(colour="#808080"),
     axis.line.x = element_line(colour="#808080"),
     axis.title.y = element_text(angle=90,vjust =2),
     axis.title.x = element_text(vjust = -0.5),
     axis.ticks = element_blank(),
     legend.key = element_rect(colour = NA),
     legend.position = c(0.8, 0.9),
     legend.direction = "vertical",
     legend.key.size = unit(0.5, "cm"),
     legend.margin = unit(0.2, "cm")
   )
   + text_sizes)
}



theme_lncatlas_distrK <- function(base_size = 15, base_family="helvetica") {
  (theme_foundation(base_size=base_size)
   + theme(
     text = element_text(),
     panel.background = element_rect(colour = NA),
     plot.background = element_rect(colour = NA),
     panel.border = element_rect(colour = NA),
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank(),
     axis.line.y = element_line(colour="#808080"),
     axis.line.x = element_line(colour="#808080"),
     axis.title.y = element_text(angle=90,vjust =2),
     axis.title.x = element_text(vjust = -0.5),
     axis.ticks = element_blank(),
     legend.key = element_rect(colour = NA),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.key.size = unit(0.5, "cm"),
     legend.margin = unit(0.2, "cm")
   )
   + text_sizes)
}


scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#ed8b36","#7fc97f","#662506","#ef3b2c","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#ed8b36","#7fc97f","#662506","#ef3b2c","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_fill_lncatlas_Ratio <- function(){
  library(scales)
  continuous_scale("fill","Ratio",
          gradient_n_pal(colours = colorRampPalette(
              brewer.pal(9, "Oranges")[4:9])(10)
          ))
}

scale_color_lncatlas_Ratio <- function(){
  library(scales)
  continuous_scale("colour","Ratio",
                   gradient_n_pal(colours = colorRampPalette(
                     brewer.pal(9, "Oranges")[c(2,9)])(2)
                   ))
}
