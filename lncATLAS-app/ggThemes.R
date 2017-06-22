################################################################################
################################################################################
#                        lncATLAS -- Themes for the ggplots
#
#         author: David Mas-Ponte @CRG
#         Themes for ggplots
#
################################################################################
################################################################################

library(grid)
library(RColorBrewer)

geneID <- "ENSG00000251562"

# 3 themes and a general theme for the text size

text_sizes <- theme(
  text = element_text(size = 15),
  plot.title = element_text(face = "bold",
              size = rel(1.75), hjust = 0.5),
  axis.title = element_text(size = rel(1.5)),
  axis.text.y = element_text(size = rel(1.5)),
  axis.text.x = element_text(size = rel(1.4)),
  strip.text = element_text(size = rel(1.4)),
  legend.text = element_text(size = rel(1.2)),
  legend.title = element_text(face="italic", size = rel(1.2))
  )

tdist <- theme(plot.subtitle = element_text(vjust = 1),
               plot.caption = element_text(vjust = 1),
               axis.line = element_line(size = 0.5,
                                        linetype = "solid"),
               axis.ticks = element_line(linetype = "blank"),
               panel.grid.major = element_line(colour = "black",
                                               size = 0.2, linetype = "dashed"),
               panel.grid.minor = element_line(linetype = "blank"),
               panel.background = element_rect(fill = NA),
               legend.position = c(0.8, 0.9),
               legend.direction = "vertical",
               legend.key.size = unit(0.5, "cm"),
               legend.margin = unit(0.2, "cm"),
               legend.key = element_rect(fill = NA))

tk  <- theme(plot.subtitle = element_text(vjust = 1),
             axis.line = element_line(size = 0.5,
                                      linetype = "solid"),
             axis.ticks = element_line(linetype = "blank"),
             panel.grid.major.y = element_line(colour = "black",
                                               linetype = "dashed",
                                               size = 0.2),
             panel.grid.major.x = element_blank(),
             panel.grid.minor = element_blank(),
             axis.title = element_text(size = 14),
             axis.text = element_text(size = 14),
             plot.title = element_text(size = 15,
                                       face = "bold",
                                       hjust = 0.5),
             panel.background = element_rect(fill = NA),
             legend.position = "bottom", legend.direction = "horizontal",
             legend.key = element_rect(fill = NA))

theme_lncatlas_Ratio <- theme(
  axis.line = element_line(size = 0.5,linetype = "solid",color = "black"),
  axis.ticks = element_line(linetype = "blank"),
  panel.grid.major.y = element_line(colour = "black",
            size = 0.2, linetype = "dashed"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 90),
  plot.title = element_text(size = 20,
                            face = "bold", hjust = 0.5),
  panel.background = element_rect(fill = NA),
  legend.key = element_rect(fill = NA),
  legend.background = element_rect(fill = NA),
  legend.position = "bottom",
  legend.direction = "horizontal",
  strip.background = element_rect(colour = "black", fill = "#f9dea4",
                                  size = 1))





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
