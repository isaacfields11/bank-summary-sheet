library(gtable)
library(grid)
library(gridExtra)
library(extrafont)
loadfonts()

plot_spacing = unit(1.5,"mm")
plot_rounding = unit(2,"mm")
plot_inside_margin=3

background_color = "#0a2342"  #"#191927"
alt_color = "white"  #"#27293d"
plot_border_color = "grey85"  #"#3e3e56"
gridline_color = "grey90"   #"#392d4b"
main_text_color =  "#0a2342"
title_text_color = "white"

pop_color_1 = "#e14eca"
pop_color_2 = "#217acd"
pop_color_3 = "#00d6b4"

font_to_use = "Malgun Gothic"
font_title_size = 13
font_plot_title_size = 11
font_plot_text_size = 9
font_table_scaling = 0.5

plot_theme = theme(text=element_text(colour=main_text_color, size=font_plot_text_size, family=font_to_use),
                   axis.text = element_text(colour=main_text_color),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size=font_plot_title_size),
                   plot.title.position = "plot",
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_line(colour=gridline_color,size=0.1),
                   plot.background = element_rect(colour=plot_border_color, fill=alt_color, size=0.25),
                   panel.background = element_rect(fill=alt_color),
                   plot.margin = margin(t = plot_inside_margin, r = plot_inside_margin, b = plot_inside_margin, l = plot_inside_margin, unit = "mm"),
                   legend.position="bottom", legend.direction="horizontal", legend.key.size = unit(4, "mm"))



round_the_corners_ggplot = function(p) {
  g <- ggplotGrob(p)
  bg <- g$grobs[[1]]
  
  round_border_effect = roundrectGrob(x=bg$x, y=bg$y, 
                                      width=bg$width-plot_spacing, height=bg$height-plot_spacing,
                                      r=plot_rounding,
                                      just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
  g$grobs[[1]] <- round_border_effect  
  
  base_layer=rectGrob(gp=gpar(fill=background_color, col=background_color))
  gt = grobTree(base_layer, g)
  return(gt)
}


formatted_rounded_table = function(data,title){
  tt <- ttheme_default(core=list(fg_params=list(fontfamily=font_to_use,cex=font_table_scaling,col=main_text_color)), 
                       colhead=list(fg_params=list(fontfamily=font_to_use,cex=font_table_scaling,col=main_text_color)))
  t1 = tableGrob(data,theme=tt,rows=NULL)
  t1 = justify_tablegrobs(t1, hjust="center", vjust = "top", draw=FALSE)
  
  title_obj <- textGrob(title,
                        #x=plot_rounding, y=plot_rounding,
                        x=unit(plot_inside_margin,"mm"), y=unit(1,"npc")-unit(plot_inside_margin,"mm"), just="left", vjust=1,
                        gp=gpar(fontsize=font_plot_title_size,col=main_text_color, fontfamily=font_to_use))
  
  test = arrangeGrob(title_obj,t1, heights=c(0.15,0.85))
  
  # padding <- unit(3,"mm")
  # table <- gtable_add_rows(t1, heights = grobHeight(titleGrob) + padding, pos = 0)
  # table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))
  
  base_layer = rectGrob(gp=gpar(fill=background_color, col=background_color))
  
  round_border_effect = roundrectGrob(x=base_layer$x, y=base_layer$y,
                                      width=base_layer$width-plot_spacing, height=base_layer$height-plot_spacing,
                                      r=plot_rounding,
                                      just=base_layer$just, gp=gpar(fill=alt_color, col=plot_border_color), vp=base_layer$vp)
  
  gt = grobTree(base_layer, round_border_effect, test)
  
  return(gt)
}



title_grob = function(title) {
  gt = grobTree(rectGrob(gp=gpar(fill=background_color, col=background_color)),  
                textGrob(title,x=0.01,just="left",
                         gp=gpar(fontsize=font_title_size,col=title_text_color,fontface="bold", fontfamily=font_to_use)))
  return(gt)
}
