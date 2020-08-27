  save_filename="final_patchwork.pdf"
  
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(dplyr)
  library(gtable)
  source("reporting_graphics_helpers.r")
  source("pdesign_plots.r")
  source("produce_plots.r")

  
  # plots ############################################################################################
  
  asset_growth = round_the_corners_ggplot(asset_growth + plot_theme + ggtitle("Asset Growth"))
  nim_graph = round_the_corners_ggplot(nim_graph + plot_theme + ggtitle("Net Interest Margin") + theme(legend.position = "none"))
  dep_graph = round_the_corners_ggplot(dep_graph + plot_theme + ggtitle("Deposit Growth") + theme(legend.position = "none"))
  alll = round_the_corners_ggplot(alll + plot_theme + ggtitle("Allowance for Loan and Lease Loss"))
  delinquency_cot = round_the_corners_ggplot(delinquency_cot + plot_theme + ggtitle("Delinquency Rates") + theme(legend.justification = "left"))
  p = round_the_corners_ggplot(p + plot_theme + ggtitle("Loans 30-89 Days Delinquent") + theme(legend.position = "none"))
  q = round_the_corners_ggplot(q + plot_theme + ggtitle("Loans 90+ Days Delinquent"))
  mort_delinquency = round_the_corners_ggplot(mort_delinquency + plot_theme + ggtitle("Mortgage Delinquency Rates"))
  auto_delinquency = round_the_corners_ggplot(auto_delinquency + plot_theme + ggtitle("Auto Delinquency Rates") + theme(legend.position = "none"))
  crcd_delinquency = round_the_corners_ggplot(crcd_delinquency + plot_theme + ggtitle("Credit Card Delinquency Rates") + theme(legend.position = "none"))
  reboxplot = round_the_corners_ggplot(reboxplot + plot_theme + ggtitle("Net Real Estate Chargeoffs") + theme(legend.position = "none"))
  autoboxplot = round_the_corners_ggplot(autoboxplot + plot_theme + ggtitle("Net Auto Loan Chargeoffs") + theme(legend.position = "none"))
  crcdboxplot = round_the_corners_ggplot(crcdboxplot + plot_theme + ggtitle("Net Credit Card Chargeoffs") + theme(legend.position = "none"))
  
  # tables ###########################################################################################
  library(data.table)
  data= setDT(p3means_table,keep.rownames=TRUE)
  title="Loans 30-89 Days Delinquent"
  t1=formatted_rounded_table(data,title)
  
  
  # assemble for grid.arrange ########################################################################
  # page 1
  header = title_grob("Banking Industry Summary Sheet")
  col1 = arrangeGrob(grobs=list(asset_growth,nim_graph,dep_graph),nrow=3,ncol=1, heights=c(0.4,0.3,0.3))
  col2 = arrangeGrob(grobs=list(alll,delinquency_cot,p,q),nrow=4,ncol=1, heights=c(0.3,0.23,0.2,0.27))
  col3 = arrangeGrob(grobs=list(mort_delinquency,auto_delinquency,crcd_delinquency),nrow=3,ncol=1, heights=c(0.4,0.3,0.3))
  col4 = arrangeGrob(grobs=list(reboxplot,autoboxplot,crcdboxplot),nrow=3,ncol=1)
  page1_grobs=list(header,col1,col2,col3,col4)
  page1_layout=rbind(c(1,1,1,1), c(2,3,4,5))
  page1_heights=unit(c(0.5,8), "in")
  page1_widths=c(0.25,0.25,0.25,0.25)
  
  
  # output to pdf ####################################################################################
  # save
  cairo_pdf(save_filename,width = 11, height = 8.5, onefile = TRUE)
  grid.arrange(grobs=page1_grobs, layout_matrix=page1_layout, heights=page1_heights, widths=page1_widths)
  #grid.arrange(grobs=page2_grobs, layout_matrix=page2_layout, heights=page2_heights)
  dev.off()
  
  # open
  command = paste0("open \"",save_filename,"\"")
  system(command)
