line_size = 1
# header_size =18
# vjust = .5
# Malgun <- "Malgun Gothic"
# look <- theme_set(theme_ipsum_pub()) + 
#   theme_update(legend.position="bottom", legend.direction="horizontal", 
#                plot.margin = margin(t=5, l=5, b=5, r=5), axis.title.x=element_blank(),
#                axis.title.y = element_text(family = Malgun, size=12),
#                plot.title = element_text(family = Malgun, size=header_size), text =element_text(family = Malgun, size=12),
#                )
#palette30 = c("#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99")
palette90 = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#b15928")
primaries = c("#CCCC00", "#1F78B4", "#ff0000", "#33a02c")
green = "#33a02c"
navy = "#123e75"
top3palette = c(navy, green) #c("#0b7552", "#b31b1b")


#mean total asset growth 2007-2020
asset_growth <- asset_means %>%
  filter(month == 12) %>%
  filter(repdte > as.Date("2013-12-31")) %>%
  ggplot(aes(x=repdte, y=asset_percent, color=navy)) +
  xlab("Year") +
  ylab(NULL) +
  scale_y_continuous(labels=percent) +
  geom_line(size=line_size) +
  geom_point(color=navy,size=line_size) +
  geom_line(data=cu_asset_means %>% filter(repdte > as.Date("2013-12-31")) %>% filter(month == 12), aes(x=repdte, y=cu_asset_percent, color=green), size=line_size) +
  geom_point(data=cu_asset_means %>% filter(repdte > as.Date("2013-12-31")) %>% filter(month == 12), aes(x=repdte, y=cu_asset_percent, color=green), size=line_size) + 
  scale_color_identity(name=NULL, breaks = top3palette,labels = c("Banks", "ONES CUs"), guide="legend")
 asset_growth 
# 
# #mean net interest margin
nim_graph <- asset_means %>%
  filter(month == 12) %>%
  filter(as.Date(repdte) > as.Date("2007-12-31")) %>%
  ggplot(aes(x=as.Date(repdte), y=nim/lnlsgr, color=navy)) +
  scale_y_continuous(labels=percent) +
  ylab(NULL) +
  geom_line(size=line_size) +
  geom_point(size=line_size, color=navy) +
  geom_line(data=cu_asset_means %>% filter(as.Date(repdte) > as.Date("2007-12-31")) %>% filter(month == 12), aes(x=repdte, y=nim/ernast, color=green), size=line_size) +
  geom_point(data=cu_asset_means %>% filter(as.Date(repdte) > as.Date("2007-12-31")) %>% filter(month == 12), aes(x=repdte, y=nim/ernast, color=green), size=line_size) +
  scale_color_identity(name=NULL, breaks = top3palette,labels = c("Banks", "ONES CUs"), guide="legend")
 nim_graph
# 
# ### mean deposit growth
dep_graph <- asset_means %>%
  filter(month == 12) %>%
  filter(repdte > as.Date("2013-12-31")) %>%
  ggplot(aes(x=repdte, y=coredep_percent, color=navy)) +
  scale_y_continuous(labels=percent) +
  ylab(NULL) +
  geom_line(size=line_size) +
  geom_point(color=navy, size=line_size) +
  geom_line(data=cu_asset_means %>% filter(repdte > as.Date("2013-12-31")) %>% filter(month == 12), aes(x=repdte, y=cu_dep_percent, color=green), size=line_size) +
  geom_point(data=cu_asset_means %>% filter(repdte > as.Date("2013-12-31")) %>% filter(month == 12), aes(x=repdte, y=cu_dep_percent, color=green), size=line_size) +
  scale_color_identity(name=NULL, breaks = top3palette,labels = c("Banks", "ONES CUs"), guide="legend")
# dep_graph

###loan and lease loss allowance by size
alll <- lnameans %>%
  ggplot(aes(x=repdte, y=lnatres2, group=bank_size, color=bank_size))+
  geom_line(size=line_size)+
  scale_y_continuous(labels=percent) +
  ylab(NULL) +
  guides(col =guide_legend(nrow =2, ncol=2, byrow =TRUE))+
  scale_color_manual(guide=(guide_legend(ncol=2, byrow =TRUE)),
                     name=NULL,
                     labels=c("Small", "Medium", "Large", "ONES CU"),
                     values = primaries)

#mean % assets past due
delinquency_cot <- asset_means_stack %>%
  ggplot(aes(x=repdte, y=Amount, group=Type, fill=Type)) +
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(labels=percent) +
  ylab("Assets Past Due")+
  #ggtitle("Delinquency Rate Over Time")+
  scale_fill_manual(name=NULL,
                    labels=c("30-89 DPD", "90+ DPD"),
                    values = c("#1F78B4","#ff0000")) +
  theme(legend.justification = "left")

#mean % delinquency for p3 and p9
p <- p3means_stack %>%
  mutate(Type = fct_reorder(Type, Amount)) %>%
  ggplot(aes(x=repdte, y=Amount, group=Type, fill=Type)) +
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(labels=percent) +
  ylab("Assets Past Due") +
  ggtitle("30-89 Days Past Due") +
  scale_fill_manual(guide=(guide_legend(ncol=3, byrow =TRUE)),
    name=NULL,values=palette90,
                    breaks=c("p3re", "p3crcd", "p3auto", "p3ci", "p3other"),
                    labels=c("Real Estate", "Credit Card", "Auto", "Commercial", "Other"))

q <- p9means_stack %>%
  mutate(Type = fct_reorder(Type, Amount)) %>%
  ggplot(aes(x=repdte, y=Amount, group=Type, fill=Type)) +
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(labels=percent) +
  #ylab("Assets Past Due") +
  ggtitle("90+ Days Past Due") +
  guides(col =guide_legend(nrow =2, ncol=3, byrow =TRUE))+
  scale_fill_manual(guide=(guide_legend(ncol=3, byrow =TRUE)),
    name=NULL,values=palette90,
                    breaks=c("p9re", "p9crcd", "p9auto", "p9ci", "p9other"),
                    labels=c("Real Estate", "Credit Card", "Auto", "Commercial", "Other"))
q

#change in mean delinquency by type over time
mort_delinquency <- lnameans %>%
  ggplot(aes(x=repdte, y=latere, group=bank_size, color=bank_size))+
  geom_line(size=1)+
  scale_y_continuous(labels=percent) +
  ylab("Loans Past Due") +
  guides(col =guide_legend(nrow =2, ncol=2, byrow =TRUE))+
  scale_color_manual(guide=(guide_legend(ncol=2, byrow =TRUE)),
                            name=NULL,
                     labels=c("Small", "Medium", "Large", "ONES CU"),
                     values = primaries)

auto_dataframe <- lnameans %>%
  mutate(year = as.numeric(repdte)) %>%
  filter(year > 15000)
auto_delinquency <- auto_dataframe %>%
  ggplot(aes(x=repdte, y=lateauto, group=bank_size, color=bank_size))+
  geom_line(size=1)+
  scale_y_continuous(labels=scales::percent_format(accuracy = .5)) +
  ylab("Loans Past Due") +
  scale_color_manual(name=NULL,
                     labels=c("Small", "Medium", "Large", "ONES CU"),
                     values = primaries)

crcd_delinquency <- lnameans %>%
  ggplot(aes(x=repdte, y=latecrcd, group=bank_size, color=bank_size))+
  geom_line(size=1)+
  scale_y_continuous(labels=percent) +
  ylab("Loans Past Due") +
  scale_color_manual(name=NULL,
                    labels=c("Small", "Medium", "Large", "ONES CU"),
                    values = primaries)
  #ggtitle("Credit Card Delinquency\nby Bank Size")

total_loans <- lnameans %>%
  ggplot(aes(x= repdte, y= lnlsgr, group=bank_size, fill=bank_size)) +
  geom_line(size=line_size) +
  facet_wrap(~bank_size, scales="free_y") +
  scale_y_continuous(labels=scales::comma)

#Current top 9 highest delinquency %
top9 <- top_banks %>%
  ggplot(aes(x= repdte, y= Amount, group=Type, fill=Type)) +
  geom_bar(data=top_banks, stat="identity") +
  scale_color_viridis(discrete = TRUE) +
  theme(
    plot.title = element_text(size=6),
    panel.grid = element_blank()
  ) +
  ggtitle("ONES CU Size Banks w/ Highest % of Overdue Debt") +
  xlab("Year") +
  ylab("% of Assets Overdue") +
  facet_wrap(~ordered_banks, scales="free_y") +
  scale_y_continuous(labels=scales::comma) +
  theme(strip.text = element_text(size = 6))
### Why are there holes in this graph? ###

#re_banks$bank_size <- fct_rev(re_banks$bank_size)
ylim1 = boxplot.stats(re_box$realntre)$stats[c(1, 5)]
reboxplot <- re_box %>%
  ggplot(aes(x=bank_size, y= realntre, group=bank_size, fill=bank_size)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(labels=scales::percent_format(accuracy = .02)) +
  xlab("Bank Size") +
  ylab("Net Chargeoff Rate") +
  scale_fill_manual(values = primaries) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = ylim1*1.05)
#reboxplot

#auto_banks$bank_size <- fct_rev(auto_banks$bank_size)
ylim1 = boxplot.stats(auto_box$realntauto)$stats[c(1, 5)]
autoboxplot <- auto_box %>%
  ggplot(aes(x=bank_size, y= realntauto, group=bank_size, fill=bank_size)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Bank Size") +
  ylab("Net Chargeoff Rate") +
  scale_fill_manual(values = primaries)+
  theme(legend.position = "none") +
  coord_cartesian(ylim = ylim1*1.05)
#autoboxplot

#crcd_banks$bank_size <- fct_rev(crcd_banks$bank_size)
ylim1 = boxplot.stats(crcd_box$realntcrcd)$stats[c(1, 5)]
crcdboxplot <- crcd_box %>%
  ggplot(aes(x=bank_size, y= realntcrcd, group=bank_size, fill=bank_size)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(labels=percent) +
  xlab("Bank Size") +
  ylab("Net Chargeoff Rate") +
  scale_fill_manual(values = primaries)+
  theme(legend.position = "none") +
  coord_cartesian(ylim = ylim1*1.05)
#crcdboxplot