############################### setup #######################################
library(ggplot2)
library(ggthemes)
library(readr)
library(tidyverse)
library(zoo)
library(viridis)
library(hrbrthemes)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(ggpubr)
library(scales)

#source("produce_bankcsv.r")
download_dir="//hqwinfs1/ones2/Call Reports - Banks/Zip Files"
download_dir2="//hqwinfs1/ones2/Call Reports - Credit Unions/Zip Files"
bank_all<- read.csv(paste(download_dir,"bank_all.csv",sep="/"))
cu_all <- read.csv(paste(download_dir2,"cu_all.csv",sep="/"))
bank_all$repdte = as.Date(bank_all$repdte)
bank_latest_date = max(bank_all$repdte)

lay=rbind(c(1,2,3))

cu_latest_date = max(cu_all$CYCLE_DATE)
bank_all[is.na(bank_all)] <- 0
cu_all[is.na(cu_all)] <- 0
b <- c(0, 10000000, 150000000, Inf)
names <- c("Small", "Medium", "Large")
bank_all$bank_size <- cut(bank_all$asset, breaks = b, labels = names)

ONES_CUs <- cu_all %>%
  filter(CYCLE_DATE == cu_latest_date) %>%
  filter(asset > 10000000000) %>%
  select(CU_NUMBER) %>%
  pull()

ones_cu <- cu_all %>%
  filter(CU_NUMBER %in% ONES_CUs) %>%
  mutate(repdte = as.Date(CYCLE_DATE), cert=CU_NUMBER) %>%
  mutate(bank_size = "ONES CU")

small_banks <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(bank_size == "Small") %>%
  select(cert) %>%
  pull()
medium_banks <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(bank_size == "Medium") %>%
  select(cert) %>%
  pull()
large_banks <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(bank_size == "Large") %>%
  select(cert) %>%
  pull()

current_banks <- c(small_banks, medium_banks, large_banks)
bank_all3 <- bank_all %>%
  filter(cert %in% current_banks)
bank_small <- bank_all3 %>%
  filter(cert %in% small_banks)
bank_medium <- bank_all3 %>%
  filter(cert %in% medium_banks)
bank_large <- bank_all3 %>%
  filter(cert %in% large_banks)

bank_medium$p3other <- bank_medium$p3other + bank_medium$p3conoth
bank_medium$p9other <- bank_medium$p9other + bank_medium$p9conoth
p3means <- aggregate(cbind(p3re, p3ci, p3crcd, p3auto, p3other) ~ repdte, bank_medium, mean)
p3means_table <- tail(p3means, n=4) %>%
  mutate(Date = repdte, `Real Estate` = p3re*100, Commercial = p3ci*100, `Credit Card` = p3crcd*100, Auto = p3auto*100, Other = (p3other)*100) %>%
  select(Date, `Real Estate`, Commercial, `Credit Card`, Auto, Other)
p3means_table_values <- signif(p3means_table %>% select(-c(Date)), digits=4)
p3means_table <- cbind(p3means_table$Date, p3means_table_values)
p3means_table$Date <- p3means_table$`p3means_table$Date`

p9means <- aggregate(cbind(p9re, p9ci, p9crcd, p9auto, p9other) ~ repdte, bank_medium, mean)
p9means_table <- tail(p9means) %>%
  mutate(Date = repdte, `Real Estate` = p9re, Commercial = p9ci, `Credit Card` = p9crcd, Auto = p9auto, Other = p9other) %>%
  select(Date, `Real Estate`, Commercial, `Credit Card`, Auto, Other)
asset_means <- aggregate(cbind(coredep, asset, asset5, ernast, lnlsgr, p9_delinquent, p3_delinquent, nclnls, nim) ~ repdte, bank_medium, mean)
cu_asset_means <- aggregate(cbind(asset, dep, ernast, nim) ~ repdte, ones_cu, mean)
asset_means$nim2 <- 0
asset_means$nim3 <- 0
asset_means <- asset_means %>%
  mutate(nim2 = lag(nim)) %>%
  mutate(assety = lag(asset, n=4)) %>%
  mutate(asset_delta = asset-assety) %>%
  mutate(asset_percent = asset_delta/assety) %>%
  mutate(coredepy = lag(coredep, n=4)) %>%
  mutate(coredep_delta = coredep-coredepy) %>%
  mutate(coredep_percent = coredep_delta/coredepy)
asset_means[is.na(asset_means)] <- 0
asset_means <- asset_means %>%
  mutate(nim3 = nim - nim2)
asset_means$nim4 <- ifelse(asset_means$nim3 > 0, asset_means$nim3 <- asset_means$nim3, asset_means$nim3 <- asset_means$nim)
asset_means$month = month(asset_means$repdte)
cu_asset_means$month = month(cu_asset_means$repdte)
cu_asset_means <- cu_asset_means %>%
  mutate(cu_assety = lag(asset, n=4)) %>%
  mutate(cu_asset_delta = asset - cu_assety) %>%
  mutate(cu_asset_percent = cu_asset_delta/cu_assety) %>%
  mutate(cu_depy = lag(dep, n=4)) %>%
  mutate(cu_dep_delta = dep-cu_depy) %>%
  mutate(cu_dep_percent = cu_dep_delta/cu_depy)

p3means_stack <- p3means %>%
  pivot_longer(-repdte, names_to="Type", values_to="Amount")
p9means_stack <- p9means %>%
  pivot_longer(-repdte, names_to="Type", values_to="Amount")
asset_means_stack <- asset_means %>%
  subset(select=c(repdte, p3_delinquent, p9_delinquent)) %>%
  pivot_longer(-repdte, names_to="Type", values_to="Amount")

##################################### top banks ################################
cu_size_overdue <- subset(bank_medium, select=c(cert, name, repdte,
                                                p3re, p3ci, p3crcd, p3auto, p3conoth, p3other, 
                                                p9re, p9ci, p9crcd, p9auto, p9conoth, p9other))
cu_size_stack = cu_size_overdue %>% pivot_longer(cols=-c(cert,name,repdte), names_to="Type", values_to="Amount")

banks_to_plot = bank_medium %>%
  filter(repdte == bank_latest_date) %>%
  slice_max(order_by=p3_delinquent, n=9) %>%
  select(cert) %>%
  pull()
top_banks <- cu_size_stack %>% 
  filter(cert %in% banks_to_plot)
current_names = top_banks %>%
  filter(repdte==bank_latest_date) %>%
  filter(Type=="p3re") %>%
  select(cert, name) %>%
  mutate(current=name) %>%
  select(cert, current)
top_banks<- merge(current_names, top_banks, by="cert")
top_banks$ordered_banks <- fct_rev(fct_reorder(top_banks$current, top_banks$Amount, max))

################# delinquency charts ############################################
ones_cu_lnatres <- ones_cu %>%
  mutate(latecrcd = late_crcd/total_crcd, lateauto = late_auto/total_auto, latere = late_re/total_re, lnlsgr=ernast) %>%
  select(repdte, bank_size, lnatres2, lnlsgr, latecrcd, lateauto, latere)

bank_lnatres <- bank_all3 %>%
  select(repdte, bank_size, lnatres2, lnlsgr, latecrcd, lateauto, latere)
bank_lnatres <- rbind(bank_lnatres, ones_cu_lnatres)
bank_lnsmall <- bank_lnatres %>% 
  filter(bank_size == "Small")
bank_lnmed <- bank_lnatres %>%
  filter(bank_size == "Medium")
bank_lnlarge <- bank_lnatres %>%
  filter(bank_size == "Large")
cu_ln <- bank_lnatres %>%
  filter(bank_size == "ONES CU")

small_lnameans <- aggregate(bank_lnsmall$lnatres2, by = list(bank_lnsmall$repdte), mean, na.rm=TRUE)
medium_lnameans <- aggregate(bank_lnmed$lnatres2, by = list(bank_lnmed$repdte), mean, na.rm=TRUE)
large_lnameans <- aggregate(bank_lnlarge$lnatres2, by = list(bank_lnlarge$repdte), mean, na.rm=TRUE)
cu_lnameans <- aggregate(cu_ln$lnatres2, by = list(cu_ln$repdte), mean, na.rm=TRUE)
small_lnameans$bank_size <- "Small"
medium_lnameans$bank_size <- "Medium"
large_lnameans$bank_size <- "Large"
cu_lnameans$bank_size <- "ONES CU"

small_lnameans[4:5] <- aggregate(bank_lnsmall$lnlsgr, by = list(bank_lnsmall$repdte), mean, na.rm=TRUE)
medium_lnameans[4:5] <- aggregate(bank_lnmed$lnlsgr, by = list(bank_lnmed$repdte), mean, na.rm=TRUE)
large_lnameans[4:5] <- aggregate(bank_lnlarge$lnlsgr, by = list(bank_lnlarge$repdte), mean, na.rm=TRUE)
cu_lnameans[4:5] <- aggregate(cu_ln$lnlsgr, by = list(cu_ln$repdte), mean, na.rm=TRUE)


small_lnameans[6:7] <- aggregate(bank_lnsmall$latecrcd, by = list(bank_lnsmall$repdte), mean, na.rm=TRUE)
medium_lnameans[6:7] <- aggregate(bank_lnmed$latecrcd, by = list(bank_lnmed$repdte), mean, na.rm=TRUE)
large_lnameans[6:7] <- aggregate(bank_lnlarge$latecrcd, by = list(bank_lnlarge$repdte), mean, na.rm=TRUE)
cu_lnameans[6:7] <- aggregate(cu_ln$latecrcd, by = list(cu_ln$repdte), mean, na.rm=TRUE)

small_lnameans[8:9] <- aggregate(bank_lnsmall$lateauto, by = list(bank_lnsmall$repdte), mean, na.rm=TRUE)
medium_lnameans[8:9] <- aggregate(bank_lnmed$lateauto, by = list(bank_lnmed$repdte), mean, na.rm=TRUE)
large_lnameans[8:9] <- aggregate(bank_lnlarge$lateauto, by = list(bank_lnlarge$repdte), mean, na.rm=TRUE)
cu_lnameans[8:9] <- aggregate(cu_ln$lateauto, by = list(cu_ln$repdte), mean, na.rm=TRUE)


small_lnameans[10:11] <- aggregate(bank_lnsmall$latere, by = list(bank_lnsmall$repdte), mean, na.rm=TRUE)
medium_lnameans[10:11] <- aggregate(bank_lnmed$latere, by = list(bank_lnmed$repdte), mean, na.rm=TRUE)
large_lnameans[10:11] <- aggregate(bank_lnlarge$latere, by = list(bank_lnlarge$repdte), mean, na.rm=TRUE)
cu_lnameans[10:11] <- aggregate(cu_ln$latere, by = list(cu_ln$repdte), mean, na.rm=TRUE)

lnameans <- rbind(small_lnameans, medium_lnameans, large_lnameans, cu_lnameans)
lnameans <- lnameans %>%
  select(Group.1, bank_size, x, x.1, x.2, x.3, x.4)
colnames(lnameans)<- c("repdte", "bank_size", "lnatres2", "lnlsgr", "latecrcd", "lateauto", "latere")
lnameans$bank_size <- factor(lnameans$bank_size, levels = c("Small", "Medium", "Large", "ONES CU"))
############################# reboxplot #####################################
ones_cu_box <- ones_cu %>%
  mutate(realntre = rollre_co/rollre, realntauto=rollauto_co/rollauto, realntcrcd=rollcrcd_co/rollcrcd) %>%
  select(repdte, bank_size, cert, ernast, realntre, realntauto, realntcrcd) %>%
  filter(repdte == cu_latest_date)
ones_cu_boxre <- ones_cu_box %>%
  select(cert, repdte, bank_size, realntre)
bank_allrenames <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(lnre/lnlsgr > .01) %>%
  select(cert) %>%
  pull()
re_banks <- bank_all %>%
  filter(cert %in% bank_allrenames) %>%
  filter(repdte == bank_latest_date) %>%
  mutate(realntre=ntre3/lnre3) %>%
  select(cert, repdte, bank_size, realntre)
re_box <- rbind(re_banks, ones_cu_boxre)
################### autoboxplot ################################################
ones_cu_boxauto <- ones_cu_box %>%
  select(cert, repdte, bank_size, realntauto)
bank_allautonames <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(lnauto/lnlsgr > .01) %>%
  select(cert) %>%
  pull()
auto_banks <- bank_all %>%
  filter(cert %in% bank_allautonames) %>%
  filter(repdte == bank_latest_date) %>%
  mutate(realntauto=ntauto3/lnauto3) %>%
  select(cert, repdte, bank_size, realntauto)
auto_box <- rbind(auto_banks, ones_cu_boxauto)
############################# crcdboxplot #####################################
ones_cu_boxcrcd <- ones_cu_box %>%
  select(cert, repdte, bank_size, realntcrcd)
bank_allcrcdnames <- bank_all %>%
  filter(repdte == bank_latest_date) %>%
  filter(lncrcd/lnlsgr > .01) %>%
  select(cert) %>%
  pull()
crcd_banks <- bank_all %>%
  filter(cert %in% bank_allcrcdnames) %>%
  filter(repdte == bank_latest_date) %>%
  mutate(realntcrcd=ntcrcd3/lncrcd3) %>%
  select(cert, repdte, bank_size, realntcrcd)
crcd_box <- rbind(crcd_banks, ones_cu_boxcrcd)