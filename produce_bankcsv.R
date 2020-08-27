library(stringr)
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

call_report_url="https://www7.fdic.gov/sdi/download_large_list_outside.asp"
url_prefix="https://www7.fdic.gov/sdi/"
#search_string="//a[contains(@href,'zip') and contains(@href,'_2007') or contains(@href,'_2008') or contains(@href,'_2009') or contains(@href,'_201') or contains(@href,'_202')]"
search_string="//a[contains(@href,'zip') and contains(@href, '_2013') or contains(@href,'_2014') or contains(@href,'_2015') or contains(@href,'_2016') or contains(@href,'_2017') or contains(@href,'_2018') or contains(@href,'_2019') or contains(@href,'_202')]"
download_dir="//hqwinfs1/ones2/Call Reports - Banks/Zip Files"

# get all links on page
page_links=read_html(call_report_url) %>% 
  html_nodes("body") %>% 
  xml_find_all(search_string) %>%
  html_attr("href")

# get filenames at source
source_filenames = page_links %>% as.data.frame() %>% 
  rename(URL=1) %>% mutate(URL=as.character(URL)) %>%
  mutate(Filename=gsub("^.*/", "", URL))

# get all files already downloaded
dest_filenames=list.files(download_dir, full.names = FALSE, pattern="All_Reports.*.zip", recursive = FALSE) %>% 
  as.data.frame() %>% rename(Filename=1) %>% mutate(AlreadyDownloaded=TRUE)

# get list of files that have not already been downloaded
files_to_download = source_filenames %>% left_join(dest_filenames) %>% filter(is.na(AlreadyDownloaded))
for (filename in (files_to_download  %>% select(URL) %>% pull())) {
  url=paste0(url_prefix,filename)
  download.file(url,paste0(download_dir,"/",tail(str_split(url,"/")[[1]],1)))
}

bank_bs = data.frame()
bank_pd = data.frame()
bank_ie = data.frame()
bank_nll = data.frame()
bank_co = data.frame()
bank_dp = data.frame()

filenames = source_filenames %>% 
  mutate(Filename=paste(download_dir,Filename,sep="/")) %>% 
  select(Filename) %>% pull()
for (filename in filenames) {
  contained_files = unzip(filename,list=TRUE)
  
  test1=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"Assets and Liabilities.csv")]), header=T, sep=",", quote="\"")
  bank_bs=bind_rows(list(bank_bs,test1))
  
  test2=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"Past Due and Nonaccrual Assets.csv")]), header=T, sep=",", quote="\"")
  bank_pd=bind_rows(list(bank_pd,test2))
  
  test3=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"Income and Expense.csv")]), header=T, sep=",", quote="\"")
  bank_ie=bind_rows(list(bank_ie,test3))
  
  test4=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"_Net Loans and Leases.csv")]), header=T, sep=",", quote="\"")
  bank_nll=bind_rows(list(bank_nll,test4))
  
  test5=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"Loan Charge-Offs and Recoveries.csv")]), header=T, sep=",", quote="\"")
  bank_co=bind_rows(list(bank_co,test5))
  
  test6=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,"Total Deposits.csv")]), header=T, sep=",", quote="\"")
  bank_dp=bind_rows(list(bank_dp,test6))
}

bank_pd$p3auto <- bank_pd$P3AUTO
bank_pd$p3conoth <- bank_pd$P3CONOTH
bank_pd$p9auto <- bank_pd$P9AUTO
bank_pd$p9conoth <- bank_pd$P9CONOTH
bank_nll$lnauto <- bank_nll$LNAUTO
bank_co$ntauto <- bank_co$NTAUTO
bank_co$ntconoth <- bank_co$NTCONOTH
bank_all = bank_nll %>% 
  select(cert, repdte, lnatres, lnlsgr, lnre, lncrcd, lnconrp, lnauto, lnconoth) %>%
  merge(bank_pd, by=c("cert", "repdte")) %>%
  select(cert, repdte, p3asset, p3re, p3ci, p3crcd, p3auto, p3conoth, p9asset, 
         p9re, p9ci, p9crcd, p9auto, p9conoth, lnatres, lnlsgr, lnre, lncrcd, lnconrp,
         lnauto, lnconoth) %>%
  merge(bank_ie, by=c("cert", "repdte")) %>%
  select(cert, repdte, p3asset, p3re, p3ci, p3crcd, p3auto, p3conoth, p9asset, 
         p9re, p9ci, p9crcd, p9auto, p9conoth, lnlsgr, lnre, lncrcd, lnconrp,
         lnauto, lnconoth, nim) %>%
  merge(bank_bs, by=c("cert", "repdte")) %>%
  mutate(p3_delinquent=p3asset/asset, p9_delinquent=p9asset/asset) %>%
  select(cert, repdte, asset, ernast, asset5, p3asset, p3_delinquent, p3re, p3ci, p3crcd, 
         p3auto, p3conoth, p9asset, p9_delinquent, p9re, p9ci, p9crcd, p9auto, 
         p9conoth, lnatres, lnlsgr, lnre, lncrcd, lnconrp, lnauto, lnconoth, nclnls, 
         nim) %>%
  merge(bank_dp, by=c("cert", "repdte")) %>%
  select(cert, repdte, asset, coredep, ernast, asset5, p3asset, p3_delinquent, p3re, p3ci, p3crcd, 
         p3auto, p3conoth, p9asset, p9_delinquent, p9re, p9ci, p9crcd, p9auto, 
         p9conoth, lnatres, lnlsgr, lnre, lncrcd, lnconrp, lnauto, lnconoth, nclnls, 
         nim) %>%
  merge(bank_co, by=c("cert","repdte")) %>%
  select(cert, name, repdte, asset, coredep, ernast, asset5, p3asset, p3_delinquent, p3re, p3ci, p3crcd, 
         p3auto, p3conoth, p9asset, p9_delinquent, p9re, p9ci, p9crcd, p9auto, 
         p9conoth, lnatres, lnlsgr, lnre, lncrcd, lnconrp, lnauto, lnconoth, 
         ntre, ntcrcd, ntauto, ntconoth, nclnls, nim)

bank_all$p3other <- bank_all$p3asset - (bank_all$p3re + bank_all$p3ci + bank_all$p3crcd + bank_all$p3auto + bank_all$p3conoth)
bank_all$p9other <- bank_all$p9asset - (bank_all$p9re + bank_all$p9ci + bank_all$p9crcd + bank_all$p9auto + bank_all$p9conoth)
bank_all$overdue <- bank_all$p3asset + bank_all$p9asset
bank_all$overdue <- as.numeric(bank_all$overdue)
bank_all$repdte <- as.Date(mdy(bank_all$repdte))

bank_all <- bank_all %>%
  mutate(latecrcd=(p3crcd+p9crcd)/lncrcd,lateauto=(p3auto+p9auto)/lnauto, latere=(p3re+p9re)/lnre, p3re=p3re/asset, p3ci=p3ci/asset, p3crcd=p3crcd/asset, p3auto=p3auto/asset, p3conoth=p3conoth/asset, p3other=p3other/asset, p9re=p9re/asset, p9ci=p9ci/asset, p9crcd=p9crcd/asset, p9auto=p9auto/asset, p9conoth=p9conoth/asset, p9other=p9other/asset)

bank_all$repdte <- as.Date(ymd(bank_all$repdte))
bank_all$cert <- as.character(bank_all$cert)
bank_all <- bank_all %>%
  mutate(lnatres2 = lnatres/lnlsgr, ntre2 = ntre/lnre, ntcrcd2 = ntcrcd/lncrcd, ntauto2 = ntauto/lnauto, ntconoth2 = ntconoth/lnconoth) %>%
  arrange(repdte) %>%
  group_by(cert) %>%
  mutate(lnre3 = rollmean(lnre, k=4, na.pad=TRUE), ntre3 = rollmean(ntre, k=4, na.pad=TRUE), lnauto3 = rollmean(lnauto, k=4, na.pad=TRUE), ntauto3 = rollmean(ntauto, k=4, na.pad=TRUE), lncrcd3 = rollmean(lncrcd, k=4, na.pad=TRUE), ntcrcd3 = rollmean(ntcrcd, k=4, na.pad=TRUE), lnconoth3 = rollmean(lnconoth, k=4, na.pad=TRUE), ntconoth3 = rollmean(ntconoth, k=4, na.pad=TRUE)) %>%
  mutate(lnre3= lag(lnre3, 2), ntre3 = lag(ntre3, 2), lnauto3 = lag(lnauto3, 2), ntauto3 = lag(ntauto3, 2), lncrcd3 = lag(lncrcd3, 2), ntcrcd3 = lag(ntcrcd3, 2), lnconoth3 = lag(lnconoth3, 2), ntconoth3 = lag(ntconoth3, 2))
bank_all <- bank_all %>%
  mutate(avg_ntre = ntre3/lnre3, avg_ntauto = ntauto3/lnauto3, avg_ntcrcd = ntcrcd3/lncrcd3, avg_ntconoth = ntconoth3/lnconoth3)

bank_all2 <- unique(bank_all)

## CU Data Below ######################################################33
call_report_url2="https://www.ncua.gov/analysis/credit-union-corporate-call-report-data/quarterly-data"
url_prefix2 = "https://www.ncua.gov"
search_string2="//a[contains(@href,'zip') and contains(@href,'Select') or contains(@href, '2007') or contains(@href,'2008') or contains(@href,'2009') or contains(@href,'201') or contains(@href,'202')]"
download_dir2="//hqwinfs1/ones2/Call Reports - Credit Unions/Zip Files"

# get all links on page
page_links2=read_html(call_report_url2) %>% 
  html_nodes("body") %>% 
  xml_find_all(search_string2) %>%
  html_attr("href")

# get filenames at source
source_filenames2 = page_links2 %>% as.data.frame() %>% 
  rename(URL=1) %>% mutate(URL=as.character(URL)) %>%
  mutate(Filename=gsub("^.*/", "", URL))
source_filenames2 <- source_filenames2 %>%
  filter(row(source_filenames2) > 6)
len = as.numeric(length(source_filenames2$Filename))
source_filenames2 <- source_filenames2 %>%
  filter(row(source_filenames2) != len)

# get all files already downloaded
dest_filenames2=list.files(download_dir2, full.names = FALSE, pattern=".*.zip", recursive = FALSE) %>% 
  as.data.frame() %>% rename(Filename=1) %>% mutate(AlreadyDownloaded=TRUE)

# get list of files that have not already been downloaded
files_to_download2 = source_filenames2 %>% left_join(dest_filenames2) %>% filter(is.na(AlreadyDownloaded))
for (filename in (files_to_download2  %>% select(URL) %>% pull())) {
  url=paste0(url_prefix2,filename)
  download.file(url,paste0(download_dir2,"/",tail(str_split(url,"/")[[1]],1)), mode="wb")
}
filenames = source_filenames2 %>% 
  mutate(Filename=paste(download_dir2,Filename,sep="/")) %>% 
  select(Filename) %>% pull()
fs220 = "(F|f)(S|s)220.txt"
fs220a = "(F|f)(S|s)220(A|a).txt"
fs220b = "(F|f)(S|s)220(B|b).txt"
fs220i = "(F|f)(S|s)220(I|i).txt"
cu_220 = data.frame()
cu_220a = data.frame()
cu_220b = data.frame()
cu_220i = data.frame()
for (filename in filenames) {
  contained_files = unzip(filename,list=TRUE)
  
  test7=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,fs220)]), header=T, sep=",", quote="\"")
  cu_220=bind_rows(list(cu_220,test7))
  
  test8=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,fs220a)]), header=T, sep=",", quote="\"")
  cu_220a=bind_rows(list(cu_220a,test8))
  
  test9=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,fs220b)]), header=T, sep=",", quote="\"")
  cu_220b=bind_rows(list(cu_220b,test9))
  
  test10=read.table(unz(filename,contained_files$Name[str_detect(contained_files$Name,fs220i)]), header=T, sep=",", quote="\"")
  cu_220i=bind_rows(list(cu_220i,test10))
}
colnames(cu_220) = toupper(colnames(cu_220))
colnames(cu_220a) = toupper(colnames(cu_220a))
colnames(cu_220b) = toupper(colnames(cu_220b))
colnames(cu_220i) = toupper(colnames(cu_220i))
cu_all <- cu_220 %>%
  select(CU_NUMBER, CYCLE_DATE, ACCT_010, ACCT_018, ACCT_025B, ACCT_550, ACCT_703, ACCT_719, ACCT_751, ACCT_755, ACCT_771, ACCT_775) %>%
  merge(cu_220a, by=c("CU_NUMBER", "CYCLE_DATE")) %>%
  select(CU_NUMBER, CYCLE_DATE, ACCT_010, ACCT_018, ACCT_025B, ACCT_116, 
         ACCT_370, ACCT_385, ACCT_396, ACCT_548, ACCT_549,
         ACCT_550, ACCT_703, ACCT_719, ACCT_751, ACCT_755, ACCT_771, ACCT_775) %>%
  merge(cu_220b, by=c("CU_NUMBER", "CYCLE_DATE")) %>%
  select(CU_NUMBER, CYCLE_DATE, ACCT_010, ACCT_018, ACCT_025B, ACCT_045B, ACCT_116, 
         ACCT_370, ACCT_385, ACCT_396, ACCT_548, ACCT_549,
         ACCT_550, ACCT_607, ACCT_608, ACCT_680, ACCT_681, ACCT_703, ACCT_719, ACCT_751, ACCT_755, ACCT_771, ACCT_775,
         ACCT_752, ACCT_753, ACCT_754, ACCT_756, ACCT_757, ACCT_758, ACCT_772, ACCT_773, ACCT_774, ACCT_776, ACCT_777, ACCT_778) %>%
  merge(cu_220i, by=c("CU_NUMBER", "CYCLE_DATE")) %>%
  select(CU_NUMBER, CYCLE_DATE, ACCT_010, ACCT_018, ACCT_025B, ACCT_045B, ACCT_116, 
         ACCT_370, ACCT_385, ACCT_396, ACCT_548, ACCT_549,
         ACCT_550, ACCT_607, ACCT_608, ACCT_680, ACCT_681, ACCT_703, ACCT_719, ACCT_751, ACCT_755, ACCT_771, ACCT_775,
         ACCT_752, ACCT_753, ACCT_754, ACCT_756, ACCT_757, ACCT_758, ACCT_772, ACCT_773, ACCT_774, ACCT_776, ACCT_777, ACCT_778,
         ACCT_550C1, ACCT_550C2, ACCT_551C1, ACCT_551C2, ACCT_020C1, ACCT_020C2, ACCT_021C1, ACCT_021C2, ACCT_022C1, ACCT_022C2, ACCT_023C1, ACCT_023C2)

cu_all2 <- cu_all %>%
  mutate(asset = ACCT_010, dep = ACCT_018, ernast = ACCT_025B, nim = ACCT_116,
         re_nco = ACCT_548+ACCT_549-(ACCT_607+ACCT_608), auto_nco = ACCT_550C1+ACCT_550C2-(ACCT_551C1+ACCT_551C2),
         crcd_nco = ACCT_680-ACCT_681, late_re = ACCT_751+ACCT_755+ACCT_771+ACCT_775+ACCT_752+ACCT_753+ACCT_754+ACCT_756+ACCT_757+ACCT_758+ACCT_772+ACCT_773+ACCT_774+ACCT_776+ACCT_777+ACCT_778,
         late_auto = ACCT_020C1+ACCT_020C2+ACCT_021C1+ACCT_021C2+ACCT_022C1+ACCT_022C2+ACCT_023C1+ACCT_023C2,
         late_crcd = ACCT_045B, total_re = ACCT_703, total_auto = ACCT_370 + ACCT_385, total_crcd = ACCT_396,
         lnatres=ACCT_719, lnatres2 = lnatres/ernast) %>%
  select(CU_NUMBER, CYCLE_DATE, asset, ernast, dep, nim, re_nco, auto_nco, crcd_nco, late_re, late_auto, late_crcd, total_re, total_auto, total_crcd, lnatres, lnatres2) %>%
  mutate(CYCLE_DATE = substr(CYCLE_DATE, 1, 10)) %>%
  mutate(CYCLE_DATE = str_trim(CYCLE_DATE)) %>%
  mutate(CYCLE_DATE = as.Date(mdy(CYCLE_DATE))) %>%
  arrange(CYCLE_DATE) %>%
  group_by(CU_NUMBER) %>%
  mutate(rollre = rollmean(total_re, k=4, na.pad=TRUE), rollre_co = rollmean(re_nco, k=4, na.pad=TRUE), 
         rollauto = rollmean(total_auto, k=4, na.pad=TRUE), rollauto_co = rollmean(auto_nco, k=4, na.pad=TRUE), 
         rollcrcd = rollmean(total_crcd, k=4, na.pad=TRUE), rollcrcd_co = rollmean(crcd_nco, k=4, na.pad=TRUE)) %>%
  mutate(rollre = lag(rollre, n=2), rollre_co = lag(rollre_co, n=2), rollauto = lag(rollauto, n=2),
         rollauto_co = lag(rollauto_co, n=2), rollcrcd =lag(rollcrcd, n=2), rollcrcd_co = lag(rollcrcd_co, n=2))

write.csv(bank_all2,paste(download_dir,"bank_all.csv",sep="/"),row.names = FALSE)
write.csv(cu_all2,paste(download_dir2,"cu_all.csv",sep="/"),row.names = FALSE)

#save(bank_all2, cu_all, file="temp.rda")
#rm(list=ls()) # remove all objects

#load("temp.rda")
#ls()