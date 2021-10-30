library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# 消費者物価指数
CPtarget<- "https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031431696&fileKind=1"

temp<- curl_download(CPtarget,"temp.csv")

temp<- read.csv("temp.csv")
CP<- temp[6:nrow(temp),c(1,2,ncol(temp))]
colnames(CP)<- c("period","消費者物価総合","消費者物価_生鮮食品・エネルギー除く")
CP$period<- paste0(CP$period,"01") %>% ymd()

dfCP<- gather(CP,
              key=item,
              value=value,
              -period)

# 企業向けサービス価格指数
WPS<- read_html("https://www.stat-search.boj.or.jp/ssi/mtshtml/pr02_m_1.html") %>%
  html_nodes("table") %>%
  html_table()
WPS<- WPS[[1]][8:nrow(WPS[[1]]),c(1,4)]
colnames(WPS)<- c("period","企業向けサービス価格総平均")
WPS$period<- paste0(WPS$period,"/1") %>% ymd()

dfWPS<- gather(WPS,
               key=item,
               value=value,
               -period)

# 企業向け物価指数
WPP<- read_html("https://www.stat-search.boj.or.jp/ssi/mtshtml/pr01_m_1.html") %>%
  html_nodes("table") %>%
  html_table()
WPP<- WPP[[1]][8:nrow(WPP[[1]]),c(1,6)]
colnames(WPP)<- c("period","企業向け物価指数総平均")
WPP$period<- paste0(WPP$period,"/1") %>% ymd()

dfWPP<- gather(WPP,
               key=item,
               value=value,
               -period)

#　貸出約定平均金利
RT<- read_html("https://www.stat-search.boj.or.jp/ssi/mtshtml/ir04_m_1.html") %>%
  html_nodes("table") %>%
  html_table()
RT<- RT[[1]][8:nrow(RT[[1]]),c(1,2,3)]
colnames(RT)<- c("period","貸出約定平均金利_短期","貸出約定平均金利_長期")
RT$period<- paste0(RT$period,"/1") %>% ymd()

dfRT<- gather(RT,
              key=item,
              value=value,
              -period)

# 実効為替レート
EX<- read_html("https://www.stat-search.boj.or.jp/ssi/mtshtml/fm09_m_1.html") %>%
  html_nodes("table") %>%
  html_table()
EX<- EX[[1]][8:nrow(EX[[1]]),c(1,2,3)]
colnames(EX)<- c("period","実効為替レート_名目","実効為替レート_実質")
EX$period<- paste0(EX$period,"/1") %>% ymd()

dfEX<- gather(EX,
              key=item,
              value=value,
              -period)

# マネーサプライ
MS<- read_html("https://www.stat-search.boj.or.jp/ssi/mtshtml/md02_m_1.html") %>%
  html_nodes("table") %>%
  html_table()
MS<- MS[[1]][8:nrow(MS[[1]]),c(1,4,3,2)]
colnames(MS)<- c("period","M1_前年比","M2_前年比","M3_前年比")
MS$period<- paste0(MS$period,"/1") %>% ymd()

dfMS<- gather(MS,
              key=item,
              value=value,
              -period)

# データ統合
df<- rbind(dfCP,dfWPP,dfWPS,dfRT,dfEX,dfMS)
df
