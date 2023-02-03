#職業安定業務統計をロング型データで取得するためのスクリプト
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# ファイルパスとファイル名の設定
#geturl<- read_html("https://www.mhlw.go.jp/toukei/list/114-1.html") %>%
#  html_nodes("a")
#geturl<- geturl[which(str_detect(geturl,"統計表一覧")==T)] %>%
#  html_attr("href")

geturl2<- read_html("https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00450222&tstat=000001020327") %>%
  html_nodes("a")
geturl2<- geturl2[which(str_detect(
  as.character(geturl2),"月次")==T)[1]] %>%
  html_attr("href")
geturl2<- paste0("https://www.e-stat.go.jp",geturl2)

thtml<- read_html(geturl2) %>%
  html_nodes("a")
url<- thtml[which(str_detect(
  as.character(thtml),
  "労働市場関係指標（求人倍率・就職率・充足率・求人数・求職者数・就職件数）")) + 1] %>%
  html_attr("href")
url<- paste0("https://www.e-stat.go.jp",url)
fileName<- "第1表.xlsx"

# ファイルのダウンロード・読み込み
curl_download(url,fileName)
temp<- read_excel(fileName,
                  skip=1,
                  col_names=F) %>% as.data.frame()

# 列名の取得・生成
tempColname1<- temp[1,]
tempColname2<- temp[2,]
df<- temp[4:nrow(temp),]

colnames(df)<- paste0(tempColname1,
                      "_",
                      tempColname2)
colnames(df)<- str_replace_all(colnames(df),"実数","原数値")

# 月次データの行だけを抽出
df<- df[df[,3] %in% c("１月",
                      "２月",
                      "３月",
                      "４月",
                      "５月",
                      "６月",
                      "７月",
                      "８月",
                      "９月",
                      "10月",
                      "11月",
                      "12月"),]

# 日付データの生成（全角ではエラーになるため半角に）
df$period<- paste0(df[,1],df[,3],"1日") %>%
  str_replace_all("１","1") %>%
  str_replace_all("２","2") %>%
  str_replace_all("３","3") %>%
  str_replace_all("４","4") %>%
  str_replace_all("５","5") %>%
  str_replace_all("６","6") %>%
  str_replace_all("７","7") %>%
  str_replace_all("８","8") %>%
  str_replace_all("９","9") %>%
  ymd()

# 必要な列に絞り込み
df2<- df[,c("period",
            "原数値_有効求人倍率",
            "季節調整値_有効求人倍率",
            "原数値_新規求人倍率",
            "季節調整値_新規求人倍率",
            "原数値_就職率（対新規）",
            "季節調整値_就職率（対新規）",
            "原数値_充足率（対新規）",
            "季節調整値_充足率（対新規）",
            "原数値_新規求人数",
            "季節調整値_新規求人数",
            "原数値_新規求職申込件数",
            "季節調整値_新規求職申込件数",
            "原数値_有効求人数",
            "季節調整値_有効求人数",
            "原数値_有効求職者数",
            "季節調整値_有効求職者数",
            "原数値_就職件数",
            "季節調整値_就職件数")]

# ロング型に展開
df3<- gather(df2,
             key=item,
             value=value,
             -period)

# 原数値と季節調整値のフラグ
df3$div<- ifelse(str_detect(df3$item,"原数値")==T,"原数値","季節調整値")
df3$item<- str_remove_all(df3$item,"原数値_") %>%
  str_remove_all("季節調整値_")
df3$value<- as.numeric(df3$value)
df3
