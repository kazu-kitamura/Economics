#IMFのWorld Economic Outlookをロング型データで取得するためのスクリプト
# ライブラリの読み込み
library(dplyr) # 行列操作
library(tidyr) # 行列操作
library(rvest) # スクレイピング
library(stringr) # 文字列操作
library(curl)
library(lubridate)

# ターゲットURLの設定
targetCont<- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOApr2021all.ashx"
targetGoup<- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOApr2021alla.ashx"

# ファイルの一時ダウンロード
curl_download(targetCont,"temp.csv")
curl_download(targetGoup,"temp2.csv")

# タブ区切りファイルとして読み込み
df1<- read.delim("temp.csv")[,c(-1,-6,-9)]
df1<-df1[,1:(ncol(df1)-1)]

df2<- read.delim("temp2.csv")[,c(-5,-9)]
colnames(df2)<- colnames(df1)
df2$ISO<- NA

# 国別・グループ別のフラグ
df1$Div<- "CountryBase"
df2$Div<- "GroupBase"

# 双方のテーブルの結合と列の並べ替え
df<- rbind(df1,df2)
df<- df[,c(ncol(df),1:(ncol(df)-1))]

# 日付文字列の作成、列名に
period<- seq(as.Date("1980-01-01"),
             by="1 year",
             length.out=ncol(df)-7) %>%
  strftime(today, format="%Y%m%d")

colnames(df)<- c(colnames(df)[1:7],
                 period)

# ロング型に変換
dfout<- gather(df,
               key=period,
               value=value,
               -Div,
               -ISO,
               -WEO.Subject.Code,
               -Country,
               -Subject.Descriptor,
               -Units,
               -Scale)

# 数値化
dfout2<- dfout
dfout2$value<- str_remove_all(dfout2$value,",")
dfout2$value<- as.numeric(dfout2$value)
dfout2$period<- ymd(dfout2$period)

dfout2<- dfout2[!is.na(dfout2$value),]
dfout2
