#三次産業活動指数をロング型データで取得するためのスクリプト
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# 原指数・季節調整指数ごとにファイルパス・ファイル名設定
filePath01<- "https://www.meti.go.jp/statistics/tyo/sanzi/result/excel/b2015_komj.xlsx"
fileName01<- "b2015_komj.xlsx"

filePath02<- "https://www.meti.go.jp/statistics/tyo/sanzi/result/excel/b2015_ksmj.xlsx"
fileName02<- "b2015_ksmj.xlsx"

# 双方のファイルをダウンロード
curl_download(filePath01,fileName01)
curl_download(filePath02,fileName02)

# ループで各々を処理
for(i in 1:2){
  if(i == 1){
    fileName<- fileName01
  }else{
    fileName<- fileName02
  }
  # ファイルの読み込み
  tempdf<- read_excel(fileName,
                      sheet=2,
                      skip=2) %>% as.data.frame()
  tempdf<- tempdf[1:261,]
  
  # 必要な行に絞り込み
  tempdf<- tempdf[tempdf$品目名称 %in% c("第３次産業総合",
                                     "電気・ガス・熱供給・水道業",
                                     "情報通信業",
                                     "運輸業，郵便業",
                                     "卸売業",
                                     "金融業，保険業",
                                     "物品賃貸業（自動車賃貸業を含む）",
                                     "事業者向け関連サービス",
                                     "小売業",
                                     "不動産業",
                                     "医療，福祉",
                                     "生活娯楽関連サービス"),
                  2:ncol(tempdf)]  
  
  # 原指数・季節調整指数のフラグ
  if(i == 1){
    tempdf$div01<- "原指数"
    df<- tempdf
  }else{
    tempdf$div01<- "季節調整指数"
    df<- rbind(df,tempdf)
  }
}
# ウエイトを便宜上、1000年1月に
df<- df[,c("div01",
           colnames(df)[1:(ncol(df)-1)])]
colnames(df)<- str_replace(colnames(df),"ウエイト","100001")
colnames(df)<- str_replace(colnames(df),"品目名称","item")

# ロング型に展開
df2<- gather(df,
             key=period,
             value=value,
             -div01,
             -item)
df2$period<- ymd(paste0(df2$period,"01"))
df2$div02<- NA

# 産業分類をまとめる
df2[df2$item == "第３次産業総合",]$div02<- 　　　　　　　　　"合計"
df2[df2$item == "電気・ガス・熱供給・水道業",]$div02<- 　　　"電ガ水・運輸郵便"
df2[df2$item == "情報通信業",]$div02<- 　　　　　　　　　　　"情報通信"
df2[df2$item == "運輸業，郵便業",]$div02<- 　　　　　　　　　"電ガ水・運輸郵便"
df2[df2$item == "卸売業",]$div02<- 　　　　　　　　　　　　　"卸・小売業"
df2[df2$item == "金融業，保険業",]$div02<- 　　　　　　　　　"金融・保険・不動産"
df2[df2$item == "物品賃貸業（自動車賃貸業を含む）",]$div02<- "金融・保険・不動産"
df2[df2$item == "事業者向け関連サービス",]$div02<- 　　　　　"対事業所サービス"
df2[df2$item == "小売業",]$div02<- 　　　　　　　　　　　　　"卸・小売業"
df2[df2$item == "不動産業",]$div02<- 　　　　　　　　　　　　"金融・保険・不動産"
df2[df2$item == "医療，福祉",]$div02<- 　　　　　　　　　　　"医療福祉"
df2[df2$item == "生活娯楽関連サービス",]$div02<- 　　　　　　"生活関連サービス"

# 面倒な区分名を修正
df2$item<- str_replace_all(df2$item,"，","・")
df2$item<- str_replace_all(df2$item,"第３次産業総合","合計")
df2$item<- str_replace_all(df2$item,"電気・ガス・熱供給・水道業","電気・ガス・水道")
df2$item<- str_replace_all(df2$item,"物品賃貸業（自動車賃貸業を含む）","物品賃貸業")

df2<- df2[,c("div01",
             "div02",
             "item",
             "period",
             "value")]

df2
