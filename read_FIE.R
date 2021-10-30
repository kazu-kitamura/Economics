library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# 勤労者世帯・二人以上世帯のファイルパス及びファイル名
filePath_w<- "https://www.stat.go.jp/data/kakei/longtime/zuhyou/kin-ts.xlsx"
filePath_a<- "https://www.stat.go.jp/data/kakei/longtime/zuhyou/zen-ts.xlsx"

fileName_w<- "kin-ts.xlsx"
fileName_a<- "zen-ts.xlsx"

# ファイル読み込みのための関数を作成
read_FIE<- function(flagName,
                    filePath,
                    fileName,
                    sheetName,
                    startLine){
  
  # ファイルのダウンロード及び読み込み
  curl_download(filePath,fileName)
  
  temp<- read_excel(fileName,
                    sheet=sheetName,
                    skip=startLine,
                    col_names=F) %>% as.data.frame()
  temp<- temp[1:(nrow(temp)-4),]
  
  # 項目名部分とデータ部分とに分割
  temp_rowName<- temp[,9:16]
  temp_realValue<- temp[,17:ncol(temp)]
  
  # 項目名の入れ物
  mat_rowName<- rep(NA,nrow(temp_rowName)) %>% as.vector()
  
  # 項目名整理のためのループ（階層がわかるようアンダーバーを付加）
  for(i in 1:(ncol(temp_rowName)-1)){
    for(j in 1:nrow(temp_rowName)){
      if(!is.na(temp_rowName[j,i])){
        fugo<- NULL
        if(i > 1){
          for(k in 1:(i-1)){
            fugo<- paste0(fugo,"_")
          }
        }
        mat_rowName[j]<- paste0(fugo,temp_rowName[j,i])
      }
    }
  }
  
  # 年月を列名に
  mat_colName<- seq(as.Date("2000-01-01"),by="1 month",length.out=ncol(temp_realValue))
  colnames(temp_realValue)<- mat_colName
  
  df1<- cbind(mat_rowName,temp_realValue) %>% as.data.frame()
  colnames(df1)<- c("item",mat_colName)
  
  # ロング型に展開
  df2<- gather(df1,
               key=period,
               value=value,
               -item)
  df2$period<- as.Date(as.numeric(df2$period),origin="1970-01-01")
  df2$value<- as.numeric(df2$value)
  df2$flag<-flagName 
  df2<- df2[,c("flag",
               "item",
               "period",
               "value")]
  df2
}

# 勤労者世帯分に読み込み関数を適用
df_kinro<- read_FIE("勤労者世帯",
                    filePath_w,
                    fileName_w,
                    "支出金額（月）",
                    28)

# 二人以上世帯分に読み込み関数を適用
df_all<- read_FIE("二人以上世帯",
                  filePath_a,
                  fileName_a,
                  "支出金額（月）",
                  28)

# データを統合・出力
df<- rbind(df_kinro,df_all) %>% as.data.frame()
df<- df[!is.na(df$item),]
df
