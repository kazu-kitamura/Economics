#商業動態統計をロング型データで取得するためのスクリプト
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# ファイルが業態別に複数でパス及びファイル名が変わる可能性があるため、パスから取得
fileUrl<- read_html("https://www.meti.go.jp/statistics/tyo/syoudou/result-2/index.html") %>%
  html_nodes("a") %>%
  html_attr("href")

fileName<- fileUrl[which(str_detect(fileUrl,"(31j|41j|42j|43j|44j)")==T)]

# 業態別のループ
for(i in 1:length(fileName)){
  # ファイルパスの生成とダウンロード
  tempfileName<- paste0("https://www.meti.go.jp/statistics/tyo/syoudou/result-2/",
                        fileName[i])
  curl_download(tempfileName,str_remove(fileName[i],"excel/"))
  
  # 百貨店又はスーパーの場合
  if(str_detect(fileName[i],"31j")==T){
    for(j in 1:2){
      if(j == 1){
        sheetName<- "百貨店　販売額　月次"
        div01<- "百貨店"
      }else{
        sheetName<- "スーパー　販売額　月次"
        div01<- "スーパー"
      }
      
      # ファイルの読み込み
      temp<- read_excel(str_remove(fileName[i],"excel/"),
                        sheet=sheetName,
                        skip=8)
      temp<- temp[2:nrow(temp),2:(ncol(temp)-2)] %>% as.data.frame()
      
      # 必要な列に絞り込み
      temp<- temp[,c("...2",
                     "合計",
                     "紳士服・洋品",
                     "婦人・子供服・洋品",
                     "その他の衣料品",
                     "身の回り品",
                     "飲食料品",
                     "家具",
                     "家庭用電気機械器具",
                     "家庭用品",
                     "その他の商品",
                     "食堂・喫茶")]
      colnames(temp)<- c("date",colnames(temp)[2:ncol(temp)])
      temp$date<- ymd(paste0(temp$date,"1日"))
      
      # ロング型に展開
      temp2<- gather(temp,
                     key=item,
                     value=value,
                     -date)
      temp2$div01<- div01
      
      # 百貨店とスーパーを統合
      if(j == 1){
        df<- temp2
      }else{
        df<- rbind(df,temp2)
      }
    }
  
  # コンビニの場合  
  }else if(str_detect(fileName[i],"41j")==T){
    # ファイルの読み込み
    temp<- read_excel(str_remove(fileName[i],"excel/"),
                      sheet="販売額(value)月次(Monthly)",
                      skip=6)
    temp<- temp[2:nrow(temp),2:(ncol(temp)-3)] %>% as.data.frame()
    
    # 必要な列に絞り込み
    temp<- temp[,c("...2",
                   "合計",
                   "FF・日配食品",
                   "加工食品",
                   "非食品",
                   "サービス売上高")]
    colnames(temp)<- c("date",colnames(temp)[2:ncol(temp)])
    temp$date<- ymd(paste0(temp$date,"1日"))
    
    # ロング型に展開
    temp2<- gather(temp,
                   key=item,
                   value=value,
                   -date)
    temp2$div01<- "コンビニ"
    
    # 百貨店・スーパーと統合
    df<- rbind(df,temp2)
    
  # 量販店の場合
  }else if(str_detect(fileName[i],"42j")==T){
    # ファイルの読み込み
    temp<- read_excel(str_remove(fileName[i],"excel/"),
                      sheet="販売額(value)月次(Monthly)",
                      skip=5)
    temp<- temp[2:nrow(temp),2:(ncol(temp)-3)] %>% as.data.frame()
    colnames(temp)<- c("date","合計",colnames(temp)[3:ncol(temp)])
    temp$date<- ymd(paste0(temp$date,"1日"))
    
    # ロング型に展開
    temp2<- gather(temp,
                   key=item,
                   value=value,
                   -date)
    temp2$div01<- "家電量販店"
    
    # データを統合
    df<- rbind(df,temp2)
    
  # ドラッグストアの場合
  }else if(str_detect(fileName[i],"43j")==T){
    # ファイルの読み込み
    temp<- read_excel(str_remove(fileName[i],"excel/"),
                      sheet="販売額(value)月次(Monthly)",
                      skip=5)
    temp<- temp[2:nrow(temp),2:(ncol(temp)-3)] %>% as.data.frame()
    colnames(temp)<- c("date","合計",colnames(temp)[3:ncol(temp)])
    temp$date<- ymd(paste0(temp$date,"1日"))
    
    # ロング型に展開
    temp2<- gather(temp,
                   key=item,
                   value=value,
                   -date)
    temp2$div01<- "ドラッグストア"
    
    # データを統合
    df<- rbind(df,temp2)
  
  # ホームセンターの場合
  }else{
    # ファイルを読み込み
    temp<- read_excel(str_remove(fileName[i],"excel/"),
                      sheet="販売額(value)月次(Monthly)",
                      skip=5)
    temp<- temp[2:nrow(temp),2:(ncol(temp)-3)] %>% as.data.frame()
    colnames(temp)<- c("date","合計",colnames(temp)[3:ncol(temp)])
    temp$date<- ymd(paste0(temp$date,"1日"))
    
    # ロング型に展開
    temp2<- gather(temp,
                   key=item,
                   value=value,
                   -date)
    temp2$div01<- "ホームセンター"
    
    # データを統合
    df<- rbind(df,temp2)
  }
}

# 整列・出力
df<- df[,c("div01","item","date","value")]
df
