library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)

# ファイル数・シート数が多いため、ループ処理
motherUrl<- "https://www.meti.go.jp/statistics/tyo/iip/xls/"
fileName<- c("b2015_gom1j.xlsx",
             "b2015_gsm1j.xlsx",
             "b2015_zom1j.xlsx",
             "b2015_zsm1j.xlsx")
sheetName<- c("生産","出荷","在庫")

# ファイルごとのループ
for(i in 1:length(fileName)){
  # ファイルダウンロード
  downUrl<- paste0(motherUrl,fileName[i])
  curl::curl_download(downUrl,fileName[i])
  
  # シートごとのループ
  for(j in 1:length(sheetName)){
    # 対象シートの読み込み
    temp<- read_excel(fileName[i],
                      sheet=sheetName[j],
                      skip=2)
    
    # ウエイト行を便宜上、1000年1月のデータに
    colnames(temp)<- ifelse(str_detect(colnames(temp),".ウエイト")==T,"100001",colnames(temp))
    
    # 業種別の場合
    if(fileName[i] == "b2015_gom1j.xlsx" |
       fileName[i] == "b2015_gsm1j.xlsx"){
      temp$div01<- "業種別"
      temp<- temp[1:146,]
      
      # 必要な行に絞り込む
      temp<- temp[temp$品目名称 %in% c("鉱工業",
                                   "製造工業",
                                   "鉄鋼・非鉄金属工業",
                                   "金属製品工業",
                                   "生産用機械工業",
                                   "汎用・業務用機械工業",
                                   "電子部品・デバイス工業",
                                   "電気・情報通信機械工業",
                                   "輸送機械工業",
                                   "窯業・土石製品工業",
                                   "化学工業",
                                   "石油・石炭製品工業",
                                   "プラスチック製品工業",
                                   "パルプ・紙・紙加工品工業",
                                   "食料品・たばこ工業",
                                   "その他工業",
                                   "鉱業"),] %>% as.data.frame()
    # 財別の場合
    }else{
      temp$div01<- "財別"
      temp<- temp[1:67,]
      
      # 必要な行に絞り込む
      temp<- temp[temp$品目名称 %in% c("鉱工業",
                                   "投資財",
                                   "耐久消費財",
                                   "非耐久消費財",
                                   "生産財"),] %>% as.data.frame()
    }
    # ファイル名で原指数・季節調整指数を判別してデータを付加
    if(fileName[i] == "b2015_gom1j.xlsx" |
       fileName[i] == "b2015_zom1j.xlsx"){
      temp$div02<- "原系列"
    }else{
      temp$div02<- "季節調整系列"
    }
    
    # シート名から生産・出荷・在庫を付加
    temp$div03<- sheetName[j]
    
    # データを統合
    if(i == 1 & j == 1){
      df<- temp
    }else{
      df<- rbind(df,temp)
    }
  }
}
colnames(df)<- str_replace(colnames(df),"品目名称","item")
df2<- df[,c("div01",
            "div02",
            "div03",
            "item",
            colnames(df[,3:(ncol(df)-3)]))]

# ロング型に展開
df3<- gather(df2,
             key=period,
             value=value,
             -div01,-div02,-div03,-item)
df3