library(dplyr)
library(tidyr)
library(rvest)
library(stringr)

# 掲載サイトのURLを取得（更新ごとにファイルパスやファイル名が変わるため）
gdp_h<- read_html("https://www.esri.cao.go.jp/jp/sna/sokuhou/sokuhou_top.html") %>%
  html_nodes("a")

gdp_h2<- gdp_h[which(str_detect(gdp_h,"最新")==T)]
gdp_url<- paste0("https://www.esri.cao.go.jp/",html_attr(gdp_h2,"href"))

# 掲載ページから必要なファイルの名称及びパスを取得
gdp_late_h<- read_html(gdp_url) %>% html_nodes("a")
searchText<- c("名目原系列（CSV形式：",
               "名目季節調整系列（CSV形式：",
               "実質原系列（CSV形式：",
               "実質季節調整系列（CSV形式：",
               "四半期・実額・増加率（CSV形式：")

# 読み込みファイル分のパスを生成
for(i in 1:length(searchText)){
  if(i == 1){
    gdp_late_h2<- gdp_late_h[which(str_detect(gdp_late_h,searchText[i]))]
  }else{
    temp<- gdp_late_h[which(str_detect(gdp_late_h,searchText[i]))]
    gdp_late_h2<- c(gdp_late_h2,temp)
  }
}
gdp_list<- list()
list_name<- vector()

# 読み込みファイルごとのループ
for(i in 1:length(gdp_late_h2)){
  
  # ファイル名を取得
  fileN<- html_attr(gdp_late_h2[[i]],"href")
  
  # csvを読み込み
  temp<- read.csv(paste0(str_remove(gdp_url,"gdemenuja.html"),
                         fileN),
                  header=F)
  
  # データの1行目を抽出し、データ部分を分割
  headLine<- which(str_detect(temp[,1],"1994/ 1- 3.")) - 1
  
  tempBody<- temp[(headLine+1):nrow(temp),]
  tempBody<- tempBody[tempBody[,2]!="",]
  
  # 項目名部分を分割
  tempHead<- temp[1:headLine,]
  header<- ncol(temp)
  
  # 主要系列表の場合
  if(str_detect(fileN,"kgaku")==F & str_detect(fileN,"kshotoku")==F){
    # 項目名を抽出
    for(j in 1:ncol(temp)){
      for(k in 1:3){
        if(tempHead[k,j] != "" &
           !is.na(tempHead[k,j]) &
           k != 3){
          header[j]<- tempHead[k,j]
          break()
        }else if(k == 3){
          header[j]<- tempHead[k,j]
        }
      }
    }
    
    # 項目名の一部をリライト
    header<- str_replace(header,"財貨・サービス","純輸出")
    header<- str_replace(header,"<参考>","要素所得純受取")
    colnames(tempBody)<- header
    
    # 日付生成
    tempBody$date<- seq(as.Date("1994-01-01"),by="3 month",length.out=nrow(tempBody))
    
    # 必要な列に絞ってリストに保管
    gdp_list[[i]]<- tempBody[,c("date",
                                "国内総生産(支出側)",
                                "民間最終消費支出",
                                "民間住宅",
                                "民間企業設備",
                                "民間在庫変動",
                                "政府最終消費支出",
                                "公的固定資本形成",
                                "公的在庫変動",
                                "純輸出",
                                "要素所得純受取")]
    
    # リスト名に系列名を設定
    list_name[i]<- paste0("GDP_",tempHead[1,1])
  
  # 消費支出内訳の場合
  }else if(str_detect(fileN,"kgaku")==T){
    # 項目名を抽出
    for(j in 1:ncol(temp)){
      for(k in 1:5){
        if(tempHead[k,j] != "" &
           !is.na(tempHead[k,j]) &
           k != 5){
          header[j]<- tempHead[k,j]
          break()
        }else if(k == 5){
          header[j]<- tempHead[k,j]
        }
      }
    }
    colnames(tempBody)<- header
    
    # 日付を生成
    tempBody$date<- seq(as.Date("1994-01-01"),by="3 month",length.out=nrow(tempBody))
    
    # 必要な列に絞ってリストに保管
    gdp_list[[i]]<- tempBody[,c("date",
                                "国内家計最終消費支出",
                                "耐久財",
                                "半耐久財",
                                "非耐久財",
                                "サービス")]
    
    # リスト名に系列名を設定
    list_name[i]<- paste0("家計消費_",tempHead[1,1])
  
  # 雇用者報酬の場合  
  }else{
    tempBody<- tempBody[,c(2,4,6,8)]
    
    colnames(tempBody)<- c("名目原系列",
                           "名目季節調整系列",
                           "実質原系列",
                           "実質季節調整系列")
    tempBody$date<- seq(as.Date("1994-01-01"),by="3 month",length.out=nrow(tempBody))
    tempBody<- tempBody[,c("date",
                           "名目原系列",
                           "名目季節調整系列",
                           "実質原系列",
                           "実質季節調整系列")]
    
    gdp_list[[i]]<- tempBody
    
    list_name[i]<- "雇用者報酬"
  }
}
names(gdp_list)<- list_name

# リストからの取り出し
for(i in 1:length(gdp_list)){
  # ロング型に変換
  temp<- gather(gdp_list[[i]],
                key=item,
                value=value,
                -date)
  
  # リスト名で判別し、各区分に設定
  divName<- names(gdp_list[i])
  
  # 雇用者報酬かどうか
  if(divName != "雇用者報酬"){
    # GDPか、家計消費内訳か
    if(str_detect(divName,"GDP")==T){
      temp$div01<- "GDP"
    }else{
      temp$div01<- "家計消費"
    }
    
    # 名目か、実質か
    if(str_detect(divName,"名目")==T){
      temp$div02<- "名目"
    }else{
      temp$div02<- "実質"
    }
    
    # 原系列か、季節調整系列か
    if(str_detect(divName,"原系列")==T){
      temp$div03<- "原系列"
    }else{
      temp$div03<- "季節調整系列"
    }
  
  # 雇用者報酬の場合
  }else{
    temp$div01<- "雇用者報酬"
    temp$div02<- ifelse(str_detect(temp$item,"名目")==T,"名目","実質")
    temp$div03<- ifelse(str_detect(temp$item,"原系列")==T,"原系列","季節調整系列")
    temp$item<- "雇用者報酬"
  }
  
  # 所得収支について別区分に
  temp$div01<- ifelse(temp$item == "要素所得純受取","GDP参考",temp$div01)
  
  # データを統合
  if(i == 1){
    df<- temp
  }else{
    df<- rbind(df,temp)
  }
}
df<- df[,c("div01","div02","div03","item","date","value")]
df
