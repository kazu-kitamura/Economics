library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(readxl)
library(curl)
library(lubridate)

# 指数一覧のサイトから指数のファイルパスを取得
targetSite<- "https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450071&tstat=000001011791&cycle=0&tclass1=000001035519&tclass2=000001144287&tclass3val=0"

idxHtml<- read_html(targetSite) %>%
  html_nodes("a") %>%
  html_attr("href")
targetPath<- idxHtml[str_detect(idxHtml,"file-download")==T & !is.na(idxHtml)]

fileNum<- length(targetPath)

# アルファベット等を削除するための変数
removeaz<- "([a-zA-Z]| |　|\\(|\\))"

# 指数の各ファイルごとのループ
for(i in 1:fileNum){
  # ダウンロード
  tempFile<- curl_download(paste0("https://www.e-stat.go.jp",targetPath[i]),"temp.xls")
  
  # シート名の取得
  sheetNum<- excel_sheets(tempFile)
  
  # シートごとのループ
  for(j in 1:length(sheetNum)){
    # 進捗状況の表示
    print(paste0(i," files & ",j," sheet"))
    
    # シートの読み込み
    tempSheet<- read_excel(tempFile,
                           sheet=sheetNum[j],
                           col_names=F,
                           progress=F) %>% as.data.frame
    
    # 指数名
    tempDiv1<- tempSheet[1,6] %>% str_remove_all(removeaz)
    # 規模区分
    tempDiv2<- tempSheet[2,6] %>% str_remove_all(removeaz)
    # 就業形態区分
    tempDiv3<- tempSheet[3,6] %>% str_remove_all(removeaz)
    # 産業分類
    tempDiv4<- tempSheet[4,6] %>% str_remove_all(removeaz)
    # 原指数・季節調整指数
    tempDiv5<- tempSheet[6,1] %>% str_remove_all(removeaz)
    
    # 指数分の行数（下半分は変化率）
    numRow<- which(is.na(tempSheet[10:nrow(tempSheet),1])==T)[1] - 1
    
    # 季節調整か否かに応じてデータ本体を取得
    if(tempDiv5 != "季節調整済指数"){
      tempData<- tempSheet[10:(10 + numRow - 1),c(1,9:20)]
    }else{
      tempData<- tempSheet[10:(10 + numRow - 1),c(1,6:17)]
    }
    # 列名称
    colnames(tempData)<- c("year",1:12)
    
    # ロング型に展開
    tempData2<- gather(tempData,
                       key=mon,
                       value=value,
                       -year)
    
    # 日付を設定
    tempData2$period<- paste0(tempData2$year,"年",
                              tempData2$mon,"月",
                              "1日") %>% ymd()
    
    # 各シートから取得した区分を列として追加
    tempData2$div01<- tempDiv1
    tempData2$div02<- tempDiv2
    tempData2$div03<- tempDiv3
    tempData2$div04<- tempDiv4
    tempData2$div05<- tempDiv5
    
    # データの整形
    tempData3<- tempData2[,c("div01",
                             "div02",
                             "div03",
                             "div04",
                             "div05",
                             "period",
                             "value")]
    
    # 表記を補正
    tempData3$div02<- ifelse(tempData3$div02 == "５人以上5","５人以上",
                             ifelse(tempData3$div02 == "３０人以上30","３０人以上",tempData3$div02))
    tempData3$div05<- ifelse(tempData3$div05 == "指数","原指数","季節調整指数")
    
    # 数値型にしてNA行を削除
    tempData3$value<- as.numeric(tempData3$value)
    tempData3<- tempData3[!is.na(tempData3$value),]
    
    # データを統合
    if(j == 1 & i == 1){
      df<- tempData3
    }else{
      df<- rbind(df,tempData3)
    }
  }
}
# 表記を補正
df$div03<- str_remove_all(df$div03,"-")

# 指数名の表記を補正
df$div01<- str_remove_all(df$div01,"季節調整済")
df$div01<-
  ifelse(df$div01 == "実質賃金指数きまって支給する給与","実質_きまって支給する給与",
  ifelse(df$div01 == "実質賃金指数現金給与総額","実質_現金給与総額",
  ifelse(df$div01 == "賃金指数きまって支給する給与","きまって支給する給与",
  ifelse(df$div01 == "賃金指数現金給与総額","現金給与総額",
  ifelse(df$div01 == "賃金指数所定内給与","所定内給与",
  ifelse(df$div01 == "労働時間指数総実労働時間","総実労働時間",
  ifelse(df$div01 == "労働時間指数所定内労働時間","所定内労働時間",
  ifelse(df$div01 == "労働時間指数所定外労働時間-","所定外労働時間",
  ifelse(df$div01 == "賃金指数実質賃金指数現金給与総額","実質_現金給与総額",df$div01)))))))))

# 産業分類の表記を補正
df$div04<- 
  ifelse(df$div04 == "鉱業，採石業，砂利採取業","鉱業",
  ifelse(df$div04 == "電気・ガス・熱供給・水道業,,","電気・ガス・水道",
  ifelse(df$div04 == "運輸業，郵便業","運輸・郵便",
  ifelse(df$div04 == "卸売業，小売業","卸小売業",
  ifelse(df$div04 == "金融業，保険業","金融保険",
  ifelse(df$div04 == "不動産業，物品賃貸業","不動産・物品賃貸",
  ifelse(df$div04 == "学術研究，専門・技術サービス業,","学術研究・専門技術",
  ifelse(df$div04 == "宿泊業，飲食サービス業,","飲食・宿泊",
  ifelse(df$div04 == "生活関連サービス業，娯楽業-","生活関連サービス",
  ifelse(df$div04 == "教育，学習支援業,","教育学習",
  ifelse(df$div04 == "医療，福祉,","医療福祉",
  ifelse(df$div04 == "サービス業（他に分類されないもの）,...","その他サービス",
  ifelse(df$div04 == "情報通信業","情報通信",
  ifelse(df$div04 == "複合サービス事業","複合サービス",
         df$div04))))))))))))))


# 実数データを取得
tempW<- read.csv("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031913616&fileKind=1",
                 header=T)

# 基準年のデータのみに絞り込み
tempW<- tempW[tempW$年 == 2015 & tempW$月 == "CY",]

# 産業分類のデータを記号から変換
tempW$産業分類<- str_remove_all(tempW$産業分類,"( |　)")
tempW$産業分類<- ifelse(tempW$産業分類 == "TL","調査産業計",
                 ifelse(tempW$産業分類 == "C","鉱業",
                 ifelse(tempW$産業分類 == "D","建設業",
                 ifelse(tempW$産業分類 == "E","製造業",
                 ifelse(tempW$産業分類 == "F","電気・ガス・水道",
                 ifelse(tempW$産業分類 == "G","情報通信",
                 ifelse(tempW$産業分類 == "H","運輸・郵便",
                 ifelse(tempW$産業分類 == "I","卸小売業",
                 ifelse(tempW$産業分類 == "J","金融保険",
                 ifelse(tempW$産業分類 == "K","不動産・物品賃貸",
                 ifelse(tempW$産業分類 == "L","学術研究・専門技術",
                 ifelse(tempW$産業分類 == "M","飲食・宿泊",
                 ifelse(tempW$産業分類 == "N","生活関連サービス",
                 ifelse(tempW$産業分類 == "O","教育学習",
                 ifelse(tempW$産業分類 == "P","医療福祉",
                 ifelse(tempW$産業分類 == "Q","複合サービス",
                 ifelse(tempW$産業分類 == "R","その他サービス",
                        NA)))))))))))))))))
tempW<- tempW[!is.na(tempW$産業分類),]

# 規模区分のデータを記号から変換
tempW$規模<- ifelse(tempW$規模 == "T","５人以上",
                  ifelse(tempW$規模 == "0","３０人以上",NA))
tempW<- tempW[!is.na(tempW$規模),]

# 就業形態のデータを記号から変換
tempW$就業形態<- ifelse(tempW$就業形態 == "0","就業形態計",
                    ifelse(tempW$就業形態 == "1","一般労働者",
                           ifelse(tempW$就業形態 == "2","パートタイム労働者",NA)))

# 列名を指数表に揃える
colnames(tempW)<- str_replace_all(colnames(tempW),"産業分類","div04") %>%
  str_replace_all("規模","div02") %>%
  str_replace_all("就業形態","div03")

# 仮の時期として1000年1月を設定
tempW$period<- as.Date("1000-01-01")

# 指数区分を実数に
tempW$div05<- "実数"

# データを整形
tempW2<- tempW[,c("div02",
                  "div03",
                  "div04",
                  "div05",
                  "period",
                  "現金給与総額",
                  "きまって支給する給与",
                  "所定内給与",
                  "総実労働時間",
                  "所定内労働時間",
                  "所定外労働時間",
                  "本月末労働者数")]

# 常用雇用指数の対になる実数データの列名を変更
colnames(tempW2)<- str_replace(colnames(tempW2),"本月末労働者数","常用雇用指数")

# ロング型に展開
tempW3<- gather(tempW2,
                key=div01,
                value=value,
                -div02,
                -div03,
                -div04,
                -div05,
                -period)

# データを整形
tempW3<- tempW3[,c("div01",
                   "div02",
                   "div03",
                   "div04",
                   "div05",
                   "period",
                   "value")]
df_temp<- df

# 産業分類の表記を補正
df$div04<- str_remove_all(df$div04,"，") %>%
  str_remove_all("-") %>%
  str_remove_all(",") %>%
  str_remove_all("\\.") %>%
  str_remove_all("（") %>%
  str_remove_all("）") %>%
  str_remove_all("，") %>%
  str_remove_all(",") %>%
  str_remove_all("-") %>%
  str_remove_all("\\.")

# 指数データと実数データを統合
df<- rbind(df,tempW3)
df
