#####################################################################
# 準備
gc();gc(reset=TRUE)
print("gc done")
rm(list=ls(all.names=TRUE))

# 乱数シードの固定
set.seed(555)

# パッケージの読み込み
library(R6)
library(dplyr)

#####################################################################
# マーケットとプレイヤーの設定
# ・　n.Trader人の取引主体がn.repeat回数の取引を行う市場
# ・　財はgoodsのみ
# ・　各取引主体は、個々に異なる財への評価額、財及び貨幣の保有量、市場価格と
#　　評価額の乖離から評価額を改定する学習率を持つ

#マーケットの設定
n.Trader<- 1000   #プレイヤー数
adjSpeed<- 0.3     #価格調整のスピード
n.repeat<- 30     #取引回数

#プレイヤーの設定
max.eva.price<- 13     #評価額の上限
min.eva.price<- 7      #評価額の下限
max.money<- 150    #貨幣保有量の上限
min.money<- 50     #貨幣保有量の下限
min.lrate<- 0      #学習率の下限
max.lrate<- 0.1    #学習率の上限
max.goods<- round(max.money /
                    ((max.eva.price +
                        min.eva.price)/2))    #財保有量の上限
min.goods<- round(min.money /
                    ((max.eva.price +
                        min.eva.price)/2))    #財保有量の下限

####################################################################
# 取引主体のオブジェクトの定義
# ・　各取引主体は、市場価格より自身の評価額が高く取引に必要な貨幣を保有し
#　　ていれば買い、評価額が低く取引に必要な財を保有していれば売りのポジショ
#　　ンをとる
# ・　売買の数を比較し、買い手が多ければ売り手の数まで、売り手が多ければ買
#　　い手の数まで取引が成立するものとし、買い手・売り手とも財一単位を市場
#　　価格で売買する
# ・　売買の成否にかかわらず、すべての取引主体は市場価格と自身の評価額との
#　　乖離を踏まえ、各々の学習率で自身の評価額を改定する

#インスタンスを保存するリスト
Trader_list<- list()

#トレーダークラスの定義
Trader<- R6Class("Trader",
                   public=list(
                     name="T00",　　     #名前
                     eva.price=0,        #売買評価額
                     goods=0,            #財の保有量
                     money=0,            #貨幣の保有量
                     lrate=0.01,         #価格差調整の学習率
                     posit=NULL,         #ポジション

                     #初期化###########################################
                     initialize=function(name,
                                         eva.price,
                                         goods,
                                         money,
                                         lrate,
                                         posit,
                                         goods.pos){
                       self$name=name
                       self$eva.price=eva.price
                       self$goods=goods
                       self$money=money
                       self$lrate=lrate
                       self$posit=posit
                     },
                     
                     #メソッド：表示#######################################
                     view2=function(){
                       print(paste(self$name,
                                   paste0("goods:",self$goods),
                                   paste0("money:",self$money),
                                   paste0("eva.price:",self$eva.price),
                                   paste0("learning rate:",self$lrate)))
                     },
                     
                     #メソッド：価格と保有量に基づく売買判断##################
                     decide=function(mkt.price){
                       
                       #市場価格が高く必要な財を保有していたら売り
                       if(mkt.price > self$eva.price &
                          self$goods > 0){
                         self$posit="sell"
                       
                       #市場価格が安く必要な貨幣を保有していたら買い
                       }else if(mkt.price < self$eva.price &
                                self$money > mkt.price){
                         self$posit="buy"
                         
                       #その他は中立
                       }else{
                         self$posit="non"
                       }
                     },
                     
                     #メソッド：売買の実行####################################
                     trade=function(mkt.price,
                                    buy.price.limit,
                                    sell.price.limit){
                       
                       #ポジションが買いで制限値より高い評価の場合、貨幣保有量から
                       #市場価格を差し引き、財保有量を一単位追加する
                       if(self$posit == "buy" &
                          self$eva.price > buy.price.limit){                         
                         self$money= self$money - mkt.price
                         self$goods= self$goods + 1
                         
                       #ポジションが売りで制限値より低い評価の場合、貨幣保有量に
                       #市場価格を足し、財保有量を一単位差し引く
                       }else if(self$posit == "sell" &
                                self$eva.price < sell.price.limit){
                         self$money= self$money + mkt.price
                         self$goods= self$goods - 1
                       }
                       
#                       #結果の表示
#                       print(paste(self$name,
#                                   self$posit,
#                                   paste0("goods:",self$goods),
#                                   paste0("money:",self$money),
#                                   paste0("eva.price:",self$eva.price)))
                       
                       #市場価格と評価額の差に学習率を乗じて評価額を更新
                       self$eva.price<- self$eva.price + 
                         (mkt.price - self$eva.price) * self$lrate
                     }
                  )
)

#####################################################################
# 取引主体のオブジェクトの生成
# ・　あらかじめ設定した評価額、財及び貨幣の保有量、学習率の最大値・最小値の範囲で
#　　一様乱数で個々に異なる取引主体のオブジェクトを、あらかじめ定義したトレーダー
#　　クラスのインスタンスとしてあらかじめ設定した取引主体数分作成し、一つのリスト
#　　に保管する
# ・　作成と合わせて各評価額を累計していき、その平均を市場価格の初期値とする

#評価額集計のための入れ物
templim<- 0        

#初期インスタンスを取引主体数分のループで作成し、リストに保存していく
for(i in 1:n.Trader){
  
  #各属性についてあらかじめ設定した最大値と最小値の範囲で一様乱数を用いて個々に異なる設定を行う
  temp<- Trader$new(name=paste0("T",formatC(i,digits=4,flag=0)),       #名前
                      eva.price=runif(1,min.eva.price,max.eva.price),  #評価額
                      goods=round(runif(1,min.goods,max.goods)),       #財保有量
                      money=round(runif(1,min.money,max.money)),       #貨幣保有量
                      lrate=runif(1,min.lrate,max.lrate),              #学習率
                      posit="non",                                     #ポジション
                      )
  
  #評価額を累計していく
  templim<- templim + temp$eva.price
  
  #リストに保存
  Trader_list[[i]]<- temp
  
  #表示メソッドで表示
  temp$view2()
}

#市場価格の初期値として、評価額の平均を算出
mkt.price<- templim / length(Trader_list)

#####################################################################
# 取引の実行
# ・　あらかじめ設定した取引回数分のループで取引を実行していく
# ・　まず最初に市場価格をもとに、売買判断のメソッドを適用し、個々の取引主体
#　　のポジションを決めるとともに、その数や評価額を集計等していく
# ・　次に、買い手と売り手の数の少ない方を取引成立件数とし、買い手にについては
#　　評価額が高い方から、売り手については評価額が低い方から、それぞれ成立件数
#　　分までの売買が可能になるものとして、その評価額の閾値を抽出する
# ・　そのうえで、市場価格、買い手及び売り手の評価額の閾値をもとに、取引実行
#　　のメソッドを適用し、各取引主体の財及び貨幣保有量を増減させる
# ・　最後に、買い手と売り手の評価額の平均の差にあらかじめ設定した価格調整スピ
#　　ードを乗じたものを、買い手が多い場合は市場価格に足し、売り手が多い場合は
#　　市場価格から差し引いて、新たな市場価格として再びループに戻る

#結果格納のためのデータフレーム
resmat<- matrix(0,nrow=n.repeat,ncol=5) %>% as.data.frame()
colnames(resmat)<- c("n.buy","n.sell","price.buy","price.sell","price")

#取引ループ
for(i in 1:n.repeat){
  
  #買い手と売り手の数を入れる変数
  n.buy<- 0
  n.sell<- 0
  
  #買い手と売り手の評価額の累計を入れる変数
  eva.price.buy<- 0
  eva.price.sell<- 0
  
  #売買の評価額をためていくベクトル
  buy.price.vec<- vector()
  sell.price.vec<- vector()
  
  #売買判断
  Trader_list<- lapply(Trader_list,function(x){
    x$decide(mkt.price)
    
    #買いの場合
    if(x$posit=="buy"){
      n.buy<<- n.buy + 1                              #買い手の数を累計
      eva.price.buy<<- eva.price.buy + x$eva.price    #買い手の評価額を累計
      buy.price.vec<<- c(buy.price.vec,x$eva.price)   #評価額ベクトル
      
      #売りの場合
    }else if(x$posit=="sell"){
      n.sell<<- n.sell + 1                             #売り手の数を累計
      eva.price.sell<<- eva.price.sell + x$eva.price   #売り手の評価額を累計
      sell.price.vec<<- c(sell.price.vec,x$eva.price)  #評価額ベクトル
    }
    #メソッド適用後のインスタンスを返す
    x
  })
  
  #売買可能件数の導出
  n.trade<- min(n.buy,n.sell)
  
  #買値の下限
  buy.price.limit<- sort(buy.price.vec,
                         decreasing=T)[n.trade]
  
  #売値の下限
  sell.price.limit<- sort(sell.price.vec,
                          decreasing=F)[n.trade]
  
  #取引の実行
  Trader_list<- lapply(Trader_list,function(x){
    x$trade(mkt.price,
            buy.price.limit,
            sell.price.limit)
    x
  })
  
  #取引1回分の結果を表示
  print(paste(paste0("seller:",n.sell),
              paste0("buyer:",n.buy),
              paste0("price:",mkt.price)))
  
  #評価額から価格
  price.buy<- eva.price.buy / n.buy       #買い手の評価額の平均
  price.sell<- eva.price.sell / n.sell    #売り手の評価額の平均
  price.gap<- price.buy - price.sell      #双方の価格差
  
  #買い手の数が多い場合
  if(n.buy > n.sell){
    mkt.price<- mkt.price + price.gap * adjSpeed   #価格差に調整スピードを掛けて足す
    
  #売り手の数が多い場合
  }else if(n.buy < n.sell){
    mkt.price<- mkt.price - price.gap * adjSpeed   #価格差に調整スピードを掛けて引く
  
  #イーブンの場合
  }else{
    mkt.price<- mkt.price
  }
  
  #結果保存
  resmat[i,]<- c(n.buy,n.sell,price.buy,price.sell,mkt.price)
}

#####################################################################
# 可視化のためのデータの作成
# ・　取引回数と評価額や市場価格の推移を示す折れ線グラフ及び財と貨幣の保有量
#　　についてのヒストグラムを作成するため、データを作成

#折れ線グラフに用いるデータを抽出
price_buy<- resmat$price.buy              #買い手の評価額
price_sell<- resmat$price.sell            #売り手の評価額
price_trade<- resmat$price                #市場価格
share_of_buy<- resmat$n.buy / n.Trader    #取引主体に占める買い手の割合

#財と資産の分布を保存するデータフレーム
distmat<- matrix(0,ncol=4,nrow=n.Trader) %>% as.data.frame()
colnames(distmat)<- c("name","goods","money","total")

#ループでデータフレームに名称、財保有量、貨幣保有量、総資産を保存
for(i in 1:n.Trader){
  distmat[i,1]<- Trader_list[[i]]$name
  distmat[i,2]<- Trader_list[[i]]$goods
  distmat[i,3]<- Trader_list[[i]]$money
  distmat[i,4]<- (distmat[i,2] * mkt.price) + distmat[i,3]
}

#####################################################################
# 結果の可視化
# ・　最初にレイアウトを指定し、エリアの上部60％に折れ線グラフ、下部40％を3列
#　　に区分し、それぞれ財及び貨幣の保有量並びに総資産のヒストグラムを配置でき
#　　るようにする
# ・　そのうえで、まずグラフの上部に市場価格、買い手の評価額、売り手の評価額
#　　及び取引主体数に占める買い手の割合をそれぞれ重ね描きする
# ・　そのうえで、プロットエリアを変えながら、財保有量、貨幣保有量及び総資産
#　　のヒストグラムを描いていく

#レイアウトをリセット
layout(1)

#レイアウト指定：2行3列、行は3：2の幅
laymat<- matrix(c(1,1,1,2,3,4),2,3,byrow=T)
layout(laymat,
       heights=c(3,2))

#折れ線グラフ###########################################################
#グラフ表示のために価格の上下を抽出
price.max<- max(resmat$price.buy)    #最大値は買値から
price.min<- min(resmat$price.sell)   #最小値は売値から

#グラフエリアを更新
par(new=F)

#市場価格の推移をグラフ化
plot(price_trade,                  #データ
     type="l",                     #折れ線
     axes=TRUE,                    #第一軸
     ylab="price",                 #縦軸のラベル
     xlab="period",                #横軸のラベル
     ylim=c(price.min,price.max),  #横軸のレンジ
     col="black")                  #折れ線の色

#重ねて描く
par(new=T)

#買値の推移をグラフ化
plot(price_buy,
     type="l",
     axes=TRUE,
     ylab="",
     xlab="",
     ylim=c(price.min,price.max),
     col="red")

#重ねて描く
par(new=T)

#売値の推移をグラフ化
plot(price_sell,
     type="l",
     axes=TRUE,
     ylab="",
     xlab="",
     ylim=c(price.min,price.max),
     col="blue")

#重ねて描く
par(new=T)

#買い手の割合の推移をグラフ化（第二軸）
plot(share_of_buy,
     type="l",
     axes=FALSE,                   #第二軸に指定
     ylab="",
     xlab="",
     col="green",
     main="Transition of Prices")  #グラフタイトル

#凡例
legend("topright",                 #凡例の位置
       legend=c("trade price",     #項目名
                "buy price",
                "sell price",
                "share of buy(right axe)"),
       lty=1,                       #折れ線
       col=c("black",               #色
             "red",
             "blue",
             "green"))

axis(4)
mtext("share",
      side=4,
      line=2)

#次のエリアに
par(new=F)

#財保有量のヒストグラム##############################################
hist(distmat$goods,
     main="Histgram of Goods")

#次のエリアに
par(new=F)

#貨幣保有量のヒストグラム#############################################
hist(distmat$money,
     main="Histgram of Money")

#次のエリアに
par(new=F)

#総資産のヒストグラム#################################################
hist(distmat$total,
     main="Histgram of Total")
