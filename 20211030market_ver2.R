gc();gc(reset=TRUE)
print("gc done")
rm(list=ls(all.names=TRUE))

# 乱数シードの固定
set.seed(55)

# パッケージの読み込み
library(R6)
library(dplyr)

#インスタンスを保存するリスト
Trader_list<- list()

#マーケットの設定
n.Trader<- 1000   #プレイヤー数
adjSpeed<- 0.3     #価格調整のスピード
n.repeat<- 30     #取引回数

#プレイヤーの設定
max.eva.price<- 13     #評価額の上限
min.eva.price<- 7      #評価額の下限
max.goods<- 150    #財保有量の上限
min.goods<- 50     #財保有量の下限
max.money<- 150    #貨幣保有量の上限
min.money<- 50     #貨幣保有量の下限
min.lrate<- 0      #学習率の下限
max.lrate<- 0.1    #学習率の上限

#トレーダークラスの定義
Trader<- R6Class("Trader",
                   public=list(
                     name="T00",　　     #名前
                     eva.price=0,        #売買評価額
                     goods=0,            #財の保有量
                     money=0,            #貨幣の保有量
                     lrate=0.01,         #価格差調整の学習率
                     posit=NULL,         #ポジション

                     #初期化
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
                     
                     #表示メソッド
                     view2=function(){
                       print(paste(self$name,
                                   paste0("goods:",self$goods),
                                   paste0("money:",self$money),
                                   paste0("eva.price:",self$eva.price),
                                   paste0("learning rate:",self$lrate)))
                     },
                     
                     #価格と保有量を変数とした売り買い判断
                     decide=function(mkt.price){
                       
                       #市場価格が高く必要な財を保有していたら売り
                       if(mkt.price > self$eva.price &
                          mkt.price < self$goods){
                         self$posit="sell"
                       
                       #市場価格が安く必要な貨幣を保有していたら買い
                       }else if(mkt.price < self$eva.price &
                                mkt.price < self$money){
                         self$posit="buy"
                         
                       #その他は中立
                       }else{
                         self$posit="non"
                       }
                     },
                     
                     #売買の実行
                     trade=function(mkt.price,
                                    buy.price.limit,
                                    sell.price.limit){
                       
                       #ポジションが買いで制限値より高い評価の場合
                       if(self$posit == "buy" &
                          self$eva.price > buy.price.limit){
                         self$money= self$money - mkt.price
                         self$goods= self$goods + mkt.price
                         
                       #ポジションが売りで制限値より低い評価の場合
                       }else if(self$posit == "sell" &
                                self$eva.price < sell.price.limit){
                         self$money= self$money + mkt.price
                         self$goods= self$goods - mkt.price
                       }
                       
                       #表示
                       print(paste(self$name,
                                   self$posit,
                                   paste0("goods:",self$goods),
                                   paste0("money:",self$money),
                                   paste0("eva.price:",self$eva.price)))
                       
                       #価格差と学習率による評価額の更新
                       self$eva.price<- self$eva.price + 
                         (mkt.price - self$eva.price) * self$lrate
                     }
                  )
)

#評価額集計のための入れ物
templim<- 0        

#初期インスタンスの作成
for(i in 1:n.Trader){
  
  #インスタンスを作成してリストにストック
  temp<- Trader$new(name=paste0("T",formatC(i,digits=4,flag=0)),       #名前
                      eva.price=runif(1,min.eva.price,max.eva.price),  #評価額
                      goods=round(runif(1,min.goods,max.goods)),       #財保有量
                      money=round(runif(1,min.money,max.money)),       #貨幣保有量
                      lrate=runif(1,min.lrate,max.lrate),              #学習率
                      posit="non",                                      #ポジション
                      )
  
  #評価額を累計していく
  templim<- templim + temp$eva.price
  
  #リストに保存
  Trader_list[[i]]<- temp
  
  #表示メソッドで表示
  temp$view2()
}

#価格の初期値は評価額の平均
mkt.price<- templim / length(Trader_list)

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
  
  #売買評価額の導出
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
  price.gap<- price.buy - price.sell  #双方の価格差
  
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

#財と資産の分布を抽出
distmat<- matrix(0,ncol=4,nrow=n.Trader) %>% as.data.frame()   #入れ物
colnames(distmat)<- c("name","goods","money","total")

for(i in 1:n.Trader){                      #各行に入れていく
  distmat[i,1]<- Trader_list[[i]]$name
  distmat[i,2]<- Trader_list[[i]]$goods
  distmat[i,3]<- Trader_list[[i]]$money
  distmat[i,4]<- distmat[i,2] + distmat[i,3]
}

#レイアウト指定
layout(1)

laymat<- matrix(c(1,1,1,2,3,4),2,3,byrow=T)
layout(laymat,
       heights=c(3,2))

#グラフ表示のために価格の上下を抽出
price.max<- max(resmat$price.buy)    #最大値は買値から
price.min<- min(resmat$price.sell)   #最小値は売値から

#グラフエリアを更新
par(new=F)

#価格の推移をグラフ化
plot(resmat$price,
     type="l",
     axes=TRUE,
     ylab="price",
     ylim=c(price.min,price.max),
     col="black")

#重ねて描く
par(new=T)

#買値の推移をグラフ化
plot(resmat$price.buy,
     type="l",
     axes=TRUE,
     ylab="",
     ylim=c(price.min,price.max),
     col="red")

#重ねて描く
par(new=T)

#売値の推移をグラフ化
plot(resmat$price.sell,
     type="l",
     axes=TRUE,
     ylab="",
     ylim=c(price.min,price.max),
     col="blue")

#重ねて描く
par(new=T)

#買い手の割合の推移をグラフ化（第二軸）
plot(resmat$n.buy / n.Trader,
     type="l",
     axes=FALSE,
     ylab="",
     col="green")
axis(4)
mtext("price",
      side=4,
      line=2)

#次のエリアに
par(new=F)

#財のヒストグラム
hist(distmat$goods,
     main="Histgram of Goods")

#次のエリアに
par(new=F)

#貨幣のヒストグラム
hist(distmat$money,
     main="Histgram of Money")

#総資産のヒストグラム
hist(distmat$total,
     main="Histgram of Total")
