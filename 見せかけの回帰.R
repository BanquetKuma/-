library(vars)
library(tseries)
library(CADFtest)
library(tidyverse)
library(ggsci)
library(magrittr)
library(vars)
install.packages("car", dependencies = TRUE)
# 偏グレンジャー因果用のスクリプト読み込み
#source('https://raw.githubusercontent.com/cran/FIAR/master/R/partGranger.R')
# install.packages("dplyr", dependencies = TRUE)
library(dplyr)
# 全ての古いパッケージをアップデートする
# update.packages()
#packageVersion("dplyr")

df <- read.csv("C:\\Users\\SHO\\PycharmProjects\\Sony_Stock_price.csv")

#属性確認
str(df)

# Date列をDat型に変更する
df$Date <- as.POSIXct(strptime(as.character(df$Date), format="%Y-%m-%d"))

#属性確認
str(df)

# 選択列のベクトル
columnList <- c("Date","Close")
df <- df[, columnList]

# 終値の原系列をプロット
g <- ggplot(df, aes(x = Date, y = Close))
g <- g + geom_line()+
  scale_x_datetime(date_breaks = "1 month")
  
plot(g)

# ADF検定を行う
CADFtest(df$Close,type=c("trend"),max.lag.y = 10)

# あるランダムウォークに従う過程
rw<-cumsum(rnorm(210))
plot.ts(rw,lwd=2)

# 終値をランダムウォークで回帰する
summary(lm(df$Close~rw))

# 二つの変数をプロットする
matplot(cbind(df$Close,rw),type="l",lwd=3,lty=1)

# 1階差分を取る
diff_df <- diff(df$Close)

plot.ts(diff_df)

# ADF検定を行う
CADFtest(diff_df,type=c("trend"),max.lag.y = 10)

# 2階差分を取る
diff_diff_df <- diff(diff_df)

plot.ts(diff_diff_df)

# ADF検定を行う(2階差分で定常過程になった）
CADFtest(diff_diff_df,type=c("trend"),max.lag.y = 10)

# ランダムウォークの2階差を求める
diff_diff_rw <- diff(diff(rw))

# ADF検定を行う
CADFtest(diff_diff_rw,type=c("trend"),max.lag.y = 10)

# 2階差で回帰する
result <- lm(diff_diff_df~diff_diff_rw)
summary(result)

# 2階差を取った二つの変数をプロットする
matplot(cbind(diff_diff_df,diff_diff_rw),type="l",lwd=3,lty=1)

# フィッティングを図示
predict.c <- predict(result)
plot.ts(diff_diff_df)
par(new=T)
plot(c(1:208), predict.c, type="o",col = "blue")

# 終値をランダムウォークとその1階差分で回帰する
result <- lm(df$Close[c(2:210)]~rw[c(2:210)]+diff(rw)
             +diff_df)

# 予測用
result2 <- lm(df$Close[c(2:180)]~rw[c(2:180)]+diff(rw)[c(1:179)]
             +diff_df[c(1:179)])

summary(result2)

require(car)

# 多重共線性をチェックする
vif(result2)

# フィッティングを図示
predict.c <- predict(result2)
plot.ts(df$Close[c(2:180)],ylim=c(28,45), xlab="",ylab="")
par(new=T)
plot(c(1:179), predict.c, ,ylim=c(28,45),type="o",col = "blue")

# 残渣
residuals(result)

df$Close[c(181:210)]

# 予測したい変数をデータフレームにまとめる
df_pred <- data.frame(rw = rw[c(181:210)], 
                      diff_rw = diff(rw)[c(180:209)], 
                      diff_diff = diff_df[c(180:209)])

pred = predict(result2, newdata=df_pred)


plot.ts(df$Close[c(181:210)], xlim=c(0,30),ylim=c(28,45),
        ylab="")
par(new=T)
plot.ts(pred,xlim=c(0,30),ylim=c(28,45),type="o",col = "blue")  
        
