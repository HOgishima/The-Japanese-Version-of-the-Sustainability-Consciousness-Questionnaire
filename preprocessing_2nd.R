### library ###
library(dplyr); library(stringr); library(stringr); library(tidyverse)
#library(apaTables)
library(sem); library(lavaan); library(semPlot)
library(car)
library(influence.SEM)
library(naniar)
library(Rcpp)
library(Amelia)
library(mice); library(norm2); library(lattice)
library(corrplot); library(corrr)
library(psych); library(GPArotation)

### 変数 ###
INVERSE        = c(2,19,22,37,41)

### インポート ###
# インポートと整理
LOCATION       <- './data/data_2nd.csv'
df             <- read.csv(LOCATION) %>%
                  filter(SCQ_attention1 == 1, SCQ_attention2 == 3, participated_study1 == 0)
df_row         <- df %>% mutate_all(~ifelse(.==99,NA,.))  # 欠損値を99->NAに
df_reverse     <- df_row %>% mutate_at(INVERSE,function(x){6-x}) # 逆転項目処理
colnames(df_reverse)[INVERSE] <- c(purrr::map_chr(names(df_reverse[INVERSE]),function(x){paste(x,'i', sep = "")})) # 逆転項目のcolnameに*をつける

scale_data     <- df_reverse[,1:49]
age            <- df_reverse[,ncol(df_reverse)-2]
sex            <- df_reverse[,ncol(df_reverse)-1]

LOCATION       <- './data/data_script.csv'
df_script      <- read.csv(LOCATION, fileEncoding = "utf-8")
list_script    <- strsplit(df_script[1,1], "\n\n")
list_script    <- strsplit(list_script[[1]], ",")[2:50]
content        <- c()
for (i in 1:ncol(scale_data)){
    inv        <- ifelse(is.element(i,INVERSE),'*','')
    txt        <- paste(inv, list_script[[i]][2])
    content    <- c(content,txt)
}
#content        <- content[unlist(ALL)]
#todo: contentの文字化けをきれいに

### 関数 ###
source('function_scq.r')

### 変数 ###
make_val()
#make_val_short()
ana_data   <- scale_data[unlist(ALL)]
content    <- content[unlist(ALL)]

### 欠損値処理 ###
## リストワイズ ##
data_lw    <- na.omit(ana_data)

## 25%削除データ ##
# 25％以上を削除
tf       <- rep(FALSE,nrow(ana_data))
varlist  <- list(K_ENV,K_SOC,A_SOC,B_ENV)
for(i in varlist){
    temp <- check_na_ratio(i,0.25)
    tf   <- tf | temp
}

# ただし，4項目しかない尺度は2/4脱落でアウトとする
varlist  <- list(K_ECO,A_ENV,A_ECO,B_SOC)
for(i in varlist){
    temp <- check_na_ratio(i,0.50)
    tf   <- tf | temp
}

data_25del <- ana_data[!tf,]
data_25del <- data.frame(t(apply(data_25del, 1, as.integer)))
nrow(data_25del)

# 多重代入 #
# MCAR test => not MCAR
mcar_test(data_25del)
# 多重代入
M          <- 5
seed2      <- 1
INPUT_DATA <- data_25del
em_result  <- emNorm(INPUT_DATA,iter.max=10000) # 期待値最大化法（EM）によって初期値を設定
max2       <- em_result$iter*2
imp        <- mice(INPUT_DATA,m=M,seed=seed2,meth='norm',maxit=max2)
dataimp    <- complete(imp, action="broad", include=FALSE)
data_imp   <- data.frame(dataimp)[,1:ncol(ana_data)]
colnames(data_imp) <- colnames(ana_data)

### 外れ被験者を除外 ###
# note: https://ides.hatenablog.com/entry/2022/02/07/023325
# note: http://blog.livedoor.jp/yukismd/archives/1398869.html
INPUT_DATA  <- data_imp
mdist       <- data.frame(distance=mahalanobis(INPUT_DATA, colMeans(INPUT_DATA), cov(INPUT_DATA)))
pvalue       <- round(apply(mdist,1,function(x){pchisq(x, ncol(INPUT_DATA),lower.tail=F)}),3)
df_distance  <- data.frame(mdist,pvalue)
#df_distance[order(df_distance$pvalue,decreasing=F),]
#nrow(df_distance)-nrow(filter(df_distance, pvalue > 0.05))
#63/(281+63)
df_distance_select <- subset(df_distance,pvalue > 0.01)
numlist            <- rownames(subset(df_distance_select,pvalue > 0.01)) # 消える行番号の抽出
data_outlier <- INPUT_DATA[numlist,]
nrow(data_outlier)

### output ###
write.csv(x = data_outlier, file = './data/data_outlier_2nd.csv')
write.csv(x = data.frame(age, sex)[numlist,], file = './data/data_outlier_demo_2nd.csv')