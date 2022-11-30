source("packages.r")
source('function_scq.r')

### Variables ###
INVERSE        = c(2,19,22,37,41)

### Import ###
# import and preparation
## scale data
LOCATION       <- './data/data.csv'
df             <- read.csv(LOCATION)
df_row         <- df %>% mutate_all(~ifelse(.==99,NA,.))  # 99->NA
df_reverse     <- df_row %>% mutate_at(INVERSE,function(x){6-x}) # for reverse items
colnames(df_reverse)[INVERSE] <- c(purrr::map_chr(names(df_reverse[INVERSE]),function(x){paste(x,'i', sep = "")})) # add * for reverse items

scale_data     <- df_reverse[,-c(ncol(df_reverse)-1,ncol(df_reverse))]
age_data       <- df_reverse[,ncol(df_reverse)-1]
sex_data       <- df_reverse[,ncol(df_reverse)]

## content data
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

content    <- content[unlist(ALL)]
#todo: contentの�?字化けをきれ�?に

ana_data   <- scale_data[unlist(ALL)]

### Missing ###
## 確�? ##
# 被験�?�?ータ
#inv_scale  <- data.frame(t(ana_data))
#miss_var_summary(inv_scale)
#vis_miss(inv_scale)
#gg_miss_var(inv_scale)
### �?損値 ###
## 確�? ##
# 質問�??目
#gg_miss_var(ana_data)
#vis_miss(ana_data) + 
#    theme(plot.margin= unit(c(1, 3, 1, 1), "lines"))
#miss_var_summary(ana_data)

### Deletion ###
## リストワイズ ##
#data_lw    <- na.omit(ana_data)

## 25%削除�?ータ ##
# 25?�?以上を削除
tf       <- rep(FALSE,nrow(ana_data))
varlist  <- list(K_ENV,K_SOC,A_SOC,B_ENV,B_SOC)
for(i in varlist){
    temp <- check_na_ratio(i,0.25)
    tf   <- tf | temp
}

# ただし�?4�?目しかな�?尺度は2/4脱落でアウトとする
varlist  <- list(K_ECO,A_ENV,A_ECO,B_ECO)
for(i in varlist){
    temp <- check_na_ratio(i,0.50)
    tf   <- tf | temp
}

data_25del <- ana_data[!tf,]
data_25del <- data.frame(t(apply(data_25del, 1, as.integer)))
nrow(data_25del)

# もう一度�?損値確�?
#inv_scale  <- data.frame(t(data_25del))
#miss_var_summary(inv_scale)
#vis_miss(inv_scale)
#gg_miss_var(inv_scale)

### Imputations ###
# MCAR test => not MCAR
Result_mcar <- mcar_test(data_25del)
# multiple imputations
M          <- 5
seed2      <- 1
INPUT_DATA <- data_25del
em_result  <- emNorm(INPUT_DATA,iter.max=10000) # 期�?値最大化法�?EM?��によって初期値を設�?
max2       <- em_result$iter*2
imp        <- mice(INPUT_DATA,m=M,seed=seed2,meth='norm',maxit=max2)
dataimp    <- complete(imp, action="broad", include=FALSE)
data_imp   <- data.frame(dataimp)[,1:ncol(ana_data)]
colnames(data_imp) <- colnames(ana_data)

### omit outliers ###
# note: https://ides.hatenablog.com/entry/2022/02/07/023325
# note: http://blog.livedoor.jp/yukismd/archives/1398869.html
INPUT_DATA  <- data_imp
mdist       <- data.frame(distance=mahalanobis(INPUT_DATA, colMeans(INPUT_DATA), cov(INPUT_DATA)))
pvalue      <- round(apply(mdist,1,function(x){pchisq(x, ncol(INPUT_DATA),lower.tail=F)}),3)
df_distance <- data.frame(mdist,pvalue)
#df_distance[order(df_distance$pvalue,decreasing=F),]
#nrow(df_distance)-nrow(filter(df_distance, pvalue > 0.05))
#63/(281+63)
df_distance_select <- subset(df_distance,pvalue > 0.01)
numlist            <- rownames(subset(df_distance_select,pvalue > 0.01)) # 消える行番号の抽出
data_outlier <- INPUT_DATA[numlist,]
 
### output ###
write.csv(x = data_outlier, file = './data/data_outlier_1st.csv')
write.csv(x = data.frame(age_data, sex_data)[numlist,], file = './data/data_outlier_demo_1st.csv')

