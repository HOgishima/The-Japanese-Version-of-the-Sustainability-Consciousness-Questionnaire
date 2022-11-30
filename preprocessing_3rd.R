# todo: imapact knowledge25%以上をNAに
# todo: スケールの�?転�?目を理解
# todo: �?算す�?
# todo: 相関


### library ###
library(dplyr); library(stringr); library(ggplot2); library(tidyverse)
library(apaTables)
library(sem); library(lavaan); library(semPlot)
library(car)
library(influence.SEM)
library(naniar)
library(Amelia)
library(mice); library(norm2); library(lattice)
library(corrplot); library(corrr)
library(psych); library(GPArotation)

### インポ�?��? ###
# インポ�?�トと�?ータ除�?
LOCATION       <- './data/data_3rd.csv'
df             <- read.csv(LOCATION) %>% 
                  filter(SCQ_attention1 == 1, SCQ_attention2 == 3)
df_row         <- df %>% mutate_all(~ifelse(.==99,NA,.))  # �?損値�?99->NAに

nrow(df_row)

# �?目ごとにスケールをまとめる
df_SCQ <- df_row[,grep('SCQ',colnames(df))]
df_IK <- df_row[,grep('Impact',colnames(df))] %>%
    mutate_all(~as.character(.,na.rm = TRUE))
df_GHA <- df_row[,grep('GHA',colnames(df))] 
df_PEB <- df_row[,grep('PEB',colnames(df))] 
df_SDO <- df_row[,grep('SOD',colnames(df))] 
df_IRI <- df_row[,grep('IRI',colnames(df))] 
df_TIPI <- df_row[,grep('TIPI',colnames(df))] 
df_GCS <- df_row[,grep('GCS',colnames(df))] 

#############################################
### 変数 ###
INVERSE        = c(2,19,22,37,41)

# 整�?
df_reverse     <- df_SCQ %>% mutate_at(INVERSE,function(x){6-x}) # �?転�?目処�?
colnames(df_reverse)[INVERSE] <- c(purrr::map_chr(names(df_reverse[INVERSE]),function(x){paste(x,'i', sep = "")})) # �?転�?目のcolnameに*をつける

scale_data     <- df_reverse[,-c(ncol(df_reverse)-1,ncol(df_reverse))]
age            <- df[,ncol(df)-1]
sex            <- df[,ncol(df)]

LOCATION       <- './data/data_script_2nd.csv'
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
#todo: contentの�?字化けをきれ�?に

### 関数 ###
source('function_scq.r')

### 変数 ###
make_val()
#make_val_short()
ana_data   <- scale_data[unlist(ALL)]
content    <- content[unlist(ALL)]

### �?損値処�? ###
## 25%削除�?ータ ##
# 25?�?以上を削除
tf           <- rep(FALSE,nrow(ana_data))
varlist      <- list(K_ENV,K_SOC,A_SOC,B_ENV,B_SOC)
delsubjlist  <- c()
for(i in varlist){
    temp <- check_na_ratio(i,0.25)
    delsubjlist <- cbind(delsubjlist,temp)
    print(delsubjlist)
    tf   <- tf | temp
}

# ただし�?4�?目しかな�?尺度は2/4脱落でアウトとする
varlist  <- list(K_ECO,A_ENV,A_ECO,B_ECO)
for(i in varlist){
    temp <- check_na_ratio(i,0.50)
    delsubjlist <- cbind(delsubjlist,temp)
    tf   <- tf | temp
}
colnames(delsubjlist) <- c('K_ENV','K_SOC','A_SOC','B_ENV','B_SOC','K_ECO','A_ENV','A_ECO','B_ECO')

#data_25del <- ana_data[!tf,]
#data_25del <- data.frame(t(apply(data_25del, 1, as.integer)))
#nrow(ana_data)
#nrow(data_25del)

# Impact Knowledge
## 25%削除�?ータ ##
# 25?�?以上を削除
tf       <- rep(FALSE,nrow(df_IK))
f     <- function(a){sum(is.na(a))/length(a)}
temp  <- apply(df_IK,1,f) >= 0.25
tf   <- tf | temp
delsubjlist <- cbind(delsubjlist,IK=temp)

#IK_25del <- df_IK[!tf,]
#IK_25del <- data.frame(t(apply(IK_25del, 1, as.integer)))

# 多重代入 #
# MCAR test => not MCAR
INPUT_DATA <- data.frame(scale_data,df_GHA,df_PEB,df_SDO,df_IRI,df_TIPI,df_GCS,df_IK)
head(INPUT_DATA)
mcar_test(INPUT_DATA) # p<.05 => not MCAR
# 多重代入
M          <- 5
seed2      <- 1
#INPUT_DATA <- data_25del
em_result  <- emNorm(INPUT_DATA,iter.max=10000) # 期�?値最大化法�?EM?��によって初期値を設�?
max2       <- em_result$iter*2
imp        <- mice(INPUT_DATA,m=M,seed=seed2,maxit=max2,
                meth = c(rep('norm',ncol(INPUT_DATA)-ncol(df_IK)),rep('polyreg',ncol(df_IK)))
              )
dataimp    <- complete(imp, action="broad", include=FALSE)
data_imp   <- data.frame(dataimp)[,1:ncol(INPUT_DATA)]
nrow(dataimp)
imp
colnames(data_imp) <- colnames(INPUT_DATA)
write.csv(data_imp, file = './data/data_imp_rev.csv',row.names = FALSE)

### 外れ被験�?を除�? ###
# note: https://ides.hatenablog.com/entry/2022/02/07/023325
# note: http://blog.livedoor.jp/yukismd/archives/1398869.html
#INPUT_DATA  <- data_imp
#mdist       <- data.frame(distance=mahalanobis(INPUT_DATA, colMeans(INPUT_DATA), cov(INPUT_DATA)))
#pvalue       <- round(apply(mdist,1,function(x){pchisq(x, ncol(INPUT_DATA),lower.tail=F)}),3)
#df_distance  <- data.frame(mdist,pvalue)
#df_distance[order(df_distance$pvalue,decreasing=F),]
#nrow(df_distance)-nrow(filter(df_distance, pvalue > 0.05))
#63/(281+63)
#df_distance_select <- subset(df_distance,pvalue > 0.01)
#numlist            <- rownames(subset(df_distance_select,pvalue > 0.01)) # 消える行番号の抽出
#data_outlier <- INPUT_DATA[numlist,]
#nrow(data_outlier)

############################################################
# dataset
ana_subj <- rownames(ana_data)
IK_25del_subj <- rownames(IK_25del)
outlier_subj <- rownames(data_outlier)

# 比�?
length(intersect(ana_subj, IK_25del_subj)
setdiff(ana_subj, IK_25del_subj)
length(intersect(rownames(data_outlier), rownames(IK_25del)))
setdiff(outlier_subj, IK_25del_subj)
setdiff(IK_25del_subj,outlier_subj)

# ローデータ
#LOCATION       <- './data/data_imp_rev.csv'
#df_imp         <- read.csv(LOCATION)
df_imp <- df_row
rownames(df_imp) <- rownames(df_row)
SCQ_K <- df_imp[,unlist(ALL$K)]
SCQ_A <- df_imp[,unlist(ALL$A)]
SCQ_B <- df_imp[,unlist(ALL$B)] 

IK_CORR <- c(3,2,3,3,2,2,3,2,1,2,2,3,1)
f2        <- function(a){sum(a == IK_CORR, na.rm = TRUE)}
f3        <- function(a){sum(a == IK_CORR)}
IK        <- apply(df_IK,1,f2)
IK_row    <- apply(df_IK,1,f3)
IK_ratio  <- IK/(ncol(df_IK)-apply(is.na(df_IK),1,sum))
IK_del    <- replace(IK_ratio,c(delsubjlist[,'IK']),NA)

SDO_INV <- c(9:16) # 7
SDO     <- df_SDO %>% 
           mutate_at(SDO_INV,function(x){8-x}) # �?転�?目処�?

IRI_INV <- c(5:7) # 5
IRI     <- df_IRI %>% 
           mutate_at(IRI_INV,function(x){6-x}) # �?転�?目処�?

TIPI_EX <- df_TIPI[,c(1,6)]
TIPI_AG <- df_TIPI[,c(2,7)]
TIPI_CO <- df_TIPI[,c(3,8)]
TIPI_NE <- df_TIPI[,c(4,9)]
TIPI_OP <- df_TIPI[,c(5,10)]

PEB     <- df_PEB[,grep('(M1)|(M4)',colnames(df_PEB))]
PEB_mean <- apply(df_PEB,1,mean,na.rm = TRUE)

# 相関�?ータフレー�?の作�??
make_val_short(c(46,47))
ALL$B$B_ECO <- c(48,49)
B$B_ECO <- c(48,49)
ECO$B_ECO <- c(48,49)

## raw
DATASET1 <- 
        data.frame(
            SCQ_total = apply(df_imp[,unlist(ALL)],1,sum),
            SCQ_K = apply(SCQ_K,1,sum),
            SCQ_A = apply(SCQ_A,1,sum),       
            SCQ_B = apply(SCQ_B,1,sum),
            IK = IK_row,
            GHA = apply(df_GHA,1,sum),
            PEB = apply(PEB,1,sum),
            SDO = apply(SDO,1,sum),
            IRI = apply(IRI,1,sum),        
            TIPI_EX = apply(TIPI_EX,1,sum),
            TIPI_AG = apply(TIPI_AG,1,sum),
            TIPI_CO = apply(TIPI_CO,1,sum),
            TIPI_NE = apply(TIPI_NE,1,sum),
            TIPI_OP = apply(TIPI_OP,1,sum),
            GCS     = apply(df_GCS,1,sum)
        )

## 25%削除 + 多重代入?�?IKは含めな�??�?
LOCATION       <- './data/data_imp_rev.csv'
df_imp         <- read.csv(LOCATION)

df_SCQ <- df_imp[,grep('SCQ',colnames(df_imp))]

rownames(df_imp) <- rownames(df_row)
SCQ_K <- df_imp[,unlist(ALL$K)]
SCQ_A <- df_imp[,unlist(ALL$A)]
SCQ_B <- df_imp[,unlist(ALL$B)] 

df_GHA <- df_imp[,grep('GHA',colnames(df_imp))] 

### 外れ被験�?を除�? ###
INPUT_DATA  <- data.frame(df_imp[,unlist(ALL$K)],df_GHA,PEB,df_SDO,df_IRI,df_TIPI,df_GCS)
    mdist       <- data.frame(distance=mahalanobis(INPUT_DATA, colMeans(INPUT_DATA), cov(INPUT_DATA)))
pvalue       <- round(apply(mdist,1,function(x){pchisq(x, ncol(INPUT_DATA),lower.tail=F)}),3)
df_distance  <- data.frame(mdist,pvalue)
df_distance[order(df_distance$pvalue,decreasing=F),]
df_distance_select <- subset(df_distance,pvalue > 0.01)
numlist            <- rownames(subset(df_distance_select,pvalue > 0.01)) # 消える行番号の抽出
data_outlier <- INPUT_DATA[numlist,]
#nrow(data_outlier)
write.csv(data_imp[numlist,], file = './data/data_imp_outlier.csv',row.names = FALSE)


DATASET2 <- DATASET1 %>%
    mutate(
        SCQ_total = replace(apply(df_imp[,unlist(ALL)],1,sum), apply(delsubjlist[,-ncol(delsubjlist)],1,any),NA),
        SCQ_K = replace(SCQ_K, apply(delsubjlist[,c('K_ENV','K_SOC','K_ECO')],1,any),NA),
        SCQ_A = replace(SCQ_A, apply(delsubjlist[,c('A_ENV','A_SOC','A_ECO')],1,any),NA),
        SCQ_B = replace(SCQ_B, apply(delsubjlist[,c('B_ENV','B_SOC','B_ECO')],1,any),NA),
        IK = IK_del,
        GHA = apply(df_GHA,1,sum),
        PEB = PEB_mean
    )


## 25%削除 + 多重代入?�?IKは含める?�?
DATASET3 <- DATASET2[numlist,]


# 相関
result <- cor(DATASET1, use = "pairwise.complete.obs")
corr.test(DATASET1)
cor.plot(result, numbers=T)

library(corrplot)
corrplot(result, tl.col="black")

result <- cor(DATASET2, use = "pairwise.complete.obs")
corr.test(DATASET2)
cor.plot(result, numbers=T)

library(corrplot)
corrplot(result, tl.col="black")

result <- cor(DATASET3, use = "pairwise.complete.obs")
corr.test(DATASET3)
cor.plot(result, numbers=T)

library(corrplot)
corrplot(result[1:4,], tl.col="black")

result[1:4,]

corrplot

ALL

warnings()
