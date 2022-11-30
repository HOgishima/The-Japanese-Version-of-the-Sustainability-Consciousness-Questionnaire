### ###
itemnames <- c(
    "SCQ_1","SCQ_2i","SCQ_3","SCQ_4","SCQ_5","SCQ_6","SCQ_7",
    "SCQ_8","SCQ_9","SCQ_10","SCQ_11","SCQ_12","SCQ_13","SCQ_14",
    "SCQ_15","SCQ_16","SCQ_17","SCQ_18","SCQ_19i","SCQ_20","SCQ_21",
    "SCQ_22i","SCQ_23","SCQ_24","SCQ_25","SCQ_26","SCQ_27","SCQ_28",
    "SCQ_29","SCQ_30","SCQ_31","SCQ_32","SCQ_33","SCQ_34","SCQ_35",
    "SCQ_36","SCQ_37i","SCQ_38","SCQ_39","SCQ_40","SCQ_41i","SCQ_42",
    "SCQ_43","SCQ_44","SCQ_45","SCQ_46","SCQ_47","SCQ_48","SCQ_49"
    )


### �֐� ###
## �e�L�X�g�쐬�֘A�̊֐� ##
# note: �v�f���ЂƂ�list���󂯕t���āC���̗v�f�̗v�f��+�Ō���
model_txt_subsub <- function(subsubscale_name){
    txt <- paste(names(subsubscale_name), ' =~ ')
    item_num <- length(subsubscale_name[[1]])
    for(j in 1:item_num){
        item_name <- itemnames[subsubscale_name[[1]][j]]
        if(j == item_num){
            txt <- paste(txt, item_name)
        }else{
            txt <- paste(txt, item_name, ' + ')
        }
    }
    return (txt)
}
# note: �����v�f��list���󂯕t���āC���X�g�̗v�f '=~' �v�f�̗v�f 
model_txt_sub    <- function(subscale_name){
    txt <- ''
    subfactor_name <- names(subscale_name)
    for (i in 1:length(subscale_name)){
        txt <- paste(txt, '\n ', model_txt_subsub(subscale_name[i]))
    }
    return(txt)
}
# ���X�g�̃��X�g���󂯕t����B
model_txt        <- function(ALL){
    txt <- ''
    for (i in 1:length(ALL)){
        txt <- paste(txt,'\n', model_txt_sub(ALL[[i]]))
    }
    return(txt)
}
cor_txt          <- function(x,y){
    #xnum  <- grep(x,original_names)      # x�̍s�ԍ��̎��o��
    #ynum  <- grep(y,original_names)      # y�̍s�ԍ��̎��o��
    xname <- itemnames[x] # x�̖��̂����o��
    yname <- itemnames[y] # y�̖��̂����o��
    return(paste(xname,' ~~ ',yname))    # ~�Ō���
}
cor_txt_all     <- function(x,y){
    temp_list <- purrr::map2(x,y,~cor_txt(.x,.y))
    return(paste(temp_list,collapse=' \n '))
}
cor_txt_from_list <- function(LIST){
    df <- combn(x=unlist(LIST),m=2)
    return(cor_txt_all(df[1,],df[2,]))
}
model_txt_higher <- function(NAME){
    txt         <- paste(NAME,' =~ ')
    VEC         <- names(get(NAME))
    item_num    <- length(VEC)
    for(j in 1:item_num){
        if(j == item_num){
            txt <- paste(txt, VEC[j])
        }else{
            txt <- paste(txt, VEC[j], ' + ')
        }
    }
    return (txt)
}
model_txt_unlist <- function(LISTNAME){
    tempvec <- unlist(get(LISTNAME))
    txt     <- paste(LISTNAME,' =~ ')
    txt2    <- paste(itemnames[tempvec], collapse = " + ")
    return(paste(txt, txt2))
}

# �e�s��NA��ratio�𒴂��邩�`�F�b�N����T/F�ŕԂ�
check_na_ratio <- function(x,y){ # x = valname, y = ratio
    f     <- function(a){sum(is.na(a))/length(a)}
    temp  <- apply(scale_data[x[[1]]],1,f) >= y 
    return(temp)
}
# list���琔���𔲂�
# note: https://jpcodeqa.com/q/263bc8e1a22528bf56ade804d81ed1c8
removeListElem <- function(inlist,elem_remove){
  outlist = lapply(inlist,setdiff,elem_remove)
  outlist[lengths(outlist) > 0]
}

### �ϐ� ###
make_val<- function(DELLIST=c()){
    ### �ϐ� ###
    K_ENV          <<- lapply(list(1:6),setdiff,DELLIST)
    K_SOC          <<- lapply(list(7:14),setdiff,DELLIST)
    K_ECO          <<- lapply(list(15:18),setdiff,DELLIST)
    names(K_ENV)   <<- c('K_ENV')
    names(K_SOC)   <<- c('K_SOC')
    names(K_ECO)   <<- c('K_ECO')

    A_ENV          <<- lapply(list(19:22),setdiff,DELLIST)
    A_SOC          <<- lapply(list(23:28),setdiff,DELLIST)
    A_ECO          <<- lapply(list(29:32),setdiff,DELLIST)
    names(A_ENV)   <<- c('A_ENV')
    names(A_SOC)   <<- c('A_SOC')
    names(A_ECO)   <<- c('A_ECO')

    B_ENV          <<- lapply(list(33:39),setdiff,DELLIST)
    B_SOC          <<- lapply(list(40:45),setdiff,DELLIST)
    B_ECO          <<- lapply(list(46:49),setdiff,DELLIST)
    names(B_ENV)   <<- c('B_ENV')
    names(B_SOC)   <<- c('B_SOC')
    names(B_ECO)   <<- c('B_ECO')

    K              <<- c(K_ENV, K_SOC, K_ECO)
    A              <<- c(A_ENV, A_SOC, A_ECO)
    B              <<- c(B_ENV, B_SOC, B_ECO)

    ENV            <<- c(K_ENV, A_ENV, B_ENV)
    SOC            <<- c(K_SOC, A_SOC, B_SOC)
    ECO            <<- c(K_ECO, A_ECO, B_ECO)

    ALL            <<- list(K, A, B)
    names(ALL)     <<- c('K','A', 'B')

    original_names <<- c(
    'KENV3','KENV4i','KENV7','KENV14','KENV18','KENV21',
    'KSOC2','KSOC5','KSOC8','KSOC9','KSOC10','KSOC11','KSOC15','KSOC20',
    'KECO12','KECO16','KECO17','KECO19',
    'AENV5i','AENV6','AENV10','AENV19i',
    'ASOC1','ASOC2','ASOC11','ASOC13','ASOC14','ASOC18',
    'AECO3','AECO7','AECO8','AECO16',
    'BENV1','BENV2','BENV3','BENV7','BENV8i','BENV10','BENV12',
    'BSOC4','BSOC5i','BSOC13','BSOC14','BSOC15','BSOC17',
    'BECO6','BECO9','BECO11','BECO16'
    )

    original_names_rev <<- c(
    'K3(ENV)','K4i(ENV)','K7(ENV)','K14(ENV)','K18(ENV)','K21(ENV)',
    'K2(SOC)','K5(SOC)','K8(SOC)','K9(SOC)','K10(SOC)','K11(SOC)','K15(SOC)','K20(SOC)',
    'K12(ECO)','K16(ECO)','K17(ECO)','K19(ECO)',
    'A5i(ENV)','A6(ENV)','A10(ENV)','A19i(ENV)',
    'A1(SOC)','A2(SOC)','A11(SOC)','A13(SOC)','A14(SOC)','A18(SOC)',
    'A3(ECO)','A7(ECO)','A8(ECO)','A16(ECO)',
    'B1(ENV)','B2(ENV)','B3(ENV)','B7(ENV)','B8i(ENV)','B10(ENV)','B12(ENV)',
    'B4(SOC)','B5i(SOC)','B13(SOC)','B14(SOC)','B15(SOC)','B17(SOC)',
    'B6(ECO)','B9(ECO)','B11(ECO)','B16(ECO)'
    )

    original_sub <<-
    "
    SUS =~ K + A + B
    K =~ K_ENV + K_SOC + K_ECO
    A =~ A_ENV + A_SOC + A_ECO
    B =~ B_ENV + B_SOC + B_ECO
    K_ENV  =~  SCQ_1  +  SCQ_2i  +  SCQ_3  +  SCQ_4  +  SCQ_5  +  SCQ_6
    K_SOC  =~  SCQ_7  +  SCQ_8  +  SCQ_9  +  SCQ_10  +  SCQ_11  +  SCQ_12  +  SCQ_13  +  SCQ_14
    K_ECO  =~  SCQ_15  +  SCQ_16  +  SCQ_17  +  SCQ_18 
    A_ENV  =~  SCQ_19i  +  SCQ_20  +  SCQ_21  +  SCQ_22i
    A_SOC  =~  SCQ_23  +  SCQ_24  +  SCQ_25  +  SCQ_26  +  SCQ_27  +  SCQ_28
    A_ECO  =~  SCQ_29  +  SCQ_30  +  SCQ_31  +  SCQ_32 
    B_ENV  =~  SCQ_33  +  SCQ_34  +  SCQ_35  +  SCQ_36  +  SCQ_37i  +  SCQ_38  +  SCQ_39  
    B_SOC  =~  SCQ_40  +  SCQ_41i  +  SCQ_42  +  SCQ_43  +  SCQ_44  +  SCQ_45 
    B_ECO  =~  SCQ_46  +  SCQ_47  +  SCQ_48  +  SCQ_49 
    SCQ_10  ~~  SCQ_11
    SCQ_6  ~~  SCQ_14 
    SCQ_20  ~~  SCQ_26 
    SCQ_19i  ~~  SCQ_22i 
    SCQ_35  ~~  SCQ_38 
    SCQ_40  ~~  SCQ_43 
    SCQ_40  ~~  SCQ_45 
    SCQ_43  ~~  SCQ_45
    "


}

make_val_short<- function(DELLIST=c()){
    ### �ϐ� ###
    K_ENV          <<- lapply(list(c(1,4,6)),setdiff,DELLIST)
    K_SOC          <<- lapply(list(c(8,11,12)),setdiff,DELLIST)
    K_ECO          <<- lapply(list(c(15,16,17)),setdiff,DELLIST)
    names(K_ENV)   <<- c('K_ENV')
    names(K_SOC)   <<- c('K_SOC')
    names(K_ECO)   <<- c('K_ECO')

    A_ENV          <<- lapply(list(c(19,20,21)),setdiff,DELLIST)
    A_SOC          <<- lapply(list(c(23,24,28)),setdiff,DELLIST)
    A_ECO          <<- lapply(list(c(29,30,31)),setdiff,DELLIST)
    names(A_ENV)   <<- c('A_ENV')
    names(A_SOC)   <<- c('A_SOC')
    names(A_ECO)   <<- c('A_ECO')

    B_ENV          <<- lapply(list(c(35,38,39)),setdiff,DELLIST)
    B_SOC          <<- lapply(list(c(40,44,45)),setdiff,DELLIST)
    B_ECO          <<- lapply(list(c(46,47,48)),setdiff,DELLIST)
    names(B_ENV)   <<- c('B_ENV')
    names(B_SOC)   <<- c('B_SOC')
    names(B_ECO)   <<- c('B_ECO')

    K              <<- c(K_ENV, K_SOC, K_ECO)
    A              <<- c(A_ENV, A_SOC, A_ECO)
    B              <<- c(B_ENV, B_SOC, B_ECO)

    ENV            <<- c(K_ENV, A_ENV, B_ENV)
    SOC            <<- c(K_SOC, A_SOC, B_SOC)
    ECO            <<- c(K_ECO, A_ECO, B_ECO)

    ALL            <<- list(K, A, B)
    names(ALL)     <<- c('K', 'A', 'B')


    original_names_short <<- c(
        'K3','K14','K21',
        'K5','K10','K11',
        'K12','K16','K17',
        'A5i','A6','A10',
        'A1','A2','A18',
        'A3','A7','A8',
        'B3','B10','B12',
        'B4','B15','B17',
        'B6','B9','B11'
    )

    original_names_short_rev <<- c(
        'K3(ENV)','K14(ENV)','K21(ENV)',
        'K5(SOC)','K10(SOC)','K11(SOC)',
        'K12(ECO)','K16(ECO)','K17(ECO)',
        'A5i(ENV)','A6(ENV)','A10(ENV)',
        'A1(SOC)','A2(SOC)','A18(SOC)',
        'A3(ECO)','A7(ECO)','A8(ECO)',
        'B3(ENV)','B10(ENV)','B12(ENV)',
        'B4(SOC)','B15(SOC)','B17(SOC)',
        'B6(ECO)','B9(ECO)','B11(ECO)'
    )

    original_sub <<-
    "
    SUS =~ K + A + B
    K =~ K_ENV + K_SOC + K_ECO
    A =~ A_ENV + A_SOC + A_ECO
    B =~ B_ENV + B_SOC + B_ECO
    K_ENV  =~  SCQ_1  +  SCQ_2i  +  SCQ_3  +  SCQ_4  +  SCQ_5  +  SCQ_6
    K_SOC  =~  SCQ_7  +  SCQ_8  +  SCQ_9  +  SCQ_10  +  SCQ_11  +  SCQ_12  +  SCQ_13  +  SCQ_14
    K_ECO  =~  SCQ_15  +  SCQ_16  +  SCQ_17  +  SCQ_18 
    A_ENV  =~  SCQ_19i  +  SCQ_20  +  SCQ_21  +  SCQ_22i
    A_SOC  =~  SCQ_23  +  SCQ_24  +  SCQ_25  +  SCQ_26  +  SCQ_27  +  SCQ_28
    A_ECO  =~  SCQ_29  +  SCQ_30  +  SCQ_31  +  SCQ_32 
    B_ENV  =~  SCQ_33  +  SCQ_34  +  SCQ_35  +  SCQ_36  +  SCQ_37i  +  SCQ_38  +  SCQ_39  
    B_SOC  =~  SCQ_40  +  SCQ_41i  +  SCQ_42  +  SCQ_43  +  SCQ_44  +  SCQ_45 
    B_ECO  =~  SCQ_46  +  SCQ_47  +  SCQ_48  +  SCQ_49 
    SCQ_10  ~~  SCQ_11
    SCQ_6  ~~  SCQ_14 
    SCQ_20  ~~  SCQ_26 
    SCQ_19i  ~~  SCQ_22i 
    SCQ_35  ~~  SCQ_38 
    SCQ_40  ~~  SCQ_43 
    SCQ_40  ~~  SCQ_45 
    SCQ_43  ~~  SCQ_45
    "

original_names <<- c(
    'KENV3','KENV4i','KENV7','KENV14','KENV18','KENV21',
    'KSOC2','KSOC5','KSOC8','KSOC9','KSOC10','KSOC11','KSOC15','KSOC20',
    'KECO12','KECO16','KECO17','KECO19',
    'AENV5i','AENV6','AENV10','AENV19i',
    'ASOC1','ASOC2','ASOC11','ASOC13','ASOC14','ASOC18',
    'AECO3','AECO7','AECO8','AECO16',
    'BENV1','BENV2','BENV3','BENV7','BENV8i','BENV10','BENV12',
    'BSOC4','BSOC5i','BSOC13','BSOC14','BSOC15','BSOC17',
    'BECO6','BECO9','BECO11','BECO16'
    )

    original_names_rev <<- c(
    'K3(ENV)','K4i(ENV)','K7(ENV)','K14(ENV)','K18(ENV)','K21(ENV)',
    'K2(SOC)','K5(SOC)','K8(SOC)','K9(SOC)','K10(SOC)','K11(SOC)','K15(SOC)','K20(SOC)',
    'K12(ECO)','K16(ECO)','K17(ECO)','K19(ECO)',
    'A5i(ENV)','A6(ENV)','A10(ENV)','A19i(ENV)',
    'A1(SOC)','A2(SOC)','A11(SOC)','A13(SOC)','A14(SOC)','A18(SOC)',
    'A3(ECO)','A7(ECO)','A8(ECO)','A16(ECO)',
    'B1(ENV)','B2(ENV)','B3(ENV)','B7(ENV)','B8i(ENV)','B10(ENV)','B12(ENV)',
    'B4(SOC)','B5i(SOC)','B13(SOC)','B14(SOC)','B15(SOC)','B17(SOC)',
    'B6(ECO)','B9(ECO)','B11(ECO)','B16(ECO)'
    )

    original_sub <<-
    "
    SUS =~ K + A + B
    K =~ K_ENV + K_SOC + K_ECO
    A =~ A_ENV + A_SOC + A_ECO
    B =~ B_ENV + B_SOC + B_ECO
    K_ENV  =~  SCQ_1  +  SCQ_2i  +  SCQ_3  +  SCQ_4  +  SCQ_5  +  SCQ_6
    K_SOC  =~  SCQ_7  +  SCQ_8  +  SCQ_9  +  SCQ_10  +  SCQ_11  +  SCQ_12  +  SCQ_13  +  SCQ_14
    K_ECO  =~  SCQ_15  +  SCQ_16  +  SCQ_17  +  SCQ_18 
    A_ENV  =~  SCQ_19i  +  SCQ_20  +  SCQ_21  +  SCQ_22i
    A_SOC  =~  SCQ_23  +  SCQ_24  +  SCQ_25  +  SCQ_26  +  SCQ_27  +  SCQ_28
    A_ECO  =~  SCQ_29  +  SCQ_30  +  SCQ_31  +  SCQ_32 
    B_ENV  =~  SCQ_33  +  SCQ_34  +  SCQ_35  +  SCQ_36  +  SCQ_37i  +  SCQ_38  +  SCQ_39  
    B_SOC  =~  SCQ_40  +  SCQ_41i  +  SCQ_42  +  SCQ_43  +  SCQ_44  +  SCQ_45 
    B_ECO  =~  SCQ_46  +  SCQ_47  +  SCQ_48  +  SCQ_49 
    SCQ_10  ~~  SCQ_11
    SCQ_6  ~~  SCQ_14 
    SCQ_20  ~~  SCQ_26 
    SCQ_19i  ~~  SCQ_22i 
    SCQ_35  ~~  SCQ_38 
    SCQ_40  ~~  SCQ_43 
    SCQ_40  ~~  SCQ_45 
    SCQ_43  ~~  SCQ_45
    "
}

make_val_japanese <- function(){
    make_val_short(c(46,47))
    ALL$B$B_ECO <- c(48,49)
    B$B_ECO <- c(48,49)
    ECO$B_ECO <- c(48,49)
}

# error_check
check_model <- function(model_data,fit){
    ### �G���[�̌��� ###
    # �����U�s��
    print('---------covariances of latent variables---------')
    print(lavInspect(fit, "cov.lv"))
    #print('---------���ݕϐ��̋����U�s��---------')
    #print(fitted(fit))
    # �W�����c���i�ϑ����ꂽ�����U�Ƌ����U�̍��j
    cov <- resid(fit)$cov
    standcov <- resid(fit, type="standardized")$cov
    print('---------residual errors (>1.96)---------')
    as.tibble(rownames_to_column(round(data.frame(standcov),3),'question')) %>%
        gather(c(1:ncol(model_data)+1),key='front',value='cov') %>%
        arrange(-cov) %>%
        filter(cov > 1.96) %>%
        filter(row_number() %% 2 == 0) %>%
        print(n=100)

    # �C���w�W
    MI <- modificationIndices(fit)
    # mi�����ŕ\��
    print('---------modification indices (>3.84)---------')
    print(subset(MI[order(MI$mi,decreasing = TRUE),], mi>=3.84))
    subset(MI[order(MI$lhs,decreasing = TRUE),], mi>=3.84)

    # ���d���ϐ�
    # note: ���ݕϐ����m�̑���
    print('---------correlations between observed variables---------')
    print(inspect(fit, what="cor.ov"))
    print('---------correlations between latent variables (>.50)---------')
    cor_ov <- inspect(fit, what="cor.ov") # ���ւ���������ϐ��͂Ȃ���
    as.tibble(rownames_to_column(round(data.frame(cor_ov),3),'question')) %>%
        gather(c(1:ncol(model_data)+1),key='front',value='cor') %>%
        arrange(-cor) %>%
        filter(cor < 1 & cor > 0.50 ) %>%
        filter(row_number() %% 2 == 0) %>%
        print(n=100)
    #inspect(fit, what="cor.all")

    # �덷����
    # note: https://qiita.com/Masahiro_T/items/c9d5d86a0653d407f5fb
    cor <- resid(fit, type="cor")$cov
    print('---------residual correlation ---------')
    as.tibble(rownames_to_column(abs(round(data.frame(cor),2)),'question')) %>%
        gather(c(1:ncol(model_data)+1),key='front',value='cor') %>%
        arrange(-cor) %>%
        filter(row_number() %% 2 == 0) %>%
        print(n=60)

    ### �O��l ###
    #gct <- genCookDist(model,model_data)
    #plot(gCt,pch=19,xlab="observations",ylab="Cook distance")
    # �Q�l
    #model <- "F1 =~ x1+x2+x3 \n F2 =~ y1+y2+y3+y4 \n F3 =~ y5+y6+y7+y8"
    #data('PDII')
    #fit0 <- sem(model, data=PDII)
    #gCD <- genCookDist(model,data=PDII)
    #plot(gCD,pch=19,xlab="observations",#ylab="Cook distance")
}

cal_demo <- function(LOCATION){

    INPUTDATA <- read.csv(LOCATION)

    # data
    agevec            <- INPUTDATA[,ncol(INPUTDATA)-1]
    agevec_60         <- subset(agevec,agevec >= 60)
    agevec_50         <- subset(agevec, (agevec >= 50 & agevec < 60))
    agevec_40         <- subset(agevec, (agevec >= 40 & agevec < 50))
    agevec_30         <- subset(agevec, (agevec >= 30 & agevec < 40))
    agevec_20         <- subset(agevec, (agevec >= 20 & agevec < 30))
    agevec_10         <- subset(agevec, (agevec >= 10 & agevec < 20))  

    sexvec            <- INPUTDATA[,ncol(INPUTDATA)]
    sexvec_m          <- subset(sexvec,sexvec==1)
    sexvec_f          <- subset(sexvec,sexvec==2)        

    ## subject num
    Nsub              <- nrow(INPUTDATA)
    Nsub_10           <- length(agevec_10)
    Nsub_20           <- length(agevec_20)
    Nsub_30           <- length(agevec_30)
    Nsub_40           <- length(agevec_40)
    Nsub_50           <- length(agevec_50)
    Nsub_60           <- length(agevec_60)
    Nsub_m            <- length(sexvec_m)
    Nsub_f            <- length(sexvec_f)
    Nsub_o            <- length(sexvec) - Nsub_m - Nsub_f

    ## meanage, maxage, minage
    mean_age          <- mean(agevec)
    sd_age            <- sd(agevec)
    max_age           <- max(agevec)
    min_age           <- min(agevec)

    output <- list(
        subject = data.frame(Nsub,Nsub_10,Nsub_20,Nsub_30,Nsub_40,Nsub_50,Nsub_60,Nsub_m,Nsub_f,Nsub_o),
        age     = data.frame(mean_age,sd_age,max_age,min_age)
    )
    return(output)
}

make_fig <- function(option='width=\\linewidth',location=NULL,caption=NULL){
    return(
        paste('\\begin{figure}[tbp]
            \\begin{center}
            \\includegraphics[', option, ']{',location,'}
            \\caption{',caption,'}
            \\end{center}
            \\end{figure}',
            sep = ''
            )
    )
}
