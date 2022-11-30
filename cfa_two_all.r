source("preamble_study2.r")
source("reliability.r")

make_val_japanese()

# 2ˆöŽq
modeltxt <- paste(
    "KA  =~  SCQ_1 + SCQ_4 + SCQ_6 + SCQ_8 + SCQ_11 + SCQ_12 + SCQ_15 + SCQ_16 + SCQ_17 +
    SCQ_19i + SCQ_20 + SCQ_21 + SCQ_23 + SCQ_24 + SCQ_28 + SCQ_29 + SCQ_30 + SCQ_31",
    model_txt_unlist("B"),
    cor_txt_from_list(ENV),
    cor_txt_from_list(SOC),
    cor_txt_from_list(ECO),     
    sep = '\n '
)
#modeltxt <- paste(
#    "KA  =~  SCQ_1 + SCQ_4 + SCQ_6 + SCQ_8 + SCQ_11 + SCQ_12 + SCQ_15 + SCQ_16 + SCQ_17 +
#    SCQ_19i + SCQ_20 + SCQ_21 + SCQ_23 + SCQ_24 + SCQ_28 + SCQ_29 + SCQ_30 + SCQ_31",
#    model_txt_unlist("B"),
#    cor_txt_all(x = c(35,40), y = c(38,45)),
#    sep = '\n '
#)
model <- paste(modeltxt, sep = '\n ')
cat(model)


fit <- lavaan::cfa(model,data=model_data,estimator='MLR')
summary(fit, standardized=TRUE)
cfa_two_all <- fitMeasures(fit,c('BIC','RMSEA','CFI','TLI'))

mutate_all(model_data, mean)

### make table ###
df_mean          <- t(summarize_all(model_data,funs(mean(., na.rm=TRUE))))
df_mean[INVERSE] <- as.numeric(c(purrr::map_chr(df_mean[,1],function(x){6-x})))[INVERSE]
df_mean          <- data.frame(mean = df_mean[unlist(ALL)])
df_sd            <- data.frame(sd = t(summarize_all(model_data[unlist(ALL)],funs(sd(., na.rm=TRUE)))))

df_table <- standardizedSolution(fit) %>%
    filter(., op == '=~')%>%
    #filter(., str_detect(rhs, "SCQ")) %>%
    mutate(., rhs_new = gsub(rhs, pattern='_', replacement='\\\\_')) %>%
    mutate(., lhs_new = gsub(lhs, pattern='_', replacement='\\\\_')) %>%
    mutate(mean = df_mean) %>%
    mutate(sd = df_sd)
    #modify_if(., ~is.numeric(.), ~round(., 4)) %>%
    #group_by(lhs)

#df_table <- df_fit
df_table$lhs_new <- factor(df_table$lhs_new, levels=unique(df_table$lhs_new))
rownames(df_table) <- df_table$rhs_new

table_list <- df_table %>%
    select(., c(est.std,se,pvalue,mean,sd)) %>%
    rename_all(vars(c('Factor Loading','\\textit{SE}','\\textit{P}','mean','sd'))) %>%
    split(., f = df_table$lhs_new)

fac1 <- paste('Knowingness/Attitude ($\\alpha$ = ', round(KA_alpha,3), ', $\\omega$ = ', round(KA_omega,3), ')', sep='')
fac2 <- paste('Behaviour ($\\alpha$ = ', round(B_alpha,3), ', $\\omega$ = ', round(B_omega,3), ')', sep='')
names(table_list) <- c(fac1, fac2)

table_cfa_two_all <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 5))
  , digits = 3
  , caption = "Factor loading of each item for the two-factor model."
  , note = '\\textit{SE} = Standard Error, \\textit{P} = P value, \\textit{Mean} = Mean item score, \\textit{SD} = Standard Deviation of the item score.  
  "i" indicates that the item is a reversed item.'
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}','\\textit{Mean}','\\textit{SD}')
  , escape = FALSE
)

cat(table_cfa_two_all)

# cor matrix
cor_matrix_two <- data.frame(round(inspect(fit,'cor.lv'),3)) %>%
    mutate_if(is.numeric, as.character)

cor_matrix[upper.tri(cor_matrix,diag=TRUE)] <- ''
#knitr::kable(cor_matrix)

cfa_two_all_cormatrix <- apa_table(
    cor_matrix_two,
    format = 'latex',
    digits = 3
)