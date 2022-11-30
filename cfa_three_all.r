source("preamble_study2.r")

make_val_japanese()

modeltxt <- paste(
    model_txt_unlist('K'),
    model_txt_unlist('A'),
    model_txt_unlist("B"),
    cor_txt_from_list(ENV),
    cor_txt_from_list(SOC),
    cor_txt_from_list(ECO),     
    sep = '\n '
)
model <- paste(modeltxt, sep = '\n ')
cat(model)

fit <- lavaan::cfa(model,data=model_data,estimator='MLR')
summary(fit, standardized=TRUE)
cfa_three_all <- fitMeasures(fit,c('BIC','RMSEA','CFI','TLI'))

### make table ###
df_table <- standardizedSolution(fit) %>%
    filter(., op == '=~')%>%
    filter(., str_detect(rhs, "SCQ")) %>%
    mutate(., rhs_new = gsub(rhs, pattern='_', replacement='\\\\_')) %>%
    mutate(., lhs_new = gsub(lhs, pattern='_', replacement='\\\\_'))
    #modify_if(., ~is.numeric(.), ~round(., 4)) %>%
    #group_by(lhs)

#df_table <- df_fit
df_table$lhs_new <- factor(df_table$lhs_new, levels=unique(df_table$lhs_new))
rownames(df_table) <- df_table$rhs_new

table_list <- df_table %>%
    select(., c(est.std,se,pvalue)) %>%
    rename_all(vars(c('Factor Loading','\\textit{SE}','\\textit{P}'))) %>%
    split(., f = df_table$lhs_new)

table_cfa_three_all <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 3))
  , digits = 3
  , caption = "Factor loading of each item for the three-factor model."
  , note = '\\textit{SE} = Standard Error, \\textit{P} = P value. 
  K = knowingness; A = attitudes; B = behaviour; ECO = economic.'
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}') 
  , escape = FALSE
)

# cor matrix
cor_matrix_three <- data.frame(round(inspect(fit,'cor.lv'),3)) %>%
    mutate_if(is.numeric, as.character)

cor_matrix[upper.tri(cor_matrix,diag=TRUE)] <- ''
#knitr::kable(cor_matrix)

cfa_three_all_cormatrix <- apa_table(
    cor_matrix_three,
    align = c('c','S[table-format=1.3]','S[table-format=1.3]','S[table-format=1.3]'),
    format = 'latex',
    caption = 'Factor correlation matrix of the three-factor model.',
    note = 'K = knowingness; A = attitudes; B = behaviour.',
    digits = 3
)