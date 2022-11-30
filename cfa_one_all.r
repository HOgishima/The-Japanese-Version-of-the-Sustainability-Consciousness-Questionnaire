source("preamble_study2.r")

make_val_japanese()

# 1??
modeltxt <- paste(
    model_txt_unlist("ALL"),
    cor_txt_from_list(ENV),
    cor_txt_from_list(SOC),
    cor_txt_from_list(ECO), 
    sep = '\n '
)
model <- paste(modeltxt, sep = '\n ')
cat(model)

fit <- lavaan::cfa(model,data=model_data,estimator='MLR')
summary(fit, standardized=TRUE)
cfa_one_all <- fitMeasures(fit,c('BIC','RMSEA','CFI','TLI'))

### make table ###
df_table <- standardizedSolution(fit) %>%
    filter(., op == '=~') %>%
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
names(table_list)[1] <- c('SC')

table_cfa_one_all <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 3))
  , digits = 3
  , caption = "Factor loading of each item for the one-factor model."
  , note = '\\textit{SE} = Standard Error, \\textit{P} = P value. SC = sustainability consciousness.'
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}')
  , escape = FALSE
)