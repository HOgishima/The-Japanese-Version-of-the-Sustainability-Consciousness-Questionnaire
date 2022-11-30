source("preamble_study2.r")

make_val_japanese()

# 2???????
modeltxt <- paste(
    "SC =~ KA + B",
    'KA  =~  SCQ_1 + SCQ_4 + SCQ_6 + SCQ_8 + SCQ_11 + SCQ_12 + SCQ_15 + SCQ_16 + SCQ_17 + SCQ_19i + SCQ_20 + SCQ_21 + SCQ_23 + SCQ_24 + SCQ_28 + SCQ_29 + SCQ_30 + SCQ_31',
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
layered_two_factor_all <- fitMeasures(fit,c('BIC','RMSEA','CFI','TLI'))

### make table ###
df_table <- standardizedSolution(fit) %>%
    filter(., op == '=~')%>%
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
    split(., f = df_table$lhs)


table_layered_two_factor_all <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 3))
  , digits = 3
  , caption = "Factor loading of each item for the two-factor hierarchical model."
  , note = '\\textit{SE} = Standard Error, \\textit{P} = P value. 
  SC = sustainability consciousness; KA = knowingness/attitudes; B = behaviour.'
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}')
  , escape = FALSE
)