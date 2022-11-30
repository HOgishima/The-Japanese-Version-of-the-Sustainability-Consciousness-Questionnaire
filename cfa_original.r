source("preamble_study1.r")

make_val_short()

### MODEL ###
# error correlations
gosa <- paste(
    cor_txt_all(x = c(35,40), y = c(38,45)),
    sep = '\n '
)
# original model
modeltxt <- paste(
    model_txt_higher('ALL'),
    model_txt_higher('K'),
    model_txt_higher('A'),
    model_txt_higher('B'),
    model_txt(ALL),
    sep = '\n '
)
# model structure
model <- paste(modeltxt, gosa,sep = '\n ')
cat(model)

### result ###
fit <- lavaan::cfa(model,data=model_data,estimator='MLR')
summary(fit, standardized=TRUE)
# fitness
fm <- fitMeasures(fit,c('RMSEA','CFI','TLI')) # ‘I‘ð

### make table ###
df_table <- standardizedSolution(fit) %>%
    filter(., op == '=~') %>%
    filter(., str_detect(rhs, "SCQ")) %>%
    mutate(., rhs_new = gsub(rhs, pattern='_', replacement='\\\\_')) %>%
    mutate(., lhs_new = gsub(lhs, pattern='_', replacement='\\\\_'))
    
df_table$lhs_new <- factor(df_table$lhs_new, levels=unique(df_table$lhs_new))

table_list <- df_table %>%
    column_to_rownames(., var = 'rhs_new') %>%
    select(., c(est.std,se,pvalue)) %>%
    rename_all(vars(c('Factor Loading','\\textit{SE}','\\textit{P}'))) %>%
        split(., f = df_table$lhs_new)


table_cfa_original <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 3))
  , digits = 3
  , caption = "Factor loading of each item for nine subfactors."
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}')
  , note = '\\textit{SE} = Standard Error; \\textit{P} = P value. K = knowingness; A = attitudes; B = behaviour; ECO = economic;
SOC = social; ENV = environmental.'
  , escape = FALSE
)

# cor matrix
cor_matrix <- data.frame(format(round(inspect(fit,'cor.lv'),3),nsmall=3)) %>%
    mutate_if(is.numeric, as.character)

rownames(cor_matrix)[1] <- "SC"
colnames(cor_matrix)[1] <- "SC" 

cor_matrix[upper.tri(cor_matrix,diag=TRUE)] <- ''
#knitr::kable(cor_matrix)
cfa_original_cormatrix <- apa_table(
    cor_matrix,
    format = 'latex',
    digits = 3,
    caption = 'Factor correlation matrix of the original model.',
    note = 'SC = sustainability consciousness; K = knowingness; A = attitudes; B = behaviour; ECO = economic;
SOC = social; ENV = environmental.'
)