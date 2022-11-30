source("preamble_study1.r")
source("make_parceldata.r")

# 2ˆöqŠK‘wƒ‚ƒfƒ‹
modeltxt <- paste(
    'SC =~ KA + B',
    'KA   =~ K_ENV_P + K_SOC_P + K_ECO_P + A_ENV_P + A_SOC_P + A_ECO_P',
    'B   =~ B_ENV_P + B_SOC_P + B_ECO_P',
    'K_ENV_P ~~ B_ENV_P', 'K_ENV_P ~~ A_ENV_P', 'A_ENV_P ~~ B_ENV_P',
    'K_SOC_P ~~ B_SOC_P', 'K_SOC_P ~~ A_SOC_P', 'A_SOC_P ~~ B_SOC_P',
    'K_ECO_P ~~ B_ECO_P', 'K_ECO_P ~~ A_ECO_P', 'A_ECO_P ~~ B_ECO_P',     
    sep = '\n '
)
model <- paste(modeltxt, sep = '\n ')
cat(model)

fit <- lavaan::cfa(model,data=parcel_data,estimator='MLR')
summary(fit, standardized=TRUE)
layered_two_factor <- fitMeasures(fit,c('RMSEA','CFI','TLI'))

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
    split(., f = df_table$lhs_new)


table_layered_two_factor_p <- apa_table(
  table_list
  , format = 'latex'
  , align = c("l", rep("c", 3))
  , digits = 3
  , caption = 'Factor loading of each item for Two-factor hierarchical parceled model.'
  , note = 'P in each factor item indicates that the corresponding data were parceled. 
  \\textit{SE} = Standard Error, \\textit{P} = P value. 
    KA = knowingness/attitudes; B = behaviour; ECO = economic; SOC = social; ENV = environmental; SC = sustainability consciousness.'
  , col.names = c('Factor/Item','Factor Loading','\\textit{SE}','\\textit{P}')
  , escape = FALSE
)