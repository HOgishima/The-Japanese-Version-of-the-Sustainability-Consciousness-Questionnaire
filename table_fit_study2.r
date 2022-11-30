source("cfa_one_all.r")
source("cfa_two_all.r")
source("cfa_three_all.r")
source("cfa_two_layered_all.r")
source("cfa_three_layered_all.r")

df <- rbind(
    '(1) One-factor model'   = cfa_one_all,
    '(2) Two-factor model'   = cfa_two_all,
    '(3) Three-factor model' = cfa_three_all,
    '(4) Two-factor hierarchical model'    =  layered_two_factor_all,
    '(5) Three-factor hierarchical model' = layered_three_factor_all
)

fit_table2 <- apa_table(
    df,
    format = 'latex',
    align = c('l','c','c','c','c'),
    caption = 'Summary of model fitness in study 2',
    note = 'There is a slight difference between the results of the three-factor model and the three-factor hierarchical model after the decimal point.',
    digits = 3,
    col.names = c('Model','BIC','RMSEA','CFI','TLI')   
)

cat(fit_table2)
