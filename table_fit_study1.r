source("cfa_one_p.r")
source("cfa_two_p.r")
source("cfa_three_p.r")
source("cfa_two_layered_p.r")
source("cfa_three_layered_p.r")

df <- rbind(
    '(1) One-factor model'   = cfa_one_p,
    '(2) Two-factor model'   = cfa_two_p,
    '(3) Three-factor model' = cfa_three_p,
    '(4) Two-factor hierarchical model'    =  layered_two_factor,
    '(5) Three-factor hierarchical model' = layered_three_factor
)

fit_table <- apa_table(
    df,
    format = 'latex',
    align = c('l','c','c','c'),
    caption = 'Summary of model fitness in study 2',
    note = 'There is a slight difference between the results of the three-factor model and the three-factor hierarchical model after the decimal point.',
    digits = 3,
    col.names = c('Model','RMSEA','CFI','TLI')   
)

cat(fit_table)
