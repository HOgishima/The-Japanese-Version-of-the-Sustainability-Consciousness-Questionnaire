source("packages.r")
source("make_cor_dataset.R")

# descriptive
INPUT_DATA <- DESDATASET
df_mean       <- t(summarize_all(INPUT_DATA,funs(round(mean(., na.rm=TRUE),2))))
df_sd         <- t(summarize_all(INPUT_DATA,funs(round(sd(., na.rm=TRUE),2))))
df_skewness   <- t(summarize_all(INPUT_DATA,funs(round(skewness(., na.rm=TRUE),2))))# ˜c“x
df_kurtosis   <- t(summarize_all(INPUT_DATA,funs(round(kurtosis(., na.rm=TRUE),2))))# ˜c“x


des_table3 <- apa_table(
    data.frame(df_mean,df_sd,df_skewness,df_kurtosis),
    align = c('l','S[table-format=3.3]','S[table-format=2.3]','S[table-format=+1.3]','S[table-format=+1.3]'),
    format = 'latex',
    caption = 'Descriptive statistics for the scales in study 3.',
    digits = 3,
    col.names = c('Scales','\\textit{Mean}','\\textit{SD}','\\textit{Skewness}','\\textit{Kurtosis}'),
    stub_indents = list('Susutainability Consciousness Questionnaire'=c(1:3), 'Climate Change Risk Perception Model'=c(4:6), 'Interpersonal Reactivity Index' = c(7:7), 'SDO$_{6}$ Scale'=c(8:8), 'Ten Items Personality Inventory'=c(9:13), 'Loyola Generativity Scale'=c(14:14)),
)

    #note = 'SCQ = the Susutainability Consciousness Questionnaire; CCRPM = The climate change risk perception model; SDO = SDO Scale; IRI = the Interpersonal Reactivity Index; LGS = the Loyola Generativity Scale.',
