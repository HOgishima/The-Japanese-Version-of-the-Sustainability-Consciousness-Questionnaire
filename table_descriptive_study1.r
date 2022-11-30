source("preamble_study1.r")
model_data     <- read.csv('./data/data_outlier_1st.csv')

make_val()
df_mean <- model_data[,unlist(ALL)] %>%
    summarize_all(mean)
df_sd <- model_data[,unlist(ALL)] %>%
    summarize_all(sd)
df_kurtosis <- model_data[,unlist(ALL)] %>%
    summarize_all(e1071::kurtosis)
df_skewness <- model_data[,unlist(ALL)] %>%
    summarize_all(e1071::skewness)

des_table <- data.frame(t(rbind('itemcode'=original_names_rev,'mean'=df_mean,'sd'=df_sd,'kurtosis'=df_kurtosis,'skewness'=df_skewness))) %>%
    mutate_at(vars(-itemcode), as.numeric) 

des_table[,c('mean')][INVERSE] <- as.numeric(c(purrr::map_chr(des_table[,c('mean')][INVERSE],function(x){6-x})))

make_val_short()
des_table[,c('itemcode')][unlist(ALL)] <- c(purrr::map_chr(des_table[,c('itemcode')][unlist(ALL)],function(x){paste(x,'*', sep = "")})) # add * for reversal item

make_val_japanese()
rownames(des_table)[unlist(ALL)] <- c(purrr::map_chr(rownames(des_table)[unlist(ALL)],function(x){paste(x,'*', sep = "")})) # add * for reversal item

table_descriptive1 <- apa_table(
    des_table,
    align = c('l','l','S[table-format=1.3]','S[table-format=1.3]','S[table-format=+1.3]','S[table-format=+1.3]'),
    format = 'latex',
    caption = 'Descriptive statistics for items of the sustainability consciousness questionnare in study 1.',
    note = 'The survey in study 1 required responses to all items of the long version of SCQ. The items selected finally for the Japanese version are marked with * after each item number.
	The Item code indicates the item number of the original version. "*" indicates the item of the short version of SCQ, and "i" indicates that the item is a reversed item. The characters in parentheses, i.e., ENV, SOC, ECO, indicates the factor to which the item belongs. K = knowingness; A = attitudes; B = behaviour; ECO = economic; SOC = social; ENV = environmental.',
    digits = 3,
    col.names = c('Item Number','Item Code','\\textit{Mean}','\\textit{SD}','\\textit{Skewness}','\\textit{Kurtosis}')      
)

table_descriptive1 <- str_replace(
    table_descriptive1
    , pattern = c('\\\\begin\\{threeparttable\\}','\\\\end\\{threeparttable\\}')
    , replacement = c('\\\\scalebox\\{0.7\\}\\{ \n \\\\begin\\{threeparttable\\}','\\\\end\\{threeparttable\\} \n \\}')
)