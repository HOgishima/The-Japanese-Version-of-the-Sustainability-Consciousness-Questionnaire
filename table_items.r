source("packages.r")
source("function_scq.R")

make_val_japanese()

df_table <- data.frame('Item Number' = read.csv('./data/scq_items_rev.csv')[,c(1)],
    'Item Code' = original_names_rev,
    read.csv('./data/scq_items_rev.csv')[,c(4,2)]
    )

df_table[unlist(ALL),1] <- c(purrr::map_chr(df_table[unlist(ALL),1],function(x){paste(x,'*', sep = "")}))
make_val_short()
df_table[unlist(ALL),2] <- c(purrr::map_chr(df_table[unlist(ALL),2],function(x){paste(x,'*', sep = "")}))

table_items <- paste(kable(
    df_table
    , longtable = T
    , format = "latex"
    , align = c("c",'c','p{6cm}','p{6cm}')
    , caption = "Item descriptions of the sustainability consciousness questionnaire."
    , booktabs = T
    , col.names = c('Item Number', 'Item Code', 'Japanese', 'Backtranslation from Japanese')
    )  %>%
    kable_styling(
        latex_options = c("repeat_header")
        , repeat_header_continued = F #r"(\rightline{\small\it Continues})"
        )
, r'(\noindent\textit{Note.} All items in the long version of SCQ (SCQ-L) were translated. The items selected finally for the Japanese version are marked with * after each item number. 
	The Item code indicates the item number of the original version. 
    "*" indicates the item of SCQ-S, and "i" indicates that the item is a reversed item. The characters in parentheses, i.e., ENV, SOC, ECO, indicate the factor to which the item belongs. 
    K = knowingness; A = attitudes; B = behaviour; ECO = economic; SOC = social; ENV = environmental.)'
, sep = '\n'
)

