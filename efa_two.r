source("preamble_study1.r")

make_val_japanese()
DF             <- model_data[unlist(ALL)]
names(DF)      <- original_names_rev[unlist(ALL)]

factor_num  = 2

result      = fa(DF, nfactors = factor_num, fm = "ml", rotate = "oblimin", use = "complete.obs")
print(result, digits = 3, sort = T)

result_loadings <- as.data.frame(unclass(result$loadings))
df_loadings <- result_loadings %>%
  mutate(X_max = pmap(select(., 1:ncol(.)), ~ pmax(...))) %>%
  mutate_at(vars(X_max), as.numeric) %>%
  mutate(ML1orNot = ML1 == X_max) %>%
  rownames_to_column(var = "Item") %>%
  group_by(ML1orNot) %>%
  arrange(-ML1orNot,-X_max) %>%
  select(Item,ML1,ML2)

section_num <- sum(df_loadings$ML1orNot)

efa_two <- apa_table(
    df_loadings[,c('Item','ML1','ML2')],
    digits = 3,
    align = c('l','r','r'),
    format = 'latex',
    caption = 'Rotated component matrix for two factor model',
    note = 'The Item code indicates the item number of the original version. 
    See Table 1 for the correspondence with the items in the Japanese version. 
    "*" indicates the item of the short version of SCQ, and "i" indicates that the item is a reversed item. 
    The characters in parentheses, i.e., ENV, SOC, ECO, indicates the factor to which the item belongs. K = knowingness; A = attitudes; B = behaviour; ECO = economic; SOC = social; ENV = environmental.',
    midrules = c(section_num),
    col.names = c('Item Code','Factor 1','Factor 2')
)