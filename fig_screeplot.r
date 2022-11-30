source("preamble_study1.r")

make_val_japanese()
DF             <- model_data[unlist(ALL)]
names(DF)      <- original_names_rev[unlist(ALL)]

### 因子数 ###
# ガットマン基準
vss(DF,n=10,rotate="oblimin")

correlation = cor(DF, use = "complete.obs" ) # 相関行列を計算
fa.parallel(correlation, n.obs = nrow(DF), fa = "fa" ) # スクリープロットを表示
abline(h = 0) # y = 0 の横線を追加
result <- fa.parallel(correlation, n.obs = nrow(DF), fa = "fa" )
plotdata <- data.frame(factor_number = c(1:length(result$fa.values)),
    eigen_value = result$fa.values)

screeplot <- ggplot(plotdata, aes(x = factor_number, y = eigen_value),
    #cex      = 2.5,     #  記号の大きさを設定する（標準は１）
    cex.lab  = 2,       #  軸の説明の字の大きさを設定する
    cex.axis = 1.8,      #  軸の数字等（ラベル）の大きさを設定する
    cex.main = 1.8) +      #  メインタイトルの字の大きさを設定する) 
    geom_line(size = 1) +
    scale_x_continuous(breaks=c(0:nrow(plotdata))) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
    geom_point(size = 3) +
    xlab("Factor number") +
    ylab('Eigenvalues of principal factors') +
    theme_bw(base_size = 13)
ggsave("./material/screeplot.png", dpi = 400)

fig_screeplot <- make_fig(
    location = 'screeplot.png',
    caption = 'Scree plot of eigenvalues in exploratory factor analysis'
    )