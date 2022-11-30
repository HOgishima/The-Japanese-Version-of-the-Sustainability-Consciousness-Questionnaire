source("preamble_study1.r")

make_val_japanese()
DF             <- model_data[unlist(ALL)]
names(DF)      <- original_names_rev[unlist(ALL)]

### ���q�� ###
# �K�b�g�}���
vss(DF,n=10,rotate="oblimin")

correlation = cor(DF, use = "complete.obs" ) # ���֍s����v�Z
fa.parallel(correlation, n.obs = nrow(DF), fa = "fa" ) # �X�N���[�v���b�g��\��
abline(h = 0) # y = 0 �̉�����ǉ�
result <- fa.parallel(correlation, n.obs = nrow(DF), fa = "fa" )
plotdata <- data.frame(factor_number = c(1:length(result$fa.values)),
    eigen_value = result$fa.values)

screeplot <- ggplot(plotdata, aes(x = factor_number, y = eigen_value),
    #cex      = 2.5,     #  �L���̑傫����ݒ肷��i�W���͂P�j
    cex.lab  = 2,       #  ���̐����̎��̑傫����ݒ肷��
    cex.axis = 1.8,      #  ���̐������i���x���j�̑傫����ݒ肷��
    cex.main = 1.8) +      #  ���C���^�C�g���̎��̑傫����ݒ肷��) 
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