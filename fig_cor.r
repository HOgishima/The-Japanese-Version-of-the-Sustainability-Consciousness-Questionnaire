source("packages.r")
source("make_cor_dataset.R")

DESDATASET1 <- DESDATASET[,c(2,3,4,5,6)]
DESDATASET2 <- DESDATASET[,-c(2,3,4,5,6)]
colnames(DESDATASET1)[3:5] <- c('Impact Knowledege about CC','Holistic Affect about CC','Pro-environmental Behaviour')
colnames(DESDATASET2)[1] <- 'SCQ'
result1     <- cor(DESDATASET1, use = "pairwise.complete.obs")
result2     <- cor(DESDATASET2, use = "pairwise.complete.obs")
pvalue1     <- corr.test(DESDATASET1)$p
pvalue2     <- corr.test(DESDATASET2)$p
#cor.plot(result, numbers=T)
# https://satoshilab.blogspot.com/2015/01/corrplot.html
# https://stackoverflow.com/questions/29709204/reporting-significance-level-in-corrplot
#png("./material/corplot1.png",width = 600, height = 450)
pdf("./material/corplot1.pdf",width = 10, height = 7.5)
#par(mar = c(100, 100, 100, 100))   #下、左、上、右の順
#par(oma = c(10, 10, 10, 10))   #下、左、上、右の順
corrplot(result1, mar = c(0, 3, 0, 0),type = 'upper', tl.col="white",tl.cex=1.0, tl.srt=45,addCoef.col='black',number.digits = 3)
corrplot(result1, type = 'lower', add = T, tl.col="black",tl.cex=1.0,tl.srt=45, tl.pos="b", p.mat = pvalue1, insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05))
#, p.mat = pvalue, ,insig="p-value", sig.level=-1
dev.off() 

fig_cor1 <- make_fig(
    location = 'corplot1.pdf',
    caption = r'(Correlation matrix between the subfactors of the sustainability consciousness questionnaire (SCQ) and other scales. 
    Correlation coefficients are shown in the upper triangular matrix and significance are shown in the lower triangular matrix. 
    The color spectrum indicates the magnitude of the correlation coefficient. 
    CC = climate change. 
    ***\textit{p} $<$ 0.001, **\textit{p} $<$ 0.01, *\textit{p} $<$ 0.05.)'
    )

#png("./material/corplot2.png", width = 500, height = 500)
pdf("./material/corplot2.pdf", width = 8, height = 8)
corrplot(result2, mar = c(0, 9, 1, 0), type = 'upper', tl.col="white",tl.cex=0.7, tl.srt=45, tl.pos="b", addCoef.col='black',number.digits = 3,number.cex = 0.8)
corrplot(result2, type = 'lower', add = T, tl.col="black",tl.cex=0.7,tl.srt=45, tl.pos="b", p.mat = pvalue2, insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05),pch.cex = 2)
#, p.mat = pvalue, ,insig="p-value", sig.level=-1
dev.off() 

fig_cor2<- make_fig(
    location = 'corplot2.pdf',
    caption = r'(Correlation matrix between the total score of the sustainability consciousness questionnaire (SCQ) and Climate Change Risk Perception Model scales. 
    Correlation coefficients are shown in the upper triangular matrix and significances are shown in the lower triangular matrix. 
    The color spectrum indicates the magnitude of the correlation coefficient. 
    ***\textit{p} $<$ 0.001, **\textit{p} $<$ 0.01, *\textit{p} $<$ 0.05.)'
    )