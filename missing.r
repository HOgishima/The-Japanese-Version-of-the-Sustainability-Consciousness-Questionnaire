source("packages.r")
source("function_scq.R")

STUDY1 <- read.csv('./data/data.csv') %>%
   mutate_all(~ifelse(.==99,NA,.))
STUDY2 <- read.csv('./data/data_2nd.csv') %>%
   mutate_all(~ifelse(.==99,NA,.))
STUDY3 <- read.csv('./data/data_3rd.csv') %>%
   mutate_all(~ifelse(.==99,NA,.))

# a1
a11 <- nrow(STUDY1)
a12 <- nrow(STUDY2)
a13 <- nrow(STUDY3)

# a2
a22 <- nrow(subset(
        subset(STUDY2, participated_study1 == 0),
        SCQ_attention1 != 1|SCQ_attention2 != 3)
        )
a23 <- nrow(subset(STUDY3,SCQ_attention1 != 1|SCQ_attention2 != 3))

# b1
b11 <- a11
b12 <- a12 - a22 - 53
b13 <- a13 - a23

# b2
b21 <- b11 - 379
b22 <- b12 - 341

# c1
c11 <- 379
c12 <- 341
c13 <- b13

# d
d11 <- nrow(read.csv('./data/data_outlier_1st.csv'))
d12 <- nrow(read.csv('./data/data_outlier_2nd.csv'))
d13 <- nrow(read.csv('./data/data_imp_outlier.csv'))

# c2
c21 <- c11 - d11
c22 <- c12 - d12
c23 <- c13 - d13

library(DiagrammeR)
library(DiagrammeRsvg)
g <- grViz("
    digraph test {
        # general Settings
        graph [
            newrank = true, 
            nodesep = '0.4', 
            ranksep = '0.4', 
            splines = ortho]; 

        node [
            shape = rectangle, 
            style = 'filled, rounded', 
            fillcolor = gray95, 
            fontsize = 10, 
            width = 3];
    
        edge [
            penwidth = 2, 
            arrowsize = 0.7];

        # node relationships between clusters
        {rank = same; a1; b1; c1; d1};
        {rank = same; a2; b2; c2};

        # edges between clusters
        a1 -> b1;
        a1 -> a2;
        b1 -> b2;
        b1 -> c1 [label = 'missing complementation',headlabel = ' ',];
        c1 -> c2;
        c1 -> d1;

        subgraph cluster_A {
            label = 'Subjects'; 
            fontsize = 12;
    
            a1 [
                height = 1.2, 
                label = 'Subjects recruited:\n study 1 (n = 409)\n study 2 (n = 414)\n study 3 (n = 456)'
            ]

            a2 [
                height = 1.2, 
                label = 'Excluded:
                - Participated in the previous study:
                study 2 (n = 53)
                - Answered incorrectly to a directed attention questionnaire:
                study 2 (n = 11)
                study 3 (n = 15)'

            ]
        }
        subgraph cluster_B {
            label = 'Preprocessing'; 
            fontsize = 12;
    
            b1 [
                height = 1.2, 
                label = 'Subjects preprocessed:\n study 1 (n = 409)\n study 2 (n = 350)\n study 3 (n = 441)'
            ]

            b2 [
                height = 1.2, 
                label = 'Excluded due to a missing of † 25%:\n study 1 (n = 30)\n study 2 (n = 9)'

            ]

            c1 [
                height = 1.2, 
                label = 'Subjects with missing value complemented:\n study 1 (n = 379)\n study 2 (n = 341)\n study 3 (n = 441)'
            ]

            c2 [
                height = 1.2, 
                label = 'Excluded by outlier test:\n study 1 (n = 77)\n study 2 (n = 39)\n study 3 (n = 59)'

            ]
        }
        
        subgraph cluster_D {
            label = 'Included'; 
            fontsize = 12;
            d1 [
                height = 1.2, 
                label = 'Subjects included:\n study 1 (n = 302)\n study 2 (n = 302)\n study 3 (n = 382)'
            ]
        }
    }
")

export_svg(g) |> 
  writeLines("./material/g.svg")


##### study 1 #####
pdf("./material/missing1.pdf",width = 12, height = 15)
vis_miss(STUDY1[,1:49]) + 
    theme(plot.margin= unit(c(1, 3, 1, 1), "lines"))
dev.off()

##### study 2 #####
pdf("./material/missing2.pdf",width = 12, height = 15)
vis_miss(STUDY2[,1:49]) + 
    theme(plot.margin= unit(c(1, 3, 1, 1), "lines"))
dev.off()

##### study 3 #####
pdf("./material/missing3.pdf",width = 12, height = 15)
vis_miss(STUDY3[,1:49]) + 
    theme(plot.margin= unit(c(1, 3, 1, 1), "lines"))
dev.off()

fig_missing <- r"(
\begin{figure}[h]
 \begin{minipage}[b]{0.50\linewidth}
  \centering
  \includegraphics[keepaspectratio, scale=0.45]
  {material/missing1.pdf}
  \subcaption{(a)Missing values in Study 1}\label{poly04}
 \end{minipage}
 \begin{minipage}[b]{0.50\linewidth}
  \centering
  \includegraphics[keepaspectratio, scale=0.45]
  {material/missing2.pdf}
  \subcaption{(b)Missing values in Study 2}\label{poly06}
 \end{minipage}\\
 \begin{minipage}[b]{0.50\linewidth}
  \centering
  \includegraphics[keepaspectratio, scale=0.45]
  {material/missing3.pdf}
  \subcaption{(c)Missing values in Study 3}\label{poly08}
 \end{minipage}
 \caption{Percentage of missing items in each study}\label{reg_poly}
\end{figure}
)"
