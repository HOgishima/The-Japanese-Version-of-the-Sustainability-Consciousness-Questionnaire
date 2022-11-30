source("preamble_study2.r")

INPUTDATA <- model_data

#alpha     <- round(psych::alpha(INPUTDATA[,unlist(ALL)])[[2]][1:4],2)
#omega     <- omega(INPUTDATA[,unlist(ALL)],digits=2)[[4]]

B_alpha   <- psych::alpha(INPUTDATA[,unlist(B)])[[1]][2]
KA_alpha  <- psych::alpha(INPUTDATA[,c(unlist(K),unlist(A))])[[1]][2]
KAB_alpha <- psych::alpha(INPUTDATA[,c(unlist(K),unlist(A),unlist(B))])[[1]][2]

B_omega   <- psych::omega(INPUTDATA[,unlist(B)])[[4]]
KA_omega  <- psych::omega(INPUTDATA[,c(unlist(K),unlist(A))])[[4]]
KAB_omega <- psych::omega(INPUTDATA[,c(unlist(K),unlist(A),unlist(B))])[[4]]

#B_ENV_alpha   <- psych::alpha(INPUTDATA[,unlist(B_ENV)])[[1]][2]
#B_SOC_alpha   <- psych::alpha(INPUTDATA[,unlist(B_SOC)])[[1]][2]
#B_ECO_alpha   <- psych::alpha(INPUTDATA[,unlist(B_ENV)])[[1]][2]