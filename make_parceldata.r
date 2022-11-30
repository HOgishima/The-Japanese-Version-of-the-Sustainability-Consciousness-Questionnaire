source("preamble_study1.r")

# make parcel data
make_val_japanese()

make_parcel <- function(){
    for(i in 1:3){
        for (j in 1:3){
            valname <- names(ALL[[i]][j])
            parcelname <- paste(valname, '_P', sep='')
            val <- c(apply(model_data[ALL[[i]][[j]]], 1, sum))
            assign(parcelname, val, envir = globalenv())
        }
    }
}

make_parcel()
parcel_data <- cbind(
    K_ENV_P, K_SOC_P, K_ECO_P,
    A_ENV_P, A_SOC_P, A_ECO_P,
    B_ENV_P, B_SOC_P, B_ECO_P
)
names(parcel_data) <- c('K_ENV_P', 'K_SOC_P', 'K_ECO_P', 'A_ENV_P', 'A_SOC_P', 'A_ECO_P', 'B_ENV_P', 'B_SOC_P', 'B_ECO_P')