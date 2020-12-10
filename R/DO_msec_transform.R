# DO_msec_transform
#
# Function to expand noldus data observations to hundredths of a second observations
# Accepts Noldus data that has been processed using the DO_descriptives function
#
# noldus_data = Noldus data previously processed using the DO_descriptives function
#
# Returns a data frame with the Noldus data expanded to hundredths of a second
#
# Library dependencies: stringr, readxl, dplyr

DO_msec_transform <- function(noldus_data){
  DO.data.1 <- noldus_data #read File
  secs <- ceiling(round(DO.data.1$Duration_sf*100)) # round duration of obs to nearest hundredth of a second (hence * 100)

  big.DO.1 <- data.frame(Behavior=as.character(rep(DO.data.1$Behavior,times=secs)),
                         METs=as.numeric(rep(DO.data.1$METs,times=secs)), stringsAsFactors = F)

  modifiers = str_which(colnames(noldus_data), 'Modifier')
  for(i in 1:length(modifiers)){
    modifier_data = as.character(rep(DO.data.1[,modifiers[i]], times=secs))
    big.DO.1 = bind_cols(big.DO.1, as.character(modifier_data))
    colnames(big.DO.1)[ncol(big.DO.1)] = colnames(noldus_data)[modifiers[i]]
  }

  big.DO.1$MET.level <- as.character(cut(as.numeric(big.DO.1$METs),breaks=c(0,1.5,3,6,Inf),right=F,labels=c("Sed","Light","Mod","Vig")))

  return(big.DO.1)

}

