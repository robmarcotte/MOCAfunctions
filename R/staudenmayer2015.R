#' Apply methods from Staudenmayer et al 2015
#'
#' @param   acc_data_raw Raw tri-axial wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   method Character string indicating the model to be applied to data
#' @param   mods_filepath Filepath that contains models in an R object. Assuming by default that model objects are already loaded in the environment.
#' @param   samp_freq Sampling frequency of the raw accelerometer data. Default is 80 hz
#' @param   epoch Non-overlapping window size in seconds. Default is 15-seconds
#' @param   expand_1sec Binary indicator of whether only estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and Staudenmayer 2015 Method estimates
#'
#' @example staudenmayer2015_rf_loco(acc_data_raw)

staudenmayer2015 = function(acc_data_raw, method = c('lm.met.model',
                                                     'rf.combo.model','rf.met.level.model','rf.met.model','rf.loc.model','rf.sed.model',
                                                     'tr.combo.model','tr.met.level.model','tr.loc.model','tr.sed.model',
                                                     'all'),
                            mods_filepath = NA, samp_freq = 80, epoch = 15, expand_1sec = F){

  # Determine number of rows in the raw data
  n = nrow(acc_data_raw)

  # Derive features from accelerometer data
  acc_data_raw.sum = ag_feature_calc(acc_data_raw, window = epoch)

  if(!is.na(mods_filepath)){
    load(mods_filepath)
  }

  switch(method,
         'all' = {
           # Check if some model objects are missing
           if(all(sapply(c('lm.met.model',
                           'rf.combo.model','rf.met.level.model','rf.met.model','rf.loc.model','rf.sed.model',
                           'tr.combo.model','tr.met.level.model','tr.loc.model','tr.sed.model'), exists))==F){
             missing_models = which(sapply(c('lm.met.model',
                                             'rf.combo.model','rf.met.level.model','rf.met.model','rf.loc.model','rf.sed.model',
                                             'tr.combo.model','tr.met.level.model','tr.loc.model','tr.sed.model'), exists) == F)
             stop('Some models are not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }

           # Linear Regression
           acc_data_raw.sum$Staudenmayer2015_lm.mets = predict(lm.met.model, newdata=acc_data_raw.sum)
           # Tree Models
           acc_data_raw.sum$Staudenmayer2015_tr.combo = factor(predict(tr.combo.model, newdata=acc_data_raw.sum, type = 'class'), levels =c('S','L','M','V'), labels = c('Sedentary','LPA','MPA','VPA'))
           acc_data_raw.sum$Staudenmayer2015_tr.metlevel = factor(predict(tr.met.level.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('light','moderate','vigorous'), labels = c('LPA','MPA','VPA'))
           acc_data_raw.sum$Staudenmayer2015_tr.locomotion = predict(tr.loc.model, newdata=acc_data_raw.sum, type = 'class')
           acc_data_raw.sum$Staudenmayer2015_tr.sedentary = factor(predict(tr.sed.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('non-sedentary','sedentary'), labels = c('Non-Sedentary','Sedentary'))
           # Random Forest Models
           acc_data_raw.sum$Staudenmayer2015_rf.combo = factor(predict(rf.combo.model, newdata=acc_data_raw.sum, type = 'class'), levels =c('S','L','M','V'), labels = c('Sedentary','LPA','MPA','VPA'))
           acc_data_raw.sum$Staudenmayer2015_rf.mets = predict(rf.met.model, newdata=acc_data_raw.sum)
           acc_data_raw.sum$Staudenmayer2015_rf.metlevel = factor(predict(rf.met.level.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('light','moderate','vigorous'), labels = c('LPA','MPA','VPA'))
           acc_data_raw.sum$Staudenmayer2015_rf.locomotion = predict(rf.loc.model, newdata=acc_data_raw.sum, type = 'class')
           acc_data_raw.sum$Staudenmayer2015_rf.sedentary = factor(predict(rf.sed.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('non-sedentary','sedentary'), labels = c('Non-Sedentary','Sedentary'))

         },
         'lm.met.model' = {
           if(!exists('lm.met.model')){
             stop('lm.met.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_lm.mets = predict(lm.met.model, newdata=acc_data_raw.sum)


         },
         'rf.combo.model' = {
           if(!exists('rf.combo.model')){
             stop('rf.combo.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_rf.combo = factor(predict(rf.combo.model, newdata=acc_data_raw.sum, type = 'class'), levels =c('S','L','M','V'), labels = c('Sedentary','LPA','MPA','VPA'))

         },
         'rf.met.level.model' = {
           if(!exists('rf.met.level.model')){
             stop('rf.met.level.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_rf.metlevel = factor(predict(rf.met.level.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('light','moderate','vigorous'), labels = c('LPA','MPA','VPA'))

         },
         'rf.met.model' = {
           if(!exists('rf.met.model')){
             stop('rf.met.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_rf.mets = predict(rf.met.model, newdata=acc_data_raw.sum)

         },
         'rf.loc.model' = {
           if(!exists('rf.loc.model')){
             stop('rf.loc.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_rf.locomotion = predict(rf.loc.model, newdata=acc_data_raw.sum, type = 'class')

         },
         'rf.sed.model' = {
           if(!exists('rf.sed.model')){
             stop('rf.sed.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_rf.sedentary = factor(predict(rf.sed.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('non-sedentary','sedentary'), labels = c('Non-Sedentary','Sedentary'))

         },
         'tr.combo.model' = {
           if(!exists('tr.combo.model')){
             stop('tr.combo.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_tr.combo = factor(predict(tr.combo.model, newdata=acc_data_raw.sum, type = 'class'), levels =c('S','L','M','V'), labels = c('Sedentary','LPA','MPA','VPA'))

         },
         'tr.met.level.model' = {
           if(!exists('tr.met.level.model')){
             stop('tr.met.level.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_tr.metlevel = factor(predict(tr.met.level.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('light','moderate','vigorous'), labels = c('LPA','MPA','VPA'))

         },
         'tr.loc.model' = {
           if(!exists('tr.loc.model')){
             stop('tr.loc.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_tr.locomotion = predict(tr.loc.model, newdata=acc_data_raw.sum, type = 'class')

         },
         'tr.sed.model' = {
           if(!exists('tr.sed.model')){
             stop('tr.sed.model is not loaded in the environment. Supply the filepath in mods_filepath or library the MOCAModelData package')
           }
           acc_data_raw.sum$Staudenmayer2015_tr.sedentary = factor(predict(tr.sed.model, newdata=acc_data_raw.sum, type = 'class'), levels = c('non-sedentary','sedentary'), labels = c('Non-Sedentary','Sedentary'))

         })

  if(expand_1sec == T){
    Staudenmayer2015 = data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = samp_freq)])

    method_col_indices = str_which(colnames(acc_data_raw.sum), 'Staudenmayer2015')

    for(i in 1:length(method_col_indices)){
      temp = acc_data_raw.sum[,method_col_indices[i]]
      temp = temp[rep(seq_len(nrow(temp)), each = epoch),]
      temp = temp[1:floor(n/samp_freq),]

      Staudenmayer2015 = dplyr::bind_cols(Staudenmayer2015, temp)
    }

    return(Staudenmayer2015)

  } else {

    return(acc_data_raw.sum)

  }

}
