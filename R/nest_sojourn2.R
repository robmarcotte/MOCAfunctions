# nest_sojourn
#
#
#
# Library dependencies:

nest_sojourn2 = function(sojourn, orig_soj_length_min = 180, nest_length = 60, step_by = .00001){
  if(length(sojourn) > orig_soj_length_min){
    n = ceiling(length(sojourn)/nest_length)-1

    additives = seq(0, n/10, by = step_by)
    additives = rep(additives, each = nest_length, length.out = length(sojourn))

    if(length(sojourn) %% nest_length > 0){
      short_window = length(sojourn) %% nest_length
      additives[(length(additives)-short_window):length(additives)] = additives[(length(additives)-short_window)]
    }
    sojourn = sojourn + additives

  }

  return(sojourn)
}
