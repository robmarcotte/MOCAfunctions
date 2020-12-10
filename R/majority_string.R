# majority_string
#
# Function to identify the majority string in a vector of character strings.
# Designed to be used with tapply function
#
# coded_strings = A vector of strings
# piece = Designate what variable should be returned from the function. Can be any one of c('coded_strings','frequency','perc.obs')
#
# Returns a single value based on the value supplied to input 'piece'
#
# Library dependencies: stringr, dplyr

majority_string = function(coded_strings, piece = 'coded_strings'){
  frequencies = as.data.frame(table(coded_strings))
  frequencies$coded_strings = as.character(frequencies$coded_strings)

  frequencies = arrange(frequencies, by = desc(Freq))
  total = sum(frequencies$Freq)

  majority = frequencies[1,]
  majority$perc.obs = majority$Freq/total
  if(piece == 'coded_strings')
    return(majority$coded_strings)
  if(piece == 'frequency')
    return(majority$Freq)
  if(piece == 'perc.obs')
    return(majority$perc.obs)

}
