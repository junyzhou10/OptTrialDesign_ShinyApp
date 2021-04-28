str2vec <- function(input){
  output = NULL
  if (input == "") {
    output = 0
  } else {
    str1 = unlist(strsplit(input, ','))
    for ( i in 1:length(str1) ) {
      if ( grepl('-', str1[i], fixed = T) ) {
        output = c( output, seq(as.numeric( unlist(strsplit(str1[i], '-')) )[1], as.numeric( unlist(strsplit(str1[i], '-')) )[2], 1) )
      } else {
        output = c(output, as.numeric(str1[i]))
      }
    }
  }
  
  return(output)
}