u_esc <- function(inputstring) {
  clean <- gsub("(\\s)", "", inputstring)
  clean <- gsub("-", "", clean, fixed=F)
  return(clean)
}
