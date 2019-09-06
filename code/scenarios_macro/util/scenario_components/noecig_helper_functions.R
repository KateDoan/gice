## Helper functions
getYearIndex <- function(yr, startYear) {
  yr - startYear + 1
}

getAgeIndex <- function(ag, startAge) {
  ag - startAge + 1
}

createMat <- function(val_vec, 
                      times_vec=c(7,12,10,10,10,10,11), 
                      numYear=51) {
  m <- matrix(rep(rep(val_vec, times_vec), numYear), ncol=numYear)
  m
}