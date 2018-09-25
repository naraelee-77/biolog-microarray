# take a 96 column matrix for trp and return the means of 3 columns (depending on j)
transformTrp <- function(trpY, j) {
  if (ncol(trpY) != 96)
    stop("Improper number of wells")

  subj = lapply(1:8, function(i) {
    n = 12*(i-1) + 3*(j-1) + 1
    rowMeans(trpY[,n:(n + 2)])
  })
  wells = do.call(cbind, subj)
  colnames(wells) = letters[1:8]
  
  wells
}