# perform regular analysis and return table
analyze <- function(repFile = NULL, repHeaders, plate, type, 
                    d1Start = NA, d1End = NA, d2Start = NA, d2End = NA) {
  # validate file and range values
  if (is.null(repFile))
    stop("File input required")
  if (is.na(d1Start) || is.na(d1End) || is.na(d2Start) || is.na(d2End) ||
      d1Start < 1 || d2Start < 1 || d1Start > d1End || d2Start > d2End || 
      (d2Start >= d1Start && d2Start <= d1End) || (d1Start >= d2Start && d1Start <= d2End))
    stop("Invalid column range")
  
  # access relevant well data
  typeNum = as.numeric(type)
  plateNum = as.numeric(plate)
  if (plate == 9) {
    analysisTable <- read.csv("trp_data.csv", header = TRUE)
    normFactors <- as.vector(analysisTable[, typeNum + 1])
    well_cms <- analysisTable[, 1:2]
  } else {
    analysisTable <- read.csv("plate_data.csv", header = TRUE)
    normFactors <- as.vector(analysisTable[, 3*plateNum+typeNum+6])
    well_cms <- analysisTable[, c(1, plateNum + 1)]
  }
  
  # access uploaded replicate data
  replicate <- read.csv(repFile, header = repHeaders)
  repD1 <- as.matrix(replicate[, d1Start:d1End])
  repD2 <- as.matrix(replicate[, d2Start:d2End])
  
  # validate replicate data
  if (!is.numeric(repD1) || !is.numeric(repD2))
    stop("Non-numeric data detected")
  if (nrow(replicate) != length(normFactors) || nrow(replicate) != nrow(well_cms))
    stop("Improper number of wells")
  
  # normalize data
  repD1 = repD1 / normFactors
  repD2 = repD2 / normFactors
  
  # Wilcox Two Sample t-test/Mann-Whitney/Independent samples
  p_value <- matrix(sapply(1:nrow(repD1), function(i) {
    wilcox.test(repD1[i,], repD2[i,])$p.value }), ncol = 1)
  
  # means of replicate data
  mean_p <- rowMeans(repD1)
  mean_cntrl <- rowMeans(repD2)
  
  #final result table
  resultTable <- cbind(well_cms, round(cbind(mean_p, mean_cntrl, p_value, repD1, repD2), digits = 5))
  names(resultTable)[1:5] <- c("Well", "CMS#", "Mean_Patient", "Mean_Control", "Unadjusted_P_Value")
  
  return(resultTable)
}

