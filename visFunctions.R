# for kinetic data, read file and return matrix of data
processCSV <- function(file, headers) {
  # read data from file
  fileData = read.csv(file[["datapath"]], headers)
  
  # separate into x and y, and validate data
  if (ncol(fileData) == 106) {
    x = as.vector(fileData[,10])
    y = as.matrix(fileData[,-(1:10)])
    colnames(y) = names(fileData)[-(1:10)]
  } else if (ncol(fileData) == 9) {
    x = as.vector(fileData[,1])
    y = as.matrix(fileData[,-1])
    colnames(y) = names(fileData)[-1]
  } 
  else
    stop("Improper number of columns")
  if (!is.numeric(x) || !is.numeric(y))
    stop("Non-numeric data detected")
  
  # fix data that ends at 23.75 hr
  if (x[length(x)] == 23.75) {
    x = append(x, 24)
    y = rbind(y, y[nrow(y),])
  }
  
  if (!identical(x, seq(0, 24, 0.25)))
    stop("Improper data in time column")
  
  return(y)
}

# make sure data is compatible for next step
validateData <- function(pData, cData, trp, columns) {
  if (!trp && (ncol(cData) != 96 || ncol(pData) != 96))
    stop("Improper number of columns")
  if (trp && ncol(cData) != 8)
    stop("Improper number of columns")
  if (trp && columns == "avg" && ncol(pData) != 8)
    stop("Improper number of columns")
  if (trp && columns != "avg" && ncol(pData) != 96)
    stop("Improper number of columns")
  
  if (!identical(names(pData), names(cData)))
    stop("Different header data between patient and control")
}

# make plot for visualization; performed 96 times
wellPlot <- function(visData, i) {
  # extract x and y variables and well name
  x = seq(0, 24, 0.25)
  y1 = visData$patient[,i]
  y2 = visData$control[,i]
  ymax = max(visData$patient, visData$control)
  well = colnames(visData[[2]])[i]
  
  # set up plot and title
  par(mai = c(0, 0, 0.2, 0), pin = c(1.1, 1.1), xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", ann = FALSE)
  plot(x, y1, type = "n", ylim = c(0, ymax), xlab = "", ylab = "")
  title(well, cex.main = 2)
  
  # make polygons to color in plot
  patient = rbind(cbind(x, y1), c(max(x), 0), c(min(x), 0))
  control = rbind(cbind(x, y2), c(max(x), 0), c(min(x), 0))
  both = rbind(cbind(x, apply(cbind(y1, y2), 1, min)), c(max(x), 0), c(min(x), 0))
  polygon(patient, col = "royalblue", border = NA)
  polygon(control, col = "tan4", border = NA)
  polygon(both, col = "khaki1", border = NA)
}