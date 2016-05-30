#' Hello World
#'
#' Basic hello world function to be called from the demo app
#'
#' @export
#' @param myname your name. Required.
hello <- function(myname = ""){
  if (!require("rjson")) {
    install.packages("rjson", dependencies = TRUE)
    library(rjson)
  }

  data <- mtcars
  #jsonString <- toJSON(unname(split(data, 1:nrow(data))))
  data <- as.matrix(data)
  d <- dist(data)
  hc <- hclust(d)
  #plot(hc)

  #names(hc)
  mergeData <- data.frame(hc$merge)
  lableData <- hc$labels
  size <- dim(mergeData)
  storeData <- c(1:size[1])

  for (i in 1:size[1]) {
    index1 <- mergeData$X1[i]
    index2 <- mergeData$X2[i]

    if (index1 < 0 & index2 < 0) {
      string <- paste('{"name": "","children":[{"name":"',
                      lableData[-index2], '"},{"name":"',lableData[-index1],'"}]}',
                      collapse = "")
    }
    else if (index1 > 0 & index2 > 0) {
      string <- paste('{"name": "","children":[',
                      storeData[index1], ',',storeData[index2],']}',
                      collapse = "")
    }
    else if (index1 > 0 & index2 < 0) {
      string <- paste('{"name": "","children":[',
                      storeData[index1], ',{"name":"',lableData[-index2],'"}]}',
                      collapse = "")
    }
    else if (index1 < 0 & index2 > 0) {
      string <- paste('{"name": "","children":[{"name":"',
                      lableData[-index1],'"},',storeData[index2],']}',
                      collapse = "")
    }
    storeData[i] <- string
  }
  cat(storeData[size[1]])

}
