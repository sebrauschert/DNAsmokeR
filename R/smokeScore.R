#' Create a DNA methylation based in utero exposure score to maternal
#' smoking.
#'
#' @param data A data frame with CpGs in the columns and study participants in the rows.
#' @param ARRAY specifying array type: either "450k" or "EPIC".
#' @param class specifying the desired output: either "prob" or "class".
#' @return \code{smokeScore} as probability or classification.
#' @examples
#'  smokeScore(data, "450k", "prob")
#'  smokeScore(data, "EPIC", "class")
#' @export


# Calculate the score ----------------------------------------
# Option for: class and score

# Plot distribution

smokeScore <- function(data=data, ARRAY=c("450K", "EPIC"), class=c("class", "prob")){

  # Load coefficient data sets ---------------------------------
  #load("data/EPIC.rda")
  #load("data/I450K.rda")

    if(ARRAY %in% "450k"){
    SCORE <- NULL
    score <- NULL
    cpg   <- NULL

    for (i in I450K$CpG[2:205]) {
        CPG <- as.numeric(I450K %>%
                      filter(CpG %in% i) %>%
                      select(Coefficient)
                      )
        score <- cbind(score , as.numeric(unlist((CPG * data[,i]))))
        cpg <- c(cpg, i)
    }
    score         <- na.omit(data.frame(score))
    names(score)  <- cpg

    smokScore   <- rowSums(score) + as.numeric(I450K[1,2])
    normalized <- as.numeric((smokScore-min(smokScore))/(max(smokScore)-min(smokScore)))
    predictSMOK <- factor(ifelse(normalized >0.5, "smoke_exp", "not_exp"))

    if(class %in% "prob"){
      return(normalized)
    }
    if(class %in% "class") {
      return(predictSMOK)
    }
  }
  if(ARRAY %in% "EPIC") {
    SCORE <- NULL
    score <- NULL
    cpg   <- NULL

    for (i in EPIC$CpG[2:182]) {
      CPG <- as.numeric(EPIC %>%
                          filter(CpG %in% i) %>%
                          select(Coefficient)
      )
      score <- cbind(score , as.numeric(unlist((CPG * data[,i]))))
      cpg <- c(cpg, i)
    }
    score         <- na.omit(data.frame(score))
    names(score)  <- cpg

    smokScore   <- rowSums(score) + as.numeric(EPIC[1,2])
    normalized  <- as.numeric((smokScore-min(smokScore))/(max(smokScore)-min(smokScore)))
    predictSMOK <- factor(ifelse(normalized >0.5, "smoke_exp", "not_exp"))

    if(class %in% "prob"){
      return(normalized)
    }
    if(class %in% "class") {
      return(predictSMOK)
    }
  }
}
