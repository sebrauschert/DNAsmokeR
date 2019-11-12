#' Create a DNA methylation based in utero exposure score to maternal
#' smoking.
#'
#' @param data A data frame with CpGs in the columns and study participants in the rows.
#' @param ARRAY specifying array type: either "450k" or "EPIC".
#' @param class specifying the desired output: either "prob" or "class".
#' @return \code{smokeScore} as probability or classification.
#' @examples
#' \dontrun{
#'  smokeScore(data, "450k", "prob")
#'  smokeScore(data, "EPIC", "class")
#'  }
#' @export


# Calculate the score ----------------------------------------
# Option for: class and score

# Plot distribution

smokeScore <- function(data=data, ARRAY=c("450k", "EPIC"), class=c("class", "prob")){


    if(ARRAY %in% "450k"){
        SCORE <- NULL
        score <- NULL
        cpg   <- NULL
        
        availCpGs <- names(data)[names(data) %in% DNAsmokeR:::I450K$CpG]
        for (i in availCpGs) {
            CPG <- as.numeric(I450K %>%
                      filter(CpG %in% i) %>%
                      select(Coefficient)
                      )
            score <- cbind(score , as.numeric(unlist((CPG * data[,i]))))
            cpg <- c(cpg, i)
        }
        score         <- as.data.frame(score)
        names(score)  <- cpg

        smokScore   <- rowSums(score) + as.numeric(I450K[1,2])
        normalized  <- as.numeric((smokScore - min(smokScore,na.rm=T))/(max(smokScore, na.rm=T) - min(smokScore, na.rm=T)))
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
        
        
        availCpGs <- names(data)[names(data) %in% DNAsmokeR:::EPIC$CpG]

        for (i in availCpGs) {
            CPG <- as.numeric(EPIC %>%
                          filter(CpG %in% i) %>%
                          select(Coefficient)
            )
            score <- cbind(score , as.numeric(unlist((CPG * data[,i]))))
            cpg <- c(cpg, i)
        }
        score         <- as.data.frame(score)
        names(score)  <- cpg

        smokScore   <- rowSums(score) + as.numeric(EPIC[1,2])
        normalized  <- as.numeric((smokScore - min(smokScore,na.rm=T))/(max(smokScore, na.rm=T) - min(smokScore, na.rm=T)))
        predictSMOK <- factor(ifelse(normalized >0.5, "smoke_exp", "not_exp"))

    if(class %in% "prob"){
      return(normalized)
    }
    
    if(class %in% "class") {
      return(predictSMOK)
    }
  }
}
