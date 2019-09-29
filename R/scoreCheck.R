#' Quality checks for the score, if smoking variable is available
#'
#' @param classScore returned from the smokeScore function, specifying "class"
#' @param questVariable a binary variable for expsure to maternal smoking during pregnancy, available
#'  in the researchers study data
#' @return \code{scoreMatrix} as confusion matrix.
#' @examples
#' \dontrun{
#'  scoreMatrix(classScore, questVariable)
#'  }
#' @export

# Quality check if binary variable for expposure to maternal smoking is available ---------------
# Confusion Matrix

scoreMatrix <- function(classScore,questVariable) {
  caret::confusionMatrix(classScore, questVariable)
}


#' Quality checks for the score, if smoking variable is available
#'
#' @param questVariable a binary variable for expsure to maternal smoking during pregnancy, available
#'  in the researchers study data
#' @param probScore probability score, returned from the smokeScore function, specifying "prob"
#' @return \code{boxplotScore} as boxplot for score and study information on maternal smoking.
#' @examples
#' \dontrun{
#'  boxplotScore(probScore, questVariable)
#'  }
#' @export
#'
# Visual representation in form of boxplot, in case the smoking varible is available

boxplotScore <- function(probScore, questVariable) {

  boxData <- data.frame(probability=as.numeric(probScore), questVar=as.factor(questVariable))

    ggplot2::ggplot(boxData, aes(x=questVariable, y=probScore,fill=questVariable)) +
    geom_boxplot() +
    xlab("Questionnaire Variable") +
    ylab("Smoking Score") +
    theme_minimal()
}

