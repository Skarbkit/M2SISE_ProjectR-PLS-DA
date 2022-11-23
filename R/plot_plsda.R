#' Model overview plot for PLS-DA
#'
#' @description
#' affiche un ensemble de graphiques (ObjectPLSDA résidus, coefficients de régression, 
#' rapport de classification  et la predictions pour le modèle PLS-DA.) 
#'
#' @param ObjectPLSDA
#' un modèle PLS-DA (ObjectPLSDA de la classe \code{plsda})
#' @param ncomp
#' combien de composants utiliser (si NULL - la valeur optimale sélectionnée par l'utilisateur sera utilisée)
#' @param nc
#'la classe pour montrer les plots
#' @param show.legend
#' montrer ou non  la legend des plots
#' @param ...
#' autres arguments
#' @export

plot.plsda <- function(ObjectPLSDA, ncomp = ObjectPLSDA$ncomp.selected, nc = 1, show.legend = TRUE, ...) {
  
  if (!is.null(ncomp) && (ncomp <= 0 || ncomp > ObjectPLSDA$ncomp)) {
    stop("Valeur erronée pour le nombre de composants.")
  }
  
  par(mfrow = c(2, 2))
  plotXResiduals(ObjectPLSDA, ncomp = ncomp, show.legend = show.legend, ...)
  plotRegcoeffs(ObjectPLSDA, ncomp = ncomp, ny = nc, ...)
  plotMisclassified(ObjectPLSDA, nc = nc, show.legend = show.legend, ...)
  plotPredictions(ObjectPLSDA, ncomp = ncomp, show.colorbar = show.legend, ...)
  par(mfrow = c(1, 1))
}
