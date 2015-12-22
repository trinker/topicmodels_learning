#' Transform Model Output for Use with the LDAvis Package
#'
#' Convert a \pkg{topicmodels} output into the JSON form required by the \pkg{LDAvis} package.
#'
#' @param model A \code{\link[]{topicmodel}} object.
#' @param \ldots Currently ignored.
#' @seealso \code{\link[LDAvis]{createJSON}}
#' @export
#' @examples
#' \dontrun{
#' data("AssociatedPress", package = "topicmodels")
#' model <- LDA(AssociatedPress[1:20,], control = list(alpha = 0.1), k = 3)
#' LDAvis::serVis(topicmodels2LDAvis(model))
#' }
topicmodels2LDAvis <- function(x, ...){
    post <- topicmodels::posterior(x)
    if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
    mat <- x@wordassignments
    LDAvis::createJSON(
        phi = post[["terms"]], 
        theta = post[["topics"]],
        vocab = colnames(post[["terms"]]),
        doc.length = slam::row_sums(mat, na.rm = TRUE),
        term.frequency = slam::col_sums(mat, na.rm = TRUE)
    )
}



