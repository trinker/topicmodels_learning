#' Find Optimal Number of Topics
#' 
#' Iteratively produces models and then compares log likelihoods in a graphical output.
#' 
#' @param x A \code{\link[tm]{DocumentTermMatrix}}.
#' @param max.k Maximum number of topics to fit (start small [i.e., default of 30] and add as necessary).
#' @param \ldots Ignored.
#' @return Returns the \code{\link[base]{data.frame}} of k (number of topics) and the associated log likelihood.
#' @references \url{http://stats.stackexchange.com/a/25128/7482}
#' @keywords k topicmodel 
#' @export
#' @author Ben Marwick and Tyler Rinker <tyler.rinker@@gmail.com>.
#' @examples
#' ## Install/Load Tools & Data
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load_gh("trinker/gofastr")
#' pacman::p_load(tm, topicmodels, dplyr, tidyr,  devtools, LDAvis, ggplot2)
#' 
#' 
#' ## Source topicmodels2LDAvis function
#' devtools::source_url("https://gist.githubusercontent.com/trinker/477d7ae65ff6ca73cace/raw/79dbc9d64b17c3c8befde2436fdeb8ec2124b07b/topicmodels2LDAvis")
#' 
#' data(presidential_debates_2012)
#' 
#' 
#' ## Generate Stopwords 
#' stops <- c(
#'         tm::stopwords("english"),
#'         "governor", "president", "mister", "obama","romney"
#'     ) %>%
#'     gofastr::prep_stopwords() 
#' 
#' 
#' ## Create the DocumentTermMatrix
#' doc_term_mat <- presidential_debates_2012 %>%
#'     with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
#'     gofastr::remove_stopwords(stops) %>%                                                    
#'     gofastr::filter_tf_idf() %>%
#'     gofastr::filter_documents() 
#' 
#' 
#' opti_k <- optimal_k(doc_term_mat)
#' opti_k
optimal_k <- function(x, max.k = 30, ...){

    if (max.k > 20) {
        message("\nGrab a cup of coffee this is gonna take a while...\n")
        flush.console()
    }

    tic <- Sys.time()

    best_model <- lapply(seq(2, max.k, by=1), function(k){
        if (k %% 10 == 0){
            elapsed <- gsub("^0+", "", as.character(round(as.numeric(difftime(Sys.time(), tic, units = "mins")), 1)))
            cat(sprintf("%s of %s iterations (Time elapsed: %s mins)\n", k, max.k, elapsed)); flush.console()
        }
        topicmodels::LDA(x, k, ...)
    })

    out <- data.frame(
        k = c(2:max.k), 
        logLik = sapply(best_model, logLik)
    )

    class(out) <- c("optimal_k", "data.frame")
    out
}

#' Plots a plot.optimal_k Object
#' 
#' Plots a plot.optimal_k object
#' 
#' @param x A \code{optimal_k} object.
#' @param \ldots Ignored.
#' @method plot plot.optimal_k
#' @export 
plot.optimal_k <- function(x, ...){

    ggplot2::ggplot(x, ggplot2::aes_string(x="k", y="logLik")) + 
        ggplot2::xlab("Number of Topics") + 
        ggplot2::ylab("Log Likelihood") + 
        ggplot2::geom_smooth(size=.8, se=FALSE) + 
        ggplot2::geom_line(size=1) + 
        ggplot2::theme_bw()  + 
        ggplot2::theme(
            axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
            axis.title.y = ggplot2::element_text(size = 14, angle=90)
        )

}

#' Prints a optimal_k Object
#' 
#' Prints a optimal_k object
#' 
#' @param x A \code{optimal_k} object.
#' @param \ldots Ignored.
#' @method print optimal_k
#' @export 
print.optimal_k <- function(x, ...){

    print(graphics::plot(x))

}