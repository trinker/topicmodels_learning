## Install/Load Tools & Data
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr, igraph, devtools, LDAvis, ggplot2)


## Source topicmodels2LDAvis & optimal_k functions
invisible(lapply(
    file.path(
        "https://raw.githubusercontent.com/trinker/topicmodels_learning/master/functions", 
        c("topicmodels2LDAvis.R", "optimal_k.R")
    ),
    devtools::source_url
))

data(presidential_debates_2012)


## Generate Stopwords 
stops <- c(
        tm::stopwords("english"),
        tm::stopwords("SMART"),    
        "governor", "president", "mister", "obama","romney"
    ) %>%
    gofastr::prep_stopwords() 


## Create the DocumentTermMatrix
doc_term_mat <- presidential_debates_2012 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops, stem=TRUE) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documents() 


## Control List
control <- list(burnin = 500, iter = 1000, keep = 100)

## Determine Optimal Number of Topics
(k <- optimal_k(doc_term_mat, 40, control = control))


## Run the Model
control[["seed"]] <- 100
lda_model <- topicmodels::LDA(doc_term_mat, k=as.numeric(k), method = "Gibbs", 
    control = control)


## Plot the Topics Per Person & Time
topics <- topicmodels::posterior(lda_model, doc_term_mat)[["topics"]]
topic_dat <- dplyr::add_rownames(as.data.frame(topics), "Person_Time")
colnames(topic_dat)[-1] <- apply(terms(lda_model, 10), 2, paste, collapse = ", ")

tidyr::gather(topic_dat, Topic, Proportion, -c(Person_Time)) %>%
    tidyr::separate(Person_Time, c("Person", "Time"), sep = "_") %>%
    dplyr::mutate(Person = factor(Person, 
        levels = c("OBAMA", "ROMNEY", "LEHRER", "SCHIEFFER", "CROWLEY", "QUESTION" ))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() +
        ggplot2::facet_grid(Person~Time) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::xlab("Proportion")


## Plot the Topics Matrix as a Heatmap 
heatmap(topics, scale = "none")


## Network of the Word Distributions Over Topics
post <- topicmodels::posterior(lda_model)

cor_mat <- cor(t(post[["terms"]]))
cor_mat[ cor_mat < .05 ] <- 0
diag(cor_mat) <- 0

graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
graph <- delete.edges(graph, E(graph)[ weight < 0.05])

E(graph)$edge.width <- E(graph)$weight*20
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 15

par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
    edge.color = "orange", vertex.color = "orange", 
    vertex.frame.color = NA, vertex.label.color = "grey30")
title("Strength Between Topics Based On Word Probabilities", cex.main=.8)


## Network of the Topics Over Documents
minval <- .1
topic_mat <- topicmodels::posterior(lda_model)[["topics"]]

graph <- graph_from_incidence_matrix(topic_mat, weighted=TRUE)
graph <- delete.edges(graph, E(graph)[ weight < minval])

E(graph)$edge.width <- E(graph)$weight*17
E(graph)$color <- "blue"
V(graph)$color <- ifelse(grepl("^\\d+$", V(graph)$name), "grey75", "orange")
V(graph)$frame.color <- NA
V(graph)$label <- ifelse(grepl("^\\d+$", V(graph)$name), paste("topic", V(graph)$name), gsub("_", "\n", V(graph)$name))
V(graph)$size <- c(rep(10, nrow(topic_mat)), colSums(topic_mat) * 20)
V(graph)$label.color <- ifelse(grepl("^\\d+$", V(graph)$name), "red", "grey30")

par(mar=c(0, 0, 3, 0))
set.seed(365)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
    vertex.color = adjustcolor(V(graph)$color, alpha.f = .4))
title("Topic & Document Relationships", cex.main=.8)


## LDAvis of Model
lda_model %>%
    topicmodels2LDAvis() %>%
    LDAvis::serVis()


##==================##
## Fitting New Data ##
##==================##


## Create the DocumentTermMatrix for New Data
doc_term_mat2 <- partial_republican_debates_2015 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, location, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops, stem=TRUE) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documents() 


## Run the Model for New Data
control2 <- control
control2[["estimate.beta"]] <- FALSE

lda_model2 <- topicmodels::LDA(doc_term_mat2, k = as.numeric(k), model = lda_model, 
    control = control2)


## Plot the Topics Per Person & Location for New Data
topics2 <- topicmodels::posterior(lda_model2, doc_term_mat2)[["topics"]]
topic_dat2 <- dplyr::add_rownames(as.data.frame(topics2), "Person_Location")
colnames(topic_dat2)[-1] <- apply(terms(lda_model2, 10), 2, paste, collapse = ", ")

tidyr::gather(topic_dat2, Topic, Proportion, -c(Person_Location)) %>%
    tidyr::separate(Person_Location, c("Person", "Location"), sep = "_") %>%
    ggplot2::ggplot(ggplot2::aes(weight=Proportion, x=Topic, fill=Topic)) +
        ggplot2::geom_bar() +
        ggplot2::coord_flip() +
        ggplot2::facet_grid(Person~Location) +
        ggplot2::guides(fill=FALSE) +
        ggplot2::xlab("Proportion")


## LDAvis of Model for New Data
lda_model2 %>%
    topicmodels2LDAvis() %>%
    LDAvis::serVis()