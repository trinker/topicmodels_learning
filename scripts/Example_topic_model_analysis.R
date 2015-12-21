## Install/Load Tools & Data
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr,  devtools, LDAvis, ggplot2)


## Source topicmodels2LDAvis & optimal_k functions
invisible(lapply(
    file.path("https://gist.githubusercontent.com/trinker", 
        c(
            "477d7ae65ff6ca73cace/raw/79dbc9d64b17c3c8befde2436fdeb8ec2124b07b/topicmodels2LDAvis",
            "594bd132b180a43945f7/raw/70fbb1aa2a9113837a9a9f8a6c43d884c2ef5bd0/optimal_k%25202"
    )),
    devtools::source_url
))

data(presidential_debates_2012)


## Generate Stopwords 
stops <- c(
        tm::stopwords("english"),
        "governor", "president", "mister", "obama","romney"
    ) %>%
    gofastr::prep_stopwords() 


## Create the DocumentTermMatrix
doc_term_mat <- presidential_debates_2012 %>%
    with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
    gofastr::remove_stopwords(stops) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documents() 

## Determine Optimal Number of Topics
optimal_k(doc_term_mat)


## Run the Model
lda_model <- topicmodels::LDA(doc_term_mat, k=18, control = list(seed=100))


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
    gofastr::remove_stopwords(stops) %>%                                                    
    gofastr::filter_tf_idf() %>%
    gofastr::filter_documents() 


## Run the Model for New Data
lda_model2 <- topicmodels::LDA(doc_term_mat2, k = k, model = lda_model, 
    control = list(seed = 100, estimate.beta = FALSE))


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