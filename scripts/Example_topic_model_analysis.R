## Install/Load Tools & Data
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/gofastr")
pacman::p_load(tm, topicmodels, dplyr, tidyr,  devtools, LDAvis, ggplot2)


## Source topicmodels2LDAvis & optimal_k functions
invisible(lapply(
    file.path("https://gist.githubusercontent.com/trinker", 
        c(
            "477d7ae65ff6ca73cace/raw/79dbc9d64b17c3c8befde2436fdeb8ec2124b07b/topicmodels2LDAvis",
            "9aba07ddb07ad5a0c411/raw/c44f31042fc0bae2551452ce1f191d70796a75f9/optimal_k"
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
optimal_k(doc_term_mat, 19)


## Run the Model
lda_model <- topicmodels::LDA(doc_term_mat, 10, control = list(seed=100))


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
