-   Key Players
-   Videos
    -   Introductory
    -   Theory
    -   Visualization
-   Articles
-   Websites & Blogs
-   R Resources
    -   lda
    -   topicmodels
    -   stm
    -   LDAvis
-   Topic Modeling R Demo
    -   Install/Load Tools & Data
    -   Generate Stopwords
    -   Create the DocumentTermMatrix
    -   Run the Model
    -   LDAvis of Model

This is a collection documenting the resources I find related to topic
models with an R flavored focus. A *topic model* is a type of
[*generative*](https://en.wikipedia.org/wiki/Generative_model) model
used to "discover" latent topics that compose a *corpus* or collection
of documents. Typically topic modeling is used on a collection of text
documents but can be used for other modes including use a caption
generation for images.

Key Players
===========

Papadimitriou, Raghavan, Tamaki & Vempala, Santosh (1997) first
introduced the notion of topic modeling in their ["Latent Semantic
Indexing: A probabilistic analysis"](articles/Papadimitriou1997.pdf).
Thomas Hofmann (1999) devloped "Probabilistic latent semantic indexing".
Blei, Ng, & Jordan (2003) proposed *latent Dirichlet allocation* (LDA)
as a means of modeling documents with multiple topics but assumes the
topic are uncorrelated. Blei & Lafferty (2007) proposed *correlated
topics model* (CTM), extending LDA to allow for correlations between
topcs. Roberts, Stewart, Tingley, & Airoldi (2013) propose a
[*Structural Topic Model*](articles/Roberts2013.pdf) (STM), allowing the
inclusion of meta-data in the modeling process.

Videos
======

Introductory
------------

-   Boyd-Graber, J. (2013). [Computational Linguistics I: Topic
    Modeling](https://www.youtube.com/watch?v=4p9MSJy761Y)

Theory
------

-   Blie, D. (2007) [Modeling Science: Dynamic Topic Models of Scholarly
    Research](https://www.youtube.com/watch?v=7BMsuyBPx90)

-   Blie, D. (2009) [Topic Models:Part
    I](http://videolectures.net/mlss09uk_blei_tm/#) & [Topic Models:Part
    II](http://videolectures.net/mlss09uk_blei_tm/#) ([Lecture
    Notes](presentations/Blie2009.pdf))

Visualization
-------------

-   statgraphics (2014) [LDAvis: A method for visualizing and
    interpreting topic
    models](https://www.youtube.com/watch?v=IksL96ls4o0)

Articles
========

-   Blei, D. M. (2012). [Probabilistic topic
    models](articles/Blie2012.pdf). *Communications of the ACM, (55)*4,
    77-84. DOI: 10.1145/2133806.2133826

-   Blei, D. M. & Lafferty, J. D. (2007) [A correlated topic model of
    Science](Blie2007.pdf). *The Annals of Applied Statistics 1*(1),
    17-35. <doi:10.1214/07-AOAS114>.
    <http://projecteuclid.org/euclid.aoas/1183143727>.

-   Blei, D. M. & Lafferty, J. D. (2009) [Topic
    models](articles/Blie2009.pdf). In A Srivastava, M Sahami (eds.),
    [*Text mining: classification, clustering, and
    applications*](articles/Srivastava2009.pdf). Chapman & Hall/CRC
    Press. 71-93.

-   Blei, D. M. & McAuliffe, J. (2008). [Supervised topic
    models](articles/Blie2008.pdf). In Advances in Neural Information
    Processing Systems 20. 1-8.

-   Blei, D. M., Ng, A.Y., & Jordan, M.I. (2003). [Latent Dirichlet
    Allocation](articles/Blie2003.pdf). *Journal of Machine Learning
    Research, 3*, 993â€“1022.

-   Chang, J., Boyd-Graber, J. , Wang, C., Gerrish, S., & Blei. D.
    (2009). [Reading tea leaves: How humans interpret topic
    models](articles/Chang2009.pdf). In *Neural Information Processing
    Systems*.

-   Griffiths, T.L. & Steyvers, M. (2004). [Finding Scientific
    Topics](articles/Griffiths2004.pdf). Proceedings of the National
    Academy of Sciences of the United States of America, 101,
    5228â€“5235.

-   Gruen, B. & Hornik, K. (2011). [topicmodels: An R Package for
    Fitting Topic Models. Journal of Statistical
    Software](articles/Grun2011.pdf), 40(13), 1-30.

-   Roberts M.E., Stewart B.M., Tingley D., & Airoldi E.M. (2013) [The
    Structural Topic Model and Applied Social
    Science](articles/Roberts2013.pdf). *Advances in Neural Information
    Processing Systems Workshop on Topic Models: Computation,
    Application, and Evaluation*. 2013.

-   Roberts, M., Stewart, B., Tingley, D., Lucas, C., Leder-Luis, J.,
    Gadarian, S., Albertson, B., et al. (2014). [Structural topic models
    for open ended survey responses](articles/Roberts2014.pdf).
    *American Journal of Political Science, American Journal of
    Political Science, 58*(4), 1064-1082.

-   Roberts, M., Stewart, B., Tingley, D. (n.d.). [stm: R Package for
    Structural Topic Models](articles/Robertsnd.pdf), 1-49.

-   Sievert, C. & Shirley, K. E.. (2014). [LDAvis: A Method for
    Visualizing and Interpreting Topics.](articles/Robertsnd.pdf)
    *Proceedings of the Workshop on Interactive Language Learning,
    Visualization, and Interfaces* 63-70.

Websites & Blogs
================

-   Blie, D. (nd). [David Blie on Topic
    Models](https://www.cs.princeton.edu/~blei/topicmodeling.html)

-   Jockers, M.L. (2013). ["Secret" Recipe for Topic Modeling
    Themes](http://www.matthewjockers.net/2013/04/12/secret-recipe-for-topic-modeling-themes/)

-   Schmidt, B.M. (20) [Words Alone: Dismantling Topic Models in the
    Humanities](http://journalofdigitalhumanities.org/2-1/words-alone-by-benjamin-m-schmidt/)

-   Weingart, S. (2012). [Topic Modeling for Humanists: A Guided
    Tour](http://www.scottbot.net/HIAL/?p=19113)

R Resources
===========

lda
---

topicmodels
-----------

stm
---

LDAvis
------

Topic Modeling R Demo
=====================

Install/Load Tools & Data
-------------------------

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh("trinker/gofastr")
    pacman::p_load(tm, topicmodels, dplyr, tidyr,  devtools, LDAvis, ggplot2)

    ## Source topicmodels2LDAvis function
    devtools::source_url("https://gist.githubusercontent.com/trinker/477d7ae65ff6ca73cace/raw/79dbc9d64b17c3c8befde2436fdeb8ec2124b07b/topicmodels2LDAvis")

    ## SHA-1 hash of file is f9a066b61c9f992daff3991a3293e18897268598

    data(presidential_debates_2012)

Generate Stopwords
------------------

    stops <- c(
            tm::stopwords("english"),
            "governor", "president", "mister", "obama","romney"
        ) %>%
        gofastr::prep_stopwords() 

Create the DocumentTermMatrix
-----------------------------

    doc_term_mat <- presidential_debates_2012 %>%
        with(gofastr::q_dtm_stem(dialogue, paste(person, time, sep = "_"))) %>%           
        gofastr::remove_stopwords(stops) %>%                                                    
        gofastr::filter_tf_idf() %>%
        gofastr::filter_documents() 

Run the Model
-------------

    lda_model <- topicmodels::LDA(doc_term_mat, 10)

    ## Plot the Topics Per Person_Time
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

![](inst/figure/unnamed-chunk-6-1.png)

LDAvis of Model
---------------

The output from **LDAvis** is not easily embedded within an R markdown
document, however, the reader may [see the results
here](http://trinker.github.io/LDAvis/example/).

    lda_model %>%
        topicmodels2LDAvis() %>%
        LDAvis::serVis("C:/Users/Tyler/Desktop/LDAvis/example")
