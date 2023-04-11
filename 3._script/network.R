source(file = "3._script/1.Datos.R", encoding = "UTF-8")

library(reshape2)
library(igraph)
library(tnet)
library(tm)
library(stringr)
library(wordcloud)
library(memoise)
library(shiny)
library(tidyr)
library(stringi)
library(dplyr)
library(ggraph)

dta1.1 <- dta


autor <- dta1.1 %>%
            distinct(Codigo, Autor) %>% 
            separate("Autor", into = paste0("autor", 1:20), ";") 


for(i in 2:ncol(autor)){
  autor[,i] <- stri_trim(autor[,i])
  autor[,i] <- gsub(" ", "_", autor[,i])
  autor[,i] <- stri_replace_all_charclass(autor[,i],"[.]", "")
  autor[,i] <- stri_replace_all_charclass(autor[,i],"[-]", "_")
} 

autor1 <- autor %>% 
            filter(!is.na(autor2))

dtaaut <- melt(autor1, id.vars = "Codigo", 
               variable.name = "Autors", value.name = "from") 


autF <- crossprod(table(dtaaut[,c("Codigo", "from")]))

autF <- autF[-1,-1]

va <- as.data.frame(autF) %>%
          dplyr::mutate(Persona = rownames(.),
                        Occurrences = rowSums(.)) %>%
          dplyr::select(Persona, Occurrences) %>% 
          dplyr::filter(Occurrences>25)


ed <- as.data.frame(autF) %>% 
          dplyr::mutate(from = rownames(.)) %>%
          tidyr::gather(to, Frequency, 1:ncol(autF)) %>%
          dplyr::mutate(Frequency = ifelse(Frequency == 0, NA, Frequency))

ed <- ed %>% 
        filter(!is.na(ed$Frequency),
               to%in%va$Persona,
               from%in%va$Persona)

ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)



tg <- tidygraph::as_tbl_graph(ig) %>% 
        tidygraph::activate(nodes) %>% 
        dplyr::mutate(label=name)
# set seed
v.size <- V(tg)$Occurrences
# inspect
# v.size[v.size<20] <- NA 

set.seed(12345)

E(tg)$weight <- E(tg)$Frequency
# inspect weights
head(E(tg)$weight, 10)



