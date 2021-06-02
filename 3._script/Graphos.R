library(tm)
library(stringr)
library(wordcloud)
library(memoise)
library(shiny)


textR <- str_c(dta1$Resumen, collapse = " ")
textT <- str_c(dta1$Título, collapse = " ")

textRT <- str_c(textR, textT, collapse = " ")

ptd <- PlainTextDocument(textRT,id = basename(tempfile()),
                         language = "en")


myText <- Corpus(VectorSource(ptd))
myText = tm_map(myText, tolower)
myText <- tm_map(myText, removePunctuation)
myText = tm_map(myText, removeNumbers)
myText = tm_map(myText, removeWords, stopwords("spanish"))

myText = tm_map(myText, removeWords, stopwords("english"))
myText = tm_map(myText, removeWords, c("two","first","perú", 
                                       "ecuador", "peru", "aplica", "species"))

myText <- tm_map(myText, PlainTextDocument)


myDTM = TermDocumentMatrix(myText)


m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)

#install.packages('wordcloud')
library(wordcloud)


# Finalmente creamos la nube:

par(mar=c(0,0,0,0))
set.seed(4332)
wordcloud(names(v),v, min.freq = 100, 
          colors=brewer.pal(6,"Dark2"),random.order=FALSE)

wordcloud_rep <- repeatable(wordcloud)


wordcloud_rep(names(v), v, scale=c(4,0.5),
              min.freq = 5, max.words=20,
              colors=brewer.pal(8, "Dark2"))




###Análisis autores

textA <- str_c(dta1$`Autor(es)`, collapse = " ")
Aptd <- PlainTextDocument(textA,id = basename(tempfile()),
                         language = "en")


myTextA <- Corpus(VectorSource(textA))
myTextA = tm_map(myTextA, tolower)
#myTextA <- tm_map(myTextA, removePunctuation)
myTextA <- tm_map(myTextA, PlainTextDocument)


myDTMA = TermDocumentMatrix(myTextA)


mA = as.matrix(myDTMA)

vA = sort(rowSums(mA), decreasing = TRUE)

set.seed(25)

wordcloud_rep(names(vA), vA, scale=c(4,0.5),
              min.freq = 4, max.words=32,
              colors=brewer.pal(8, "Dark2"))












rownames(tipo) <- tipo$Codigo 
  
AutCrop <- corpus(tipo, text_field = "Autor(es)"  )

Aut_DTM = TermDocumentMatrix(AutCrop)

AutP_m <- as.matrix(Aut_DTM)

AutP_m <- AutP_m[,nchar(colnames(AutP_m))>3 ]

# for(i in 1:length(Autor)) nodes[[i]] <-  rep(names(Autor[i]), length(Autor[[i]]))
# nodes <- unlist(nodes)
# 
# target <- unlist(Autor)
# 
# Autm <- crossprod(table(nodes, target))