loadData<-function()
{
    
    NLP_tokenizer1 <- function(x) {
        unlist(lapply(ngrams(words(x),1), paste, collapse = "_"), use.names = FALSE)
    }
    
    NLP_tokenizer2 <- function(x) {
        unlist(lapply(ngrams(words(x),2), paste, collapse = "_"), use.names = FALSE)
    }
    
    
    NLP_tokenizer3 <- function(x) {
        unlist(lapply(ngrams(words(x),3), paste, collapse = "_"), use.names = FALSE)
    }
    
    tokenizer<-c(NLP_tokenizer1, NLP_tokenizer2, NLP_tokenizer3)
    

library(tm)
library(ngram)
setwd("C:/studies/data-science/10-capstone")

nlines<-1000
filesIn<-c("../Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "../Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "../Coursera-SwiftKey/final/en_US/en_US.news.txt")
filesOut<-c("en_US.twitter", "en_US.blogs", "en_US.news")
    

    for(i in 1:length(filesIn))
    {
    data<-fread(filesIn[i],  sep="\n", header=F, nrows=nlines)
    
    corpus = VCorpus(VectorSource(data$V1))
    
    #Create ngrams
        for(j in 1:3){
            freqs<-DocumentTermMatrix(corpus, list(tokenize = tokenizer[[j]]))
            
            sparse<-freqs
            #sparse = removeSparseTerms(freqs, 0.995) #remove non-frequent words
            tSparse = as.data.frame(as.matrix(sparse))
            
            colnames(tSparse) = make.names(colnames(tSparse))
            filename<-paste(filesOut[i], j, ".csv", sep="")
            write.csv(tSparse, filename)
        }
    
      }

}






