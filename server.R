library(shiny)
library(data.table)
library(stringi)
library(DT)

unigrams = fread("unigrams.txt", header=T)
bigrams = fread("bigrams.txt", header=T)
trigrams = fread("trigrams.txt", header=T)
quadgrams = fread("quadgrams.txt", header=T)

#setkey(unigrams, term)
#setkey(bigrams, term)
#setkey(trigrams, term)
#setkey(quadgrams, term)

unigrams = unigrams[order(-occurrences)]
bigrams = bigrams[order(-occurrences)]
trigrams = trigrams[order(-occurrences)]
quadgrams = quadgrams[order(-occurrences)]

# returns in lower case the last n words from a phrase in vector form
endOfPhrase <- function(phrase, n){
    tail(unlist(stri_extract_all_words(tolower(phrase))),n)
}

# returns the original word list without the first word
oneLessWord <- function(wordlist) {
    tail(wordlist, length(wordlist) - 1)
}

# make a search phrase for startswith(), where last char is a space or not
searchPhrase <- function(wordlist, lastCharIsSpace=FALSE) {
    restoredPhrase <- paste(wordlist, '', collapse='')
    if(lastCharIsSpace == FALSE){
        return(stri_sub(restoredPhrase, from=1, to=-2))
    }
    return(restoredPhrase)
}

# Choose which n-gram set to use
nGramList <- function(number){
    if(number == 4){
        return(quadgrams)
    } else if(number == 3){
        return(trigrams)
    } else if(number == 2){
        return(bigrams)
    } else if(number == 1){
        return(unigrams)
    } else{
        print("There was an error, no n-gram model selected.")
        return()
    }        
}

# Extract the last 3 words of a phrase,
# If the phrase ends in a space, search quadgrams list and
# return the last word of each quadgram in order of frequency.
# If phrase ends in a character, return last words of trigrams instead.
# This acts to autocomplete a word rather than the next predicted word.
# Repeat the process with trigrams/bigrams, then bigrams/unigrams.
# Return empty data frame if no results are found.
topXPreds <- function(phrase, numPreds=5) {
    lastXWords = endOfPhrase(phrase, 3)
    numWords = length(lastXWords)
    result = data.frame(term=character(),
                        word = character(),
                        occurrences=numeric(),
                        stringsAsFactors=FALSE)
    
    if(is.na(lastXWords[[1]])){
        return(result)
    }    
    isSpace = FALSE
    if(stri_sub(phrase,from=-1) == " "){
        isSpace = TRUE
        # e.g. look at trigrams if last char is a space, else bigrams
        numWords = numWords + 1 
    }
    
    i = 3
    while(i > 0){
        #print(i)
        if(length(lastXWords) == i){
            phrase = searchPhrase(lastXWords, lastCharIsSpace=isSpace)
            searchIndices = which(startsWith(nGramList(numWords)$term, phrase))
            if(length(searchIndices) > 0){
                numOfRows = min(length(searchIndices), numPreds) # limit rows returned to < 5
                # Don't need to reorder preds, the n-gram list is already ordered
                predsFound = nGramList(numWords)[searchIndices,][1:numOfRows,]
                #predsFound = nGramList(numWords)[searchIndices,][order(-occurrences)][1:numOfRows,]
                words = sapply(predsFound$term, function(x) endOfPhrase(x,1),
                               USE.NAMES=FALSE)
                predsFound$word = words
                # we want to prioritise results from larger n-grams,
                # as they hold more weight and meaning
                # So we only add results from smaller n-grams
                # that haven't been predicted yet
                predsFound = predsFound[!(predsFound$word %in% result$word),]
                result = rbind(result, predsFound)
                # return a result if we have enough unique word preds
                # Merge 'of the' and 'out of the' - same last word for example
                if(length(unique(result$word)) >= numPreds){
                    break
                }
            }
            lastXWords = oneLessWord(lastXWords)
            numWords = numWords - 1
        }
        i = i - 1
    }
    numRows = min(nrow(result), numPreds)
    return(result[1:numRows,])
    #wordList = select(result, word:occurrences)
    #wordList =  wordList %>%
    #    group_by(word) %>%
    #    summarise_each(funs(sum)) %>%
    #    arrange(-occurrences)
    #return(as.data.frame(wordList[1:numRows,]))
}

werePredsFound <- function(preds){
    #a successful pred call has class data.table and data.frame, so...
    if(nrow(preds)==0 || is.na(preds[[1]])){
        return("Please enter a phrase, or try again.")
    }
    return("The top matching predictions found are:")
}

shinyServer(function(input, output) {

    phraseUsed <- reactive({
        werePredsFound(topXPreds(paste0(trimws(input$phrase), input$variable),
                                 numPreds=5))
    })

    output$predictions <- phraseUsed
    
    output$predTable <- renderTable(
        topXPreds(paste0(trimws(input$phrase), input$variable), numPreds=5)[,1:2]
    )    

})