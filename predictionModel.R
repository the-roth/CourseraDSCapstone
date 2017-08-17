# BEFORE DOING ANYTHING, SET YOUR WORKING DIRECTORY TO WHERE THIS FILE IS

# In this file we will work only with the sample data created
# But should import functions to recreate Train, Test and Validation
# Sample datasets and clean them, Remove profanity, punctuation, 
# convert to lower case and remove whitespace

library(quanteda)
library(dtplyr)
library(data.table) # still need data.table
library(stringi)
#library(dplyr) # dplyr is now superseced by dtplyr
library(profvis)
#library(pryr) # for checking memory usage

# Read a file from an open connection
# Need 'rb' mode and skipNul=TRUE for it to work
# I think the important part is opening in 'rb' mode
# Also used in other file but whatever
readFile <- function(f){
    con = file(f, 'rb')
    output = readLines(con, encoding="UTF-8", skipNul=TRUE)
    close(con)
    output
}


# Open a file, convert to sentences and remove punctuation / profanity
readAndClean <- function(dataFile){
    f <- readFile(dataFile)
    # If we convert to corpus straight away we have to do a lot of cleaning
    # operations, which take forever. I don't want to do that so instead
    # will use regular expressions to make the process a lot faster.
    
    # Note: Doing gsub before saving to disk messes up the file, not sure 
    # why. The unsplit(strsplit()) functions split the files by sentence
    # for better grammar but if there is '. . .' it makes empty items.
    out <- stri_trim(tolower(
        gsub("([0-9]|[^[:alnum:][:space:]['-]|[QA]:|([AaPp].[Mm].))+",
             '', unlist(strsplit(f, "\\.\\s+")))))
    
    # make a profanity filter as well
    profanityFile = readFile("profanity.txt")
    profanityFilter = paste(profanityFile, collapse='|')
    out <- stri_trim(gsub(profanityFilter, '', out))
    
    return(out)
}

# Actually make the training / test etc. sets and n-grams now
outTrain <- readAndClean("US_NewsBlogsTwitterTrain.txt")
outTest <- readAndClean("US_NewsBlogsTwitterTest.txt")

# get rid of unnecessarily short lines as they are not useful
outTrain = outTrain[nchar(outTrain) > 25]
outTest = outTest[nchar(outTest) > 25]

Corpus = corpus(outTrain)

# Take a tokenized Corpus with punctuation and hyphens removed
# Converts it to a Document Frequency Matrix
# Then returns a word-frequency data table with single phrases removed
# The quanteda package is much faster than tm
#__________________________________________________________
WordFrequencyDT <- function(tokens, NGrams = 1){
    DFM = dfm(tokens_ngrams(tokens,
                            n=NGrams,
                            concatenator=' '))
    wf = topfeatures(DFM, ncol(DFM))
    dt <- data.table(term = names(wf), occurrences = wf)
    setkey(dt, term)    # data tables use Radix sorting, extremely fast
    if(NGrams > 1){
        dt = dt[occurrences > 1,] # cut down on sparse terms
    }
    print("The dfm size is:")
    print(object.size(DFM))
    print("Number of phrases in DFM")
    print(nrow(dt))
    print("The word/frequency data table size is:")
    print(object.size(dt))
    return(dt)
}

# remove punctuation just in case funny things happened earlier
Tokens = tokenize(Corpus,
                  removeHyphens=TRUE,
                  removePunct=TRUE)

# It is TIME
unigrams = WordFrequencyDT(Tokens, 1)
bigrams = WordFrequencyDT(Tokens, 2)
trigrams = WordFrequencyDT(Tokens, 3)
quadgrams = WordFrequencyDT(Tokens, 4)

# Finally...
write.table(unigrams,"unigrams.txt",row.names=FALSE)
write.table(bigrams,"bigrams.txt",row.names=FALSE)
write.table(trigrams,"trigrams.txt",row.names=FALSE)
write.table(quadgrams,"quadgrams.txt",row.names=FALSE)

#_________________
# If you already have your n-gram .txt files, start from here
# Now for the prediction stuff. Need to read in our files first etc.

unigrams = fread("unigrams.txt", header=T)
bigrams = fread("bigrams.txt", header=T)
trigrams = fread("trigrams.txt", header=T)
quadgrams = fread("quadgrams.txt", header=T)

# Setting a key allows for radix sorting for data tables, very fast
# There must be way to use this and filter results faster than startswith()
#setkey(unigrams, term)
#setkey(bigrams, term)
#setkey(trigrams, term)
#setkey(quadgrams, term)

# Doing this, the unigrams lose their setkey properties and I can't sort
unigrams = unigrams[order(-occurrences)]
bigrams = bigrams[order(-occurrences)]
trigrams = trigrams[order(-occurrences)]
quadgrams = quadgrams[order(-occurrences)]

#_____________________________________________________________________
# Now we need to turn the information into a predictive model

# returns in lower case the last n words from a phrase in vector form
endOfPhrase <- function(phrase, n){
    tail(unlist(stri_extract_all_words(tolower(phrase))),n)
}

# returns the original word list without the first word
oneLessWord <- function(wordlist) {
    tail(wordlist, length(wordlist) - 1)
}

# construct a regular expression term to search our word frequency lists
# No longer used, but kept for now
regexPhrase <- function(wordlist, lastCharIsSpace=FALSE) {
    restoredPhrase <- paste(wordlist, '', collapse='')
    regex = paste('^', restoredPhrase, sep='')
    if(lastCharIsSpace == FALSE){
        regex = stri_sub(regex, from=1, to=-2)
    }
    return(regex)
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
                numOfRows = min(length(searchIndices), numPreds)
                # limit rows returned to < 5
                # Don't need to reorder preds, n-gram list is already ordered
                predsFound = nGramList(numWords)[searchIndices,][1:numOfRows,]
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

# Used for Shiny app
werePredsFound <- function(preds){
    #a successful pred call has class data.table and data.frame, so...
    if(nrow(preds)==0 || is.na(preds[[1]])){
        return("Please enter a phrase, or try again.")
    }
    return("The top matching predictions found are:")
}

# Extract all words from an article
# If length of article is more than 6, take a random 6 word phrase from
# the article
# Put the starting 5 words into my model and predict the next word.
# Compare this word with either top 1 predictions or top 3 predictions
convertPhrase <- function(s){
    words = unlist(stri_extract_all_words(tolower(s)))
    if(length(words) >= 6){
        min = sample(1:(length(words) - 5), 1)
        wordList = words[min:(min+5)]
        return(wordList)
    } else {
        return('')
    }
}

# Figure out if the last word in a phrase appears in the top 1, 3 or 5
# preds. returns 1, 2 or 3 respectively, and 0 if result isn't found
testPhrase <- function(wordList, npreds){
    if(wordList[[1]] == ''){
        return(NULL)
    }
    phrase = paste(c(wordList[1:5], ''), collapse = ' ') 
    correctWord = wordList[6]
    #print("The phrase used is:")
    #print(phrase)
    #print('The correct word is:')
    #print(correctWord)
    wordPreds <- topXPreds(phrase, npreds)
    #print('The word predictions are:')
    #print(wordPreds)
    #print("_________________________________________")
    # here 1 = top pred, 2 = top 3 preds, 3 = top 5 preds
    if (correctWord %in% wordPreds$word){
        return(ceiling(match(correctWord, wordPreds$word)/2))
    }
    return(0)
}

# Could probably break this function down to make more readable
predictionAccuracy <- function(dataset, nSamples, nPreds=5, seed=123){
    set.seed(seed)
    selection = sample(1:length(dataset), nSamples)
    # results == (top1preds, top3preds, top5preds)
    results = c(0, 0, 0)
    skipped = 0
    total = 0
    for(i in selection){
        #a = as.numeric(Sys.time())*1000
        words = convertPhrase(dataset[i])
        # phraseIsCorrect is either 1, 2 or 3 for top 1, 3 and 5 preds
        phraseIsCorrect = testPhrase(words, nPreds)
        
        if(is.null(phraseIsCorrect)){
            skipped = skipped + 1
            #print("Phrase wasn't long enough.")
            next
        }
        # else, we have a result and need to count it in
        total = total + 1
        
        if (phraseIsCorrect > 0){
            # e.g. If in top 3 preds, 
            # need to update top3 and top5, but not top1
            results[phraseIsCorrect:3] = results[phraseIsCorrect:3] + 1
            
        }
        else{
            next
        }
        #print(as.numeric(Sys.time())*1000 - a, digits=15)
    }
    accuracy = round(results*100/total, digits = 1)
    
    print(paste0("Top 1, 3 and 5 word prediction accuracy  over ",
                 nSamples, ' samples,'))
    print(noquote(paste0(accuracy, '%')))
    
    print(paste0(skipped, ' phrases were skipped as they were too short.'))
    #print(rbind(c('skipped', 'correct', 'total', 'accuracy (%)'),
               # c(skipped, correct, total, accuracy)))
}

sentence = "This looks like a piece of cake "
preds = topXPreds(sentence)
preds

# Used to use grep for searching, 110 - 330ms per n-gram list search
# startswith() reduces searches to about 10 - 30ms
# So now 1000 searches takes 35 ish seconds instead of about 5 minutes
# Current Accuracy (seed 345):
# 29% (top 5 predictions), 27% (top 3), 21% (top 1)
predictionAccuracy(outTest, 1000, seed=345)
