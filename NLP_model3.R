library(quanteda)
library(data.table)
library(stringi)
library(dplyr)

# Download the file and unzip it, if the unzipped folder doesn't exist yet
if(!dir.exists("final")){
    File <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(File, "Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
}

# Read a file from an open connection
# Need 'rb' mode and skipNul=TRUE for it to work
# I think the important part is opening in 'rb' mode
readFile <- function(f){
    con = file(f, 'rb')
    output = readLines(con, encoding="UTF-8", skipNul=TRUE)
    close(con)
    output
}

twitter = readFile("final/en_US/en_US.twitter.txt")
blogs = readFile("final/en_US/en_US.blogs.txt")
news = readFile("final/en_US/en_US.news.txt")

rbind(
    c(  'Twitter dataset details',
        'Blogs dataset details',
        'News dataset details'
    ),
    
    c(  paste('Size: ', round(object.size(twitter)/1000000), 'Mb'),
        paste('Size: ', round(object.size(blogs)/1000000), 'Mb'),
        paste('Size: ', round(object.size(news)/1000000), 'Mb')
    ),
    c(  paste(length(twitter), 'entries'),
        paste(length(blogs), 'entries'),
        paste(length(news), 'entries')
    )
)

# So we now need to sample from this in order to have sufficient
# information to build a predictive model, but not have too large a 
# sample dataset otherwise our model won't fit in the Shiny app.
percentage = 0.12

newsSampleSize = round(length(news) * percentage)
blogsSampleSize = round(length(blogs) * percentage)
twitterSampleSize = round(length(twitter) * percentage)

set.seed(123)
newsSample <- news[sample(1:length(news), newsSampleSize,
                          replace=FALSE)]
set.seed(456)
blogsSample <- blogs[sample(1:length(blogs), blogsSampleSize,
                            replace=FALSE)]
set.seed(789)
twitterSample <- twitter[sample(1:length(twitter), twitterSampleSize,
                            replace=FALSE)]

c(length(newsSample), length(blogsSample), length(twitterSample))

newsSampleTrain <- newsSample[1:60000]
newsSampleTest <- newsSample[60001:80000]
newsSampleValidation <- newsSample[80001:100000]

blogsSampleTrain <- blogsSample[1:60000]
blogsSampleTest <- blogsSample[60001:80000]
blogsSampleValidation <- blogsSample[80001:100000]

twitterSampleTrain <- twitterSample[1:120000]
twitterSampleTest <- twitterSample[120001:160000]
twitterSampleValidation <- twitterSample[160001:200000]

dataTrain = c(newsSampleTrain,
              blogsSampleTrain,
              twitterSampleTrain) # 240000 entries
dataTest = c(newsSampleTest,
             blogsSampleTest,
             twitterSampleTest)   # 80000 Entries
dataValidation = c(newsSampleValidation,
                   blogsSampleValidation,
                   twitterSampleValidation) # 80000 Entries

rbind('Sample dataset details',
      paste('Size: ', round(object.size(dataTrain)/1000000), 'Mb'),
      paste(length(dataTrain), 'entries'))

sum(nchar(dataTrain))

# We can save these datasets and work with them later
write.table(dataTrain,"US_NewsBlogsSampleTrain.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

write.table(dataTest,"US_NewsBlogsSampleTest.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

write.table(dataValidation,"US_NewsBlogsSampleValidation.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

# Might as well free up some workspace for the next part, if needed
rm(blogs, blogsSampleTrain, blogsSampleTest, blogsSampleValidation,
   news, newsSampleTrain, newsSampleTest, newsSampleValidation,
   twitter, twitterSampleTrain, twitterSampleTest, twitterSampleValidation,
   dataTrain, dataTest, dataValidation) 

#______________________________________________________________
# From now onwards we work only with the sample data created
# But should make a function to recreate Train, Test and Validation
# Sample datasets and clean them
# Remove profanity, punctuation, convert to lower case and remove whitespace

readAndClean <- function(dataFile){
    f <- readFile(dataFile)
    # If we convert to a corpus straight away we have to do a lot of cleaning
    # operations, which take forever. I don't want to do that so will instead
    # use regular expressions to make the process a lot faster.
    
    # make a profanity filter as well
    profanityFile = readFile("profanity.txt")
    profanityFilter = paste(profanityFile, collapse='|')
    
    # Note: Doing gsub before saving to disk messes up the file, not sure why.
    out <- stri_trim(tolower(
        gsub("([0-9]|[^[:alnum:][:space:]['-]|[QA]:|([AaPp].[Mm].))+",
             '', f)))
    # I should probably ignore articles with swear words instead, in the future
    out <- stri_trim(gsub(profanityFilter, '', out))
    return(out)
}
# This process merges sentences together, undesirable but okay for now.
# I still think I should separate out each sentence into its own line.
# It would give the correct grammar and reduce the no. of silly phrases.

#__________________________________________________________
WordFrequencyDT <- function(tokens, NGrams = 1){
    # Takes a tokenized Corpus with punctuation and hyphens removed
    # Converts it to a Document Frequency Matrix
    # Then returns a word-frequency data table with single phrases removed
    # The quanteda package is much faster than tm
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

#_____________________________________________________________________
# Actually make the training / test etc. sets and n-grams now
outTrain <- readAndClean("US_NewsBlogsSampleTrain.txt")
outTest <- readAndClean("US_NewsBlogsSampleTest.txt")

Corpus = corpus(outTrain)
Tokens = tokenize(Corpus,
                  removeHyphens=TRUE,
                  removePunct=TRUE)

unigrams = WordFrequencyDT(Tokens, 1)
bigrams = WordFrequencyDT(Tokens, 2)
trigrams = WordFrequencyDT(Tokens, 3)
quadgrams = WordFrequencyDT(Tokens, 4)

#_________________

write.table(unigrams,"unigrams.txt",row.names=FALSE)
write.table(bigrams,"bigrams.txt",row.names=FALSE)
write.table(trigrams,"trigrams.txt",row.names=FALSE)
write.table(quadgrams,"quadgrams.txt",row.names=FALSE)

unigrams = fread("unigrams.txt", header=T)
bigrams = fread("bigrams.txt", header=T)
trigrams = fread("trigrams.txt", header=T)
quadgrams = fread("quadgrams.txt", header=T)

setkey(unigrams, term)
setkey(bigrams, term)
setkey(trigrams, term)
setkey(quadgrams, term)

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
regexPhrase <- function(wordlist, lastCharIsSpace=FALSE) {
    restoredPhrase <- paste(wordlist, '', collapse='')
    regex = paste('^', restoredPhrase, sep='')
    if(lastCharIsSpace == FALSE){
        regex = stri_sub(regex, from=1, to=-2)
    }
    return(regex)
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
# This acts to autocomplete a word instead.
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
            regex = regexPhrase(lastXWords, lastCharIsSpace=isSpace)
            searchIndices = grep(regex, nGramList(numWords)$term)
            if(length(searchIndices) > 0){
                numOfRows = min(length(searchIndices), numPreds) # limit rows returned to < 5
                predsFound = nGramList(numWords)[searchIndices,][order(-occurrences)][1:numOfRows,]
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
    if(nrow(preds)==0){
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

testPhrase <- function(wordList, i){
    if(wordList[[1]] == ''){
        return(NULL)
    }
    phrase = paste(c(wordList[1:5], ''), collapse = ' ') 
    correctWord = wordList[6]
    #print("The phrase used is:")
    #print(phrase)
    #print('The correct word is:')
    #print(correctWord)
    wordPreds <- topXPreds(phrase, i)
    #print('The word predictions are:')
    #print(wordPreds)
    #print("_________________________________________")
    return(correctWord %in% wordPreds$word)
}

predictionAccuracy <- function(dataset, nSamples, nPreds=3, seed=123){
    set.seed(seed)
    selection = sample(1:length(dataset), nSamples)
    correct = 0
    skipped = 0
    total = 0
    for(i in selection){
        #a = as.numeric(Sys.time())*1000
        words = convertPhrase(dataset[i])
        phrase = testPhrase(words, nPreds)
        if(is.null(phrase)){
            skipped = skipped + 1
            #print("Phrase wasn't long enough.")
            next
        }
        if(phrase == TRUE){
            correct = correct + 1
        }
        total = total + 1
        #print(as.numeric(Sys.time())*1000 - a, digits=15)
    }
    accuracy = round(correct*100/total, digits = 2)
    
    print(paste(c("Results based off of the top",
                  nPreds,
                  "word predictions"),
                collapse=' '))
    print(rbind(c('skipped', 'correct', 'total', 'accuracy (%)'),
                c(skipped, correct, total, accuracy)))
}

sentence = "This looks like a piece of cake to"

sentence = "It's time to eat a piece of c"
preds = topXPreds(sentence, 5)
preds

# speed of predictions is limited to grep search speed
# if we need to traverse multiple n-gram lists, the search is longer
# My code can test 3-4 phrases/second, which is stupidly slow
print("Top 5 prediction accuracy over 1000 samples")
predictionAccuracy(outTest, 1000, nPreds=5)
print("Top 3 prediction accuracy over 1000 samples")
predictionAccuracy(outTest, 1000)
print("Top 1 prediction accuracy over 1000 samples")
predictionAccuracy(outTest, 1000, nPreds=1)
