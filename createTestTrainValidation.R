# BEFORE DOING ANYTHING, SET YOUR WORKING DIRECTORY TO WHERE THIS FILE IS
# OTHERWISE YOU'LL KEEP DOWNLOADING THE ZIP FILE

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

c(
    paste0("Length of newsSample: ", length(newsSample)),
    paste0("Length of blogsSample: ", length(blogsSample)),
    paste0("Length of twitterSample: ", length(twitterSample))
)

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
write.table(dataTrain,"US_NewsBlogsTwitterTrain.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

write.table(dataTest,"US_NewsBlogsTwitterTest.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

write.table(dataValidation,"US_NewsBlogsTwitterValidation.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)