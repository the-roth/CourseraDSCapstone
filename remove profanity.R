library(stringi)

readFile <- function(f){
    con = file(f, 'rb')
    output = readLines(con, encoding="UTF-8", skipNul=TRUE)
    close(con)
    output
}

p = readFile("profanity all words.txt")
q = strsplit(p, ":1,")
q = gsub('"', '', q)
q = gsub('[a-z]*[0-9!+.:_]+[a-z]*', '', q)
r = q[!q=='']

write.table(r,"profanity2.txt",row.names=FALSE,
            col.names=FALSE,quote=FALSE,append=FALSE)

profanityFile2 = readFile("profanity2.txt")
profanityFilter2 = paste(profanityFile2, collapse='|')
profanityFilter2
