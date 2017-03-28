library(shiny)

shinyUI(
     fluidPage(
         titlePanel("Coursera Data Science Capstone - David Rothall (March 28th, 2017)"),
             sidebarLayout(
                 sidebarPanel(
                     radioButtons("variable", "Prediction Type",
                                  c("Predict Next Word" = " ",
                                    "Autocomplete Word" = ""),
                                  selected = " ",
                                  inline = FALSE,
                                  width = NULL),
                     hr(),
                     textInput("phrase", "Enter an English Phrase Here"),
                     hr(),
                     textOutput("predictions"),
                     hr(),
                     tableOutput("predTable")
                 ),
                 
                 mainPanel(
                     tabsetPanel(type = "tabs", 
                                 tabPanel("Information",
                                          h3("Word Prediction and Autocompletion"),
                                          p("This app will either predict the next word in a phrase or ",
                                            "autocomplete the current word. Try it out!"
                                            ),
                                          br(),
                                          p("Try choosing a single word, type a second word from the ",
                                            "given predictions, and another, and another... and see what ",
                                            "phrases you end with!"),
                                          br(),
                                          p("Notes:"),
                                          tags$ul(
                                              tags$li("Please wait a few seconds for the page to load."),
                                              tags$li("Any questions feel free to visit my ",
                                                      a("LinkedIn profile", href= "https://www.linkedin.com/in/davidrothall/"),
                                                      " or contact me at david.rothall@gmail.com"),
                                              tags$li("Any projects I'm working on can be found on ",
                                                a("GitHub!", href="https://github.com/the-roth"))
                                          )
                                 ),
                                 tabPanel("Documentation", 
                                          h3("Project Description"),
                                          p("This app was designed as part of the Capstone project for ",
                                            "the Data Science specialisation ",
                                            "on the Coursera website. It contains 10 courses ",
                                            "which were very interesting and engaging ",
                                            "and I'd recommend anyone considering learning Data Science ",
                                            "to study it."),
                                          p("Submitted for the Capstone was a Shiny app that predicts ",
                                            "the next word in a phrase along with a ",
                                            a("5 slide presentation ", href="http://rpubs.com/the_roth/CourseraCapstonePitch"),
                                            "highlighting its features. ",
                                            a("The dataset", href="https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"),
                                            "is from a corpus called ",
                                            a("HC Corpora", href="www.corpora.heliohost.org"),
                                            " which also contains a ",
                                            a("readme file", href="http://www.corpora.heliohost.org/aboutcorpus.html"),
                                            " for details. A sample from these (millions of) news articles, blogs ",
                                            "and Twitter posts is ",
                                            "taken as to cover as much of the English language as possible."
                                          ),
                                          h3("Extracting phrases from the dataset"),
                                          p("The sample dataset used contained roughly 6% of all blogs, news ",
                                            "and Twitter articles in the given corpus, 240000 articles in total. ",
                                            "The punctuation in the sample as well as numbers and any ",
                                            "undesired phrases (e.g. some profanity) ",
                                            "was then removed as to properly extract all words later. ",
                                            "All uni- bi- tri- and quadgrams appearing more than once in the ",
                                            "sample dataset were then extracted. ",
                                            "Please see the Improvements section for further details."
                                          ),
                                          h3("Prediction Model / Algorithm"),
                                          p("The user enters a phrase of any length, with the last 3 words being ",
                                            "extracted from the ",
                                            "phrase (fewer if the phrase is shorter). If the user checks the ",
                                            "'Predict Next Word' box, ",
                                            "the model will return the last word of any quadgrams found ",
                                            "in the sample dataset, in order of maximum likelihood.",
                                            "If the 'Autocomplete Word' box is checked, the model ",
                                            "will instead extract the last words of any trigrams in the sample ",
                                            "dataset, which then acts to autocomplete the last word in the given phrase."
                                            ),
                                          p("If less than 5 results are found in the above process then the model will ",
                                            "then consider the last 2 words of the phrase and search for results in the ",
                                            "trigram and bigram list respectively depending on the mode of ",
                                            "prediction. ",
                                            "This process if then repeated for the last phrase in the word ",
                                            "if no results are found (using bigram/unigram data). ",
                                            "If no predictions are found at all the app will tell the user ",
                                            "to try another phrase."
                                            )
                                 ),
                                 tabPanel("Improvements",
                                          h3("Improving Grammar"),
                                          p("Some styles of writing are missed in News, blogs and Twitter posts. ",
                                            "Future attempts to cover the English language could include ",
                                            "other sources geared towards flows of conversation as opposed to ",
                                            "simply relaying information from the writer to the reader."
                                          ),
                                          h3("Improving Prediction Accuracy"),
                                          p("The algorithm currently uses maximum likelihood estimates and so may ",
                                            "not be as accurate with phrases not found in the quadgram list. ",
                                            "Current prediction accuracy is 15, 24 and 28% for the top 1, 3 and 5 ",
                                            "word predictions respectively. ",
                                            "Katz's backoff or Kneser-Ney smoothing models allow ",
                                            "for backoff probabilities for smaller phrases if the larger phrases ",
                                            "aren't observed. These models are likely to be more accurate."
                                            ),
                                          h3("Predicting from Multiple languages"),
                                          p("The corpus only covers English at this stage ",
                                            "however German and Russian articles are also available in the dataset.")
                                 )
                     )
                 )
             )
         )
)