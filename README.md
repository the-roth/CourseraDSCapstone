# Coursera Capstone Project - README file
The final project for the 10 Data Science Specialization involves making a Shiny app which predicts the next word in a phrase entered by the user. My app not only does that but also has the option of autocompleting a word if the user requires that instead.

The createTestTrainValidation.R file downloads and creates the training/test datasets. The predictionModel.R file uses this training dataset to generate the n-gram/frequency lists and then creates the prediction model used in the Shiny app later on. The prediciton accuracy is also tested here (21%, 27% and 29% for the top 1, 3 and 5 word predictions respectively using the generated testing data). Everything here should be reproducible from these 2 files alone.

The Shiny app can be found [here](https://the-roth.shinyapps.io/Course-10/).

The 5 slide presentation pitch can be found [here](http://rpubs.com/the_roth/CourseraCapstonePitch).

If you have any questions or feedback, feel free to contact me at david.rothall@gmail.com.


