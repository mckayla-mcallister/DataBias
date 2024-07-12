# Data Bias

This project explores the potential bias of the data in adult.data. By creating models for each gender tag separately, creating one model for the combined dataset, and then comparing the accuracy of each model, we can begin to understand whether or not the different machine learning models tested locate bias within the data.

## Description

The goal was to compare different machine learning analysis methods to find out which, if any, will pick up on potential biases in the data. Ideally, a machine learning method should be at least 90% accurate. In this dataset, the models are attempting to distinguish whether or not a given person makes more than $50,000 per year. The two models tested are neural networks and decision trees. The models were created based on one protected variable at a time (in this case, there is a “male” and “female” model). That way, we have the accuracy for each value of the protected variable, and we can calculate how different they are from each other to attempt to determine if the bias is too steep for our data. It is important to remember that while methods can be biased, it is also possible that the data that we are using is biased.

Although both methods passed our test for bias, neither method passed the basic accuracy standard of 90%. This indicates that, though the data is not biased, the dataset is also not adequate enough for machine learning to create an accurate model. Perhaps integrating the weight variable (which indicates how many times similar data appeared in the original dataset) would have impacted the data in a way that allowed the model to be more accurate. I did try implementing this solution, as I suspect the data is skewed towards uncommon values, but the data frame could not handle the request to duplicate all the columns because of the size. 

## Getting Started

### Installing

* Use Git Hub to download the program files.
* Read Miniproject 3.docx to see my full analysis of the data.
* View adult.data to gain an understanding of the dataset.
* Open miniproject3.R in your favorite compatible IDE (like RStudio or Visual Studio).
* Edit line 13 to reflect where you've downloaded the files.
Example:
```
setwd("C:/Users/user/Downloads/Miniproject3")
```
### Executing program

* After setting the working directory in your miniproject3.R file, save it and run it.
* The program will update you in the console as it runs.

## Authors
McKayla McAllister
