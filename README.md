# Analyzing Credits to Determine Loan Sanction Prediction

## Methodology 
- Based on our analysis and variable selection process, we have identified 11 attributes deemed crucial and 5 as insignificant among 16 attributes present. 
- Later as our data contains both numerical and categorical variables,we have  decided to use tree based methods to carry out our research. We initially used decision trees utilizing those crucial variables  which achieved us an accuracy of 73%. 
- We also have experimented with random forest, employing 10,000 trees, resulting in a 73% accuracy, slightly below the model utilizing all attributes. 
- Then we have  implemented bagging as it uses multiple independent learners to reduce the sensitivity of the model to training data and the variance of the model along with improving the stability of the model, which yielded us a consistent accuracy of 74%, matching the performance of the model with all attributes included.
