<h1 align="center"> Correlation Between Disciplines </h1>

<h2> Summary </h2> 
___________________
<p> This project </p>
<p align="center">
    <a href= "#About">About </a> *
    <a href= "#Requirements">Requirements </a> *
    <a href= "#Problem">Problem </a> *
    <a href= "#Solution">Solution </a> *
    <a href= "#Method">Method </a> *
    <a href= "#Visualization">Visualization </a> *
    <a href= "#Conclusion">Conclusion </a> *
</p>

# About
<p> open the PCA principal components algorithm black box and understand each phase </p>

# Requirements
<p> I recommend downloading the R cran installer https://cran.r-project.org/ and later the RStudio software https://www.rstudio.com/products/rstudio/download/ to run the scripts below and view the interactions </p>

# Problem
<p> I need to understand if certain subjects in which students have low grades are correlated with each other and generate a report to the teaching direction in order to help the institution's continuous improvement </p>

# Solution
<p> Create an algorithm capable of analyzing the correlation between variables. </p>

# Method
<p> Create an unsupervised machine learning algorithm using the PCA principal component analysis method. </p>

# Visualization
<p> Correlation charts and tables </p>

# Conclusion
<p> The disciplines of cost and finance have a linear correlation. After generating a heat map, the disciplines: costs, finance and actuaries have a strong correlation with each other and after reducing the dimensionality of the variables it is possible to obtain in the first PC1 about 62.98% and in the second PC2 about 25.01%, which represents almost 88% of the original base variance. Eigenvalues, eigenvectors and commonalities were generated to understand the correlations and finally it can be shown that the discipline that has no correlation with the others is Marketing. Therefore, if the student obtains below average grades in 1 of the 3 correlated subjects, there is a chance of being affected in the others as well.  </p>