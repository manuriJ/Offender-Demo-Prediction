### Introduction
This study is carried out with the main aim of identifying the offender characteristics by 
predicting the demographic group of the offender (based on age and gender) based on crime factors. The 
main research question for study is,
    **What are the factors associated with predicting offender demographics, specifically based on age and 
    gender, in the context of homicide cases in the United States, utilizing victim demographics, case 
    characteristics, and organizational factors?**

### Data
The dataset utilized for this study is sourced from The Murder Accountability Project (MAP), an initiative 
launched in the United States which gathers information from federal, state, and local governments on unsolved homicides and 
publishes it for public access.

**Link :** https://www.murderdata.org/p/data-docs.html

The study is conducted on an extracted version of the original data spanning from 1976 to 2022, 
consists of 870,937 crime records and 30 variables on various crime related factors. Hence, this analysis 
will only focus on the crime records spanning from 2000 to 2022, comprising 390,689 records consists of 25 categorical variables and 5 numerical variables.

### Analysis 
The study consisted of 4 steps; data preprocessing(implemented in R) , exploratory data analysis(using Python) , Principle Component Analysis(R and pySpark) and predicitive modelling of eXtreme Gradiant Boosting(R and pySpark) to predict the offender demographics to identify the characteristics of an offender.

Multiclass classification approaches were selected as the dependant variable of the study consisted of 6 categories(reflecting the age and gender aspect of the offender).