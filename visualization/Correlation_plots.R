library(GoodmanKruskal)
library(dplyr)
library(GGally)
library(ggplot2)

crime_cleaned<-read.csv("data-v2/cleaned_v3.csv",stringsAsFactors = FALSE)

# Generate the heatmap for the quantitative variables
cont_col<- c('Year','Offender_age','Victim_age','add_victim_count','add_offender_count')
df_cont<- crime_cleaned[,cont_col]
ggcorr(df_cont, method = c("pairwise", "pearson"),label = TRUE,label_color = "white",low="lightblue",mid ="steelblue",high="darkblue" )+
  theme_void()+
  ggtitle("Correlation coefficients of numerical variables")


# Goodman- Kruskal tau measure for categorical variable correlation

cat_cols<- c("State","Agency_type","Month","Crime_type","Crime_status",
             "Victim_sex","Victim_race","Offender_sex","Offender_race","Weapon",
             "Relationship","Crime_cause","Offender_demo")

df_cat <- crime_cleaned[,cat_cols]


GKmat <- GKtauDataframe(df_cat)
plot(GKmat, diagSize = 0.8,colorPlot = TRUE,corrColors = "blue")