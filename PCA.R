library(dplyr)
library(caret)
library(FactoMineR)
library(factoextra)
library(ggplot2)

#..............Load data

df <- read.csv("data/cleaned_v2.csv")
dim(df)
head(df)
str(df)

cat_col_names<- c("Agency_type","Month","Crime_type","Crime_status","Victim_sex",
                  "Victim_race","Offender_sex","Offender_race","Weapon","Relationship","Crime_cause","County","Offender_demo") 

df <- df %>%
  mutate(across(all_of(cat_col_names), as.factor))

#Drop the target variable and offender age and sex
df_input<- subset(df,select = -c(Offender_demo,Offender_sex,Offender_age))


#.................................PCA......................#

#Encode the categorical/factor variables
dummy<- dummyVars(" ~ .", data=df_input)

encoded_df<- data.frame(predict(dummy,newdata=df_input))
head(encoded_df)
View(encoded_df)

pc_crime <- prcomp(encoded_df, center = T, scale. = T,rank. = 10)

# Generate summary of the PEV
summary(pc_crime)

# Obtained explained variance
pc_crime_var <- pc_crime$sdev^2
pc_crime_var_10 <- pc_crime_var[1:10]
pc_crime_var_10

# Generate proportion of variance Explained(PVE) 
pve_crime <- pc_crime$sdev^2/ sum(pc_crime$sdev^2)
pve_crime


# Plot the Variance plot
var_df<- data.frame(PC= paste0("PC",1:10),
                    EV = pve_crime[1:10]*100)

var_df %>%
  ggplot(aes(reorder(PC,-EV), EV)) + 
  geom_col()+
  xlab("Principal Component") + 
  ylab("Percenatage of Variance Explained") +
  ggtitle("PCA : Variance Plot")


#plot PVE - scree plot
opar<- par(no.readonly = TRUE)
plot(pve_crime[1:10],
     main = "Proportion of Variance Explained",
     ylim = c(0,0.01),
     xlab = "PC",
     ylab = "PVE",
     type = "o",
     pch = 20)


# Plot the cumulative PVE
opar<- par(no.readonly = TRUE)
plot(cumsum(pve_crime[1:10]),
     ylim = c(0,1),
     xlab = "PC",
     ylab = "Cumulative PEV",
     type = "o",
     pch = 20)
abline(h=0.85,col="red",lty="dashed")


#Plot the biplot
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,2))
biplot(
  pc_crime,
  scale = 0,
  col = c('grey40','orange')
)
biplot(
  pc_crime,
  choices = c(1,3),
  scale = 0,
  col = c('grey40','orange')
)
biplot(
  pc_crime,
  choices = c(2,3),
  scale = 0,
  col = c('grey40','orange')
)
par(opar)


#PCA loadings
pc_crime_loadings <- pc_crime$rotation
pc_crime_loadings

#PCA scores
scores_df<- pc_crime$x
scores_df




#.............Alternative to prcomp()
#................Factor Analysis of Mixed Data...................#

# set graph = FALSE if the plots are not needed
crime_famd <- FAMD(df_input,graph=TRUE)  


# Plot FAMD variance plot
fviz_screeplot(crime_famd)


# Plot the PVE plot
fviz_eig(crime_famd,geom = 'line')


# Generate cumulative PVE
eig.val <- get_eigenvalue(crime_famd)  # variance summary of famd
eig.val <- as.data.frame(eig.val)
famd_cumsum<- eig.val$cumulative.variance.percent

# Plot the cumulative PVE
opar<- par(no.readonly = TRUE)
plot(famd_cumsum,
     ylim = c(0,1),
     xlab = "PC",
     ylab = "Cumulative PEV",
     type = "o",
     pch = 20)
abline(h=0.85,col="red",lty="dashed")


#To generate the PCA scores
head(crime_famd$ind$coord)
fmd_Scores <- crime_famd$ind$coord
#write.csv(asfmd_Scores,"fmd_scores.csv")


# To generate the squared loadings of the variables
crime_famd$var$coord
fmd_loadings <- crime_famd$ind$coord
#write.csv(as.data.frame(fmd_Scores),"fmd_loadings.csv")


