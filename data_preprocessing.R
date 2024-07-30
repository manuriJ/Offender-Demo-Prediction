
library(dplyr)
library(validate)
library(ggplot2)
library(scales)

# Data exploration and preparation

# Load the original dataset as a dataframe
df<- read.csv("data-v2/SHR65_22.csv")

# Get the dimensions of data
nrow(df)
ncol(df)

# Explore the dataframe
View(df)
head(df)

# Sub setting the original dataset
#Since we are considering only subsample of data where the crime records are from 2000 to 2022

crime_2000<- subset(df,Year >= 2000)
dim(crime_2000)

# Save the new subset into a csv for reuse in further steps
#write.csv(crime_2000,"data-v2/us_crime_2000-2022.csv",row.names = FALSE)

crime_2000<- read.csv("data-v2/us_crime_2000-2022.csv")

# Explore the variables
str(crime_2000)  # variables are correctly loaded with relevant types
summary(crime_2000)

#...........................Data preprocessing and Cleaning................................#

# 1...Remove unnecessary columns 
# ----> agency identification key,agency code,case identification by agency,file recorded date

crime_2000[, c('ID','Ori','Agency','Incident','Source','FileDate','MSA')] <- list(NULL)


# 2...Modify the 'CNTYFIPS' to create new variable to have county name where the crime happned

lst_county<- strsplit(crime_2000$CNTYFIPS,",")  # split the variable by delimiter(,)
crime_2000$County<- sapply(lst_county,`[`,1)
crime_2000$CNTYFIPS <- NULL  # drop the original


# 3...Rename the columns - rename some columns meaningfully and so that easy to understand

cols_to_rename<- c("Agentype","ActionType","Homicide","Situation","VicAge","VicSex","VicRace","VicEthnic",
                   "OffAge","OffSex","OffRace","OffEthnic","Circumstance","Subcircum","VicCount","OffCount")

rename_cols_into <- c("Agency_type","Report_status","Crime_type","Crime_status","Victim_age","Victim_sex",
                      "Victim_race","Victim_ethnicity","Offender_age","Offender_sex","Offender_race",
                      "Offender_ethnicity","Crime_cause","Victim_prior_offense_status","add_victim_count","add_offender_count")

crime_2000<- crime_2000 %>% 
  rename_with(~rename_cols_into,all_of(cols_to_rename))

# New modified column names
colnames(crime_2000)



# 4....convert the categorical variables with defined levels into factors for 
#       accurate representation and enhancing analytical usability

cat_col_names<- c("Agency_type","Solved","Month","Report_status","Crime_type","Crime_status","Victim_sex",
              "Victim_race","Victim_ethnicity","Offender_sex","Offender_race",
              "Offender_ethnicity","Weapon","Relationship","Crime_cause","Victim_prior_offense_status") 

crime_2000 <- crime_2000 %>%
  mutate(across(all_of(cat_col_names), as.factor))


#Generate frequency table for each categorical variable
crime_cat_table <- apply(crime_2000[, cat_col_names], 2, table)
crime_cat_table

#Note : 
#'Victim_sex','Victim_race','Victim_ethnicity','Offender_sex',
#''Offender_race','Offender_ethnicity' have a 'Unknown' category

# proportional tables for Victim and offender sex,race and ethnicity
cols<- c('Victim_sex','Victim_race','Victim_ethnicity','Offender_sex','Offender_race','Offender_ethnicity')
cat_prop_table <- lapply(crime_2000[,cols], function(x) prop.table(table(x)))
cat_prop_table


#Re categorize the 'unknown' categories from Victim_sex,offender sex as missing since the % is very low
crime_2000$Victim_sex[crime_2000$Victim_sex=='Unknown'] <- NA
crime_2000$Offender_sex[crime_2000$Offender_sex=='Unknown'] <- NA


# Remove the ethnicity for both offender and victim as more than 50% of data for the column is 'unknown' or missing.
crime_2000[, c('Victim_ethnicity','Offender_ethnicity')] <- list(NULL)


#Rename selected categorical variable levels due to unnecessary information or lengthy descriptions

#Weapon
crime_2000$Weapon<- dplyr::recode(crime_2000$Weapon,
                           'Asphyxiation - includes death by gas'='Suffocation',
                           'Blunt object - hammer, club, etc'='Blunt object',
                           'Firearm, type not stated'='Firearm',
                           'Handgun - pistol, revolver, etc'='Handgun',
                           'Knife or cutting instrument' = 'Knife/Blade',
                           'Narcotics or drugs, sleeping pills'= 'Narcotics',
                           'Other or type unknown'= 'Unspecified',
                           'Personal weapons, includes beating'='Personal weapons/Assaultive',
                           'Poison - does not include gas'='Poison',
                           'Pushed or thrown out window'='Pushed/thrown out window',
                           'Strangulation - hanging'='Strangulation/Hanging',
                           'Weapon Not Reported'='Unknown')
                           

#Relationship
crime_2000$Relationship<- dplyr::recode(crime_2000$Relationship,
                                 'Homosexual relationship'='Homosexual',
                                 'Relationship not determined'='Unknown',
                                 'Other - known to victim'= 'known to victim')


#Crime Cause
crime_2000$Crime_cause<- dplyr::recode(crime_2000$Crime_cause,
                                'All other manslaughter by negligence'='Unintentional manslaughter',
                                'Argument over money or property'='Arguments(money or property)',
                                'Brawl due to influence of alcohol'='alcohol-induced fights',
                                'Brawl due to influence of narcotics'='Narcotic-induced fights',
                                'Child killed by babysitter'='Babysitter fatality',
                                'Children playing with gun'='Child-involved firearm incident',
                                'Gun-cleaning death - other than self'='Accidental gun cleaning fatality',
                                'Narcotic drug laws' ='Narcotic usage',
                                'Other - not specified'='Other',
                                'Other negligent handling of gun' = 'Other negligent gun handelling',
                                'Prostitution and commercialized vice'='commercialized prostitution',
                                'Victim shot in hunting accident' = 'Hunting related',
                                'Circumstances undetermined'='Unknown')


#Victim_prior_offense_status - recode level with the empty string
levels(crime_2000$Victim_prior_offense_status)[1] <- "Not specified"
table(crime_2000$Victim_prior_offense_status)



# 5 .....Explore the numerical variables

num_col_names<- c('Victim_age','Offender_age','Year','add_victim_count','add_offender_count')

#Generate summaries for each numerical variable
crime_num_summaries <- apply(crime_2000[, num_col_names], 2, summary)
crime_num_summaries



# 6........Apply validation rules to assess the variable constraints

val_rules<- validator(ok_solved = is.element(Solved,c("Yes","No")),
                      non_neg_vicAge = Victim_age>=0 & Victim_age<100,
                      ok_vicsex= is.element(Victim_sex,c('Female','Male','Unknown')),
                      ok_vicrace= is.element(Victim_race,c("American Indian or Alaskan Native","Asian","Black","Native Hawaiian or Pacific Islander","Unknown","White")),
                      
                      non_neg_OffAge = Offender_age>5 & Offender_age<100,
                      ok_offrace= is.element(Offender_race,c("American Indian or Alaskan Native","Asian","Black","Native Hawaiian or Pacific Islander","Unknown","White")),
                      
                      ok_add_viccount = add_victim_count>=0 & add_victim_count<=10,  
                      ok_add_offcount = add_offender_count>=0 & add_offender_count<=10  
                      )
qual_check <- confront(crime_2000,val_rules)
summary(qual_check)


avc<- violating(crime_2000, qual_check["ok_add_viccount"])
avc

aofc<- violating(crime_2000, qual_check["ok_add_offcount"])
aofc


vc_age<- violating(crime_2000, qual_check["non_neg_vicAge"])
vc_age

of_age<- violating(crime_2000, qual_check["non_neg_OffAge"])
of_age



# 7......Issues identified from the validation

#Victim Age - Missing is indicated as 999 in Victim Age columns

crime_2000$Victim_age[crime_2000$Victim_age==999] <- NA
summary(crime_2000$Victim_age)

#Offender Age - Outlier detection

# Explore the records offender age >90
#crime_2000[crime_2000$Offender_age>=90, ]
nrow(crime_2000[crime_2000$Offender_age>=90 & crime_2000$Offender_age<100, ]) #289

# Explore the records offender age <5
#crime_2000[crime_2000$Offender_age<5, ]
nrow(crime_2000[crime_2000$Offender_age<5, ]) #568


# Remove implausible records for offender Age
crime_2000$Offender_age[crime_2000$Offender_age>=90 | crime_2000$Offender_age<2 ]<- NA
summary(crime_2000$Offender_age)



# 8......Missing value analysis

count_miss<- colSums(is.na(crime_2000))
count_miss

# Calculate the percentages of missing values in each column relative to the available number of data points.
perct_miss<- round(colMeans(is.na(crime_2000),2)*100)
perct_miss

# No missings excepts for the age columns generated from coded categories(999) and 'Unknown' in categorical categories



# 9.......Explore for any duplicates.

# Check for any duplicated rows/records
duplicated_df <- crime_2000[duplicated(crime_2000), ]
paste("Number of duplicated records found : ", nrow(duplicated_df)) #4472

#write.csv(duplicated_df,"data-v2/duplicated.csv")

# remove the duplicated records
crime_dedup<- unique(crime_2000)
dim(crime_dedup) 



# 10.......Remove all the missing values and create the cleaned dataset
crime_cleaned<- crime_dedup[complete.cases(crime_dedup),]
dim(crime_cleaned)



# ........Create the new target variable combining Offender age and gender for further analysis


crime_cleaned$Offender_demo <- ifelse(crime_cleaned$Offender_age<19 & crime_cleaned$Offender_sex=='Male',"<18M",
                                      ifelse(crime_cleaned$Offender_age<19 & crime_cleaned$Offender_sex=='Female',"<18F",
                                             ifelse(crime_cleaned$Offender_age<56 & crime_cleaned$Offender_sex=='Male',"19-55M",
                                                    ifelse(crime_cleaned$Offender_age<56 & crime_cleaned$Offender_sex=='Female',"19-55F",
                                                           ifelse(crime_cleaned$Offender_age<90 & crime_cleaned$Offender_sex=='Male',">56M",
                                                                  ifelse(crime_cleaned$Offender_age<90 & crime_cleaned$Offender_sex=='Female',">56F","Unknown"))))))




#convert the variable to a factor
crime_cleaned$Offender_demo <- factor(crime_cleaned$Offender_demo)


table(crime_cleaned$Offender_demo)

prop.table(table(crime_cleaned$Offender_demo))


#................Additional issues detected after EDA.............................#



# Re categorization


#Offender and Victim race
crime_cleaned$Offender_race<- dplyr::recode(crime_cleaned$Offender_race,
                                            'Asian' = 'Other',
                                            'Native Hawaiian or Pacific Islander' = 'Other',
                                            'American Indian or Alaskan Native' = 'Other',
                                            'Unknown' = 'Other')

crime_cleaned$Victim_race<- dplyr::recode(crime_cleaned$Victim_race,
                                             'Asian' = 'Other',
                                            'Native Hawaiian or Pacific Islander' = 'Other',
                                            'American Indian or Alaskan Native' = 'Other',
                                            'Unknown' = 'Other')

table(crime_cleaned$Offender_race)


#Crime_cause

gp1<-c('Felon killed by police','Felon killed by private citizen','Robbery',
       'Burglary','Motor vehicle theft','Larceny','Gangland killings','Institutional killings','Juvenile gang killings')
gp2<- c('Lovers triangle','Arguments (money or property)','Other arguments',
        'Alcohol-induced fights','Narcotic-induced fights')
gp3<- c('Rape','Other sex offense','Commercialized prostitution')                                  
gp4<- c('Arson','Narcotic usage','Gambling')
gp5<-c('Unintentional manslaughter','Accidental gun cleaning fatality','Other negligent gun handling',
       'Child-involved firearm incident','Hunting related')
gp6<-c ('Sniper attack','Babysitter fatality')

crime_cleaned$Crime_cause_old <- crime_cleaned$Crime_cause

crime_cleaned$Crime_cause<- ifelse(crime_cleaned$Crime_cause_old%in%gp1,"Crime-related killings",
                                 ifelse(crime_cleaned$Crime_cause_old%in%gp2,"Personal disputes",
                                        ifelse(crime_cleaned$Crime_cause_old%in%gp3,"Sex-related offenses",
                                               ifelse(crime_cleaned$Crime_cause_old%in%gp4,"Criminal activities",
                                                      ifelse(crime_cleaned$Crime_cause_old%in%gp5,"Accidental killings","Special circumstances")))))


table(crime_cleaned$Crime_cause)



#weapon

crime_cleaned$Weapon_old <- crime_cleaned$Weapon

wp1<-c('Firearm','Handgun','Rifle','Shotgun','Other gun')
wp3<- c('Blunt object','Personal weapons/Assaultive','Pushed/thrown out window')
wp4<- c('Strangulation/Hanging','Suffocation')
wp5<- c('Poison','Drowning','Explosives','Fire','Narcotics','Unknown')  

crime_cleaned$Weapon <- ifelse(crime_cleaned$Weapon_old%in%wp1,"Firearms",
                            ifelse(crime_cleaned$Weapon_old=='Knife/Blade',"Bladed weapons",
                                   ifelse(crime_cleaned$Weapon_old%in%wp3,"Blunt forced weapons",
                                          ifelse(crime_cleaned$Weapon_old%in%wp4,"Asphyxiation and suffocation","Other"))))

table(crime_cleaned$Weapon)


#relationship

crime_cleaned$Relationship_old <- crime_cleaned$Relationship

rg1<-c ('Boyfriend','Girlfriend','Husband','Wife','Common-law husband','Common-law wife',
        'Ex-husband','Ex-wife','Homosexual')
rg2<- c('Brother','Sister','Mother','Father','Daughter','Son','Stepdaughter',
        'Stepson','Stepfather','Stepmother','In-law','Other family')
rg3<- c('Friend','Acquaintance','Neighbor')
rg4<- c('Employee','Employer')
rg5<- c('Stranger','Known to victim','Unknown')

crime_cleaned$Relationship <- ifelse(crime_cleaned$Relationship_old%in%rg1,"Marital and Domestic",
                                   ifelse(crime_cleaned$Relationship_old%in%rg2,"Family",
                                          ifelse(crime_cleaned$Relationship_old%in%rg3,"Social",
                                                 ifelse(crime_cleaned$Relationship_old%in%rg4,"Employment","Other"))))



table(crime_cleaned$Relationship)



# Drop the variable that do not hold any significant information
# Drop variables which are re categorized with '_old'

crime_cleaned[, c('Victim_prior_offense_status','Report_status',
                  'Solved','Relationship_old','Weapon_old','Crime_cause_old')] <- list(NULL)

write.csv(crime_cleaned,"data/cleaned_v2.csv",row.names = FALSE)
