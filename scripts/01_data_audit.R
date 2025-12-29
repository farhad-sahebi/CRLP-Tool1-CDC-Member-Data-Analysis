#Installing the required packages
install.packages("readxl")
install.packages("dplyr") 
install.packages("skimr") 
install.packages("janitor")
install.packages("naniar") 

#Load the libraries
library(readxl)
library(dplyr)
library(skimr)
library(janitor)
library(naniar)

#Checking the WD 
getwd()

#Set WD
setwd("C:/Users/Dell/Desktop/Portfolio Projects/R Projects/1st_ CRLP Project/CRLP Tool 1 CDC Member List Aanalysis/Raw_Dataset")

#Importing the dataset
tool1_raw <- read_excel("Tool1_CDC_Members List_12324.xlsx")

#cheeking the first row
head(tool1_raw)

#Number of the rows and columns
dim(tool1_raw)

#Column names
names(tool1_raw)

#Quick structure
str(tool1_raw)

#Better overview
glimpse(tool1_raw) #comes from dplyr package

#Professional summary
skim(tool1_raw) #comes from skimr package

#Total Missing Values per column
colSums(is.na(tool1_raw)) 

#Percentage of missing per column
missing_summary <- sapply(tool1_raw, function(x) mean(is.na(x)) * 100) 
round(missing_summary, 2)
missing_summary

#Count duplicated rows
sum(duplicated(tool1_raw))

#see duplicated rows
tool1_raw %>%
  get_dupes()

#Checking the duplicated Provinces
tool1_raw %>%
  get_dupes(Member_Province)

#Province frequency
tool1_raw %>%
  count(Member_Province, sort = TRUE)

#District frequency
tool1_raw %>%
  count(Member_District, sort = TRUE)

#Gender Distribution
tool1_raw %>%
  count(Member_Gender, sort = TRUE)








