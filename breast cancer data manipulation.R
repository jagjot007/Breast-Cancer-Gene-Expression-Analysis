#gene expression data manipulation
getwd()
#setwd("~/Desktop/data science /breast cancer project")

#installing libraries
# GEOquery library
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("GEOquery")

#dplyr and tidyverse
install.packages("dplyr")
install.packages("tidyverse")

#loading the libraries
library(dplyr)
library(tidyverse)
library(GEOquery)

#reading the GEOdataset file (NCBI) on breast cancer
data = read.csv(file ="/Users/jagjotarora/Desktop/data science /breast cancer project/GSE183947_fpkm.csv")
dim(data)

#we require the meta data to distinguish between the tumor samples and normal samples
#meta data files can be obtained directly from NCBI but requires multiple steps to get the tabular form
#using GEOquery to get meta data
gse = getGEO(GEO ="GSE183947", GSEMatrix = TRUE)  
#if gives error, could be due to size of connection buffer. correct the error via -
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 1000)

gse

metadata = pData(phenoData(gse[[1]]))    #refer to GEOquery tutorial for understanding
head(metadata)

#selecting only desired columns from metadata 
#this step can be done in mySQL too, However it is shown here in R
metadata.subset = select(metadata, c(1,10,11,17))
#or better do 
metadata %>%
  select(1,10,11,17) %>%
  rename(tissue = characteristics_ch1) %>%
  rename(metastasis = characteristics_ch1.1) %>%
  mutate(tissue = gsub("tissue: ","", tissue)) %>%
  mutate(metastasis = gsub("metastasis: ","", metastasis)) %>%
  head()

#using the head after all the changes lets us visualize that the changes done have been actually implemented or not
#once confirmed, we can save these changes in a new variable
metadata.modified = metadata %>%
  select(1,10,11,17) %>%
  rename(tissue = characteristics_ch1) %>%
  rename(metastasis = characteristics_ch1.1) %>%
  mutate(tissue = gsub("tissue: ","", tissue)) %>%
  mutate(metastasis = gsub("metastasis: ","", metastasis))
  

#now our metadata is resolved, we add it to ou gene expression data
head(data)
#reshaping gene expression data from wide format to the long format
#it is easier to work with long format
data.long = data %>%
  rename(gene = X) %>%
  gather(key = "samples", value = "FPKM", -gene)
  #storing all the values in a container called samples. we exclude the genes column from this.
  
#join the data frames - GE data and metadata (can be done in SQL too)
data.long %>%
  left_join(., metadata.modified, by = c("samples" = "description")) %>%
  head()
#the "." represents the data.long which was used before the pipe
# we save this join in data.long
data.long = data.long %>%
  left_join(., metadata.modified, by = c("samples" = "description"))
  
#data exploration
data.long %>%
  filter(gene == "BRCA1" | gene == "BRCA2") %>%
  group_by(gene, tissue) %>%
  summarise(mean_FPKM = mean(FPKM),
            median_FPKM = median(FPKM)) %>%
  #the mean FPKM values are put in column name mean_FPKM
  arrange(-mean_FPKM)
