#*******************************************************************************************************************
## ANALYSE INVESTMENT TREND FOR SPARKS FUNDS INVESTMENTS CO. FROM CRUNCHBASE.COM
#*******************************************************************************************************************

# BUSINESS UNDERSTANDING
#To identify best sectors, countries and a suitable investment type for making investments.
#Invest where others are investing.
# Business Constraint #1 : Invest between 5 to 15 million USD
# Business Constraint #2 : Invest only in English-speaking countries because of the ease of communication 

# Data Understanding
# Companies.txt --A table with basic data of companies , PK = permalink
# rounds2.txt --Funding round details , PK =company_permalink
# mapping.csv -- Mapping between category names (companies.txt) to sector names (8 broad categories) - Meta table

#***********************************************************************************************************************
#EDA

#Download the required Library if not already  present 

requiredPackages = c('dplyr','stringr','tidyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)){ 
    install.packages(p)
  }
  library(p,character.only = TRUE)
}

library(dplyr)
library(stringr)
library(tidyr)

# downlaoding project data sets and other files
url_Companies <- "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
url_round2 <- "https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv"
url_mapping <- "https://cdn.upgrad.com/UpGrad/temp/231dc91c-0642-470d-a362-29ddcd7142ce/mapping.csv"

# creating and setting working directory
setwd("~/Assignment-Investment Case")

download.file(url_Companies, destfile="./companies.txt")
download.file(url_round2, destfile="./rounds2.csv")
download.file(url_mapping, destfile="./mapping.csv")

companies <- read.delim("companies.txt", header = TRUE, sep = "\t")
companies$permalink <- tolower(companies$permalink)

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# CHECKPOINT 1 *******************************************************************************************
# Merge the companies and round2 data after validation 

#How many unique companies are present in rounds2?
df <- rounds2$company_permalink
unique_comp_rounds2 <- unique(df)
length(unique_comp_rounds2)
#663368

  #How many unique companies are present in companies?
df <- companies$permalink
unique_comp_companies <- unique(df)
length(unique_comp_companies)
#663368
  
#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# company_permalink
  
# Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
# NO

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?
colnames(companies)[1] <- "company_permalink"
master_frame <- merge(rounds2, companies, by="company_permalink")
nrow(master_frame)
#114949

library(openxlsx)
write.xlsx(master_frame, file = "Master Sparks Funds.xlsx", sheetName = "my_data", append = FALSE)

# CHECKPOINT 2  Investment type analysis*****************************************************************
#choose one of the four investment types for potential investment 

#1.	Calculate the average investment amount for each of the four 
#funding types (venture, angel, seed, and private equity)
master_frame[which(is.na(master_frame$raised_amount_usd)),"raised_amount_usd"]<-0
shares <- group_by(master_frame, funding_round_type)
summarise(shares, 
          mean(raised_amount_usd, na.rm = T) )
 
# VENTURE        -  10,634,054
# ANGEL          - 764564
# SEED           - 556607
# PRIVATE EQUITY - 62,111,788

#2.	Based on the average investment amount calculated above, which investment type 
#do you think is the most suitable for Spark Funds (Investment range 5~15 million)

# VENTURE

# CHECKPOINT 3 Country Analysis**********************************************************************
# Select top nine countries which have received the highest total funding across ALL sectors for vennture investment type
# Then select top 3 English speaking countries from the chosen set of 9


#comp_venture <- master_frame[!is.na(master_frame$raised_amount_usd),]
comp_venture <- master_frame [! (master_frame$country_code==""),]
comp_venture <- subset(comp_venture, funding_round_type =="venture")
comp_venture_agg <- aggregate(comp_venture$raised_amount_usd, list(comp_venture$country_code), FUN=sum, na.rm=TRUE)
comp_venture_agg <- comp_venture_agg[order(comp_venture_agg$x ,decreasing=TRUE),]


# 2.	For the chosen investment type, make a data frame named top9 with the top nine countries 
# (based on the total investment amount each country has received)
top_9 <- head(comp_venture_agg, 9 )
top_9

# 3. Identify the top three English-speaking countries in the data frame top9.
top_country <- arrange()

# USA, GBR, IND

# CHECKPOINT 4 Sector analysis****************************************************************************

#1.	Extract the primary sector of each category list from the category_list column
#2.	Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#(Note that 'Others' is also considered one of the main sectors)


#str1 <- c("Application Platforms|Real Time|Social Network Media")
#str_split(str1, fixed("|"))[[1]][1]

#str_split ( "AB C| DEF | G", fixed("|"))[[1]][1]

str(comp_venture)

comp_venture$category_list <- as.character(comp_venture$category_list)
comp_venture$primary_sector<-str_split(comp_venture$category_list,"\\|",simplify = TRUE)[,1]

mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
 
new_mapping <- gather(mapping, main_sector, industry_val, Automotive_Sports:Social_Finance_Analytics_Advertising)
new_mapping <- new_mapping[!(new_mapping$industry_val == 0),]
new_mapping <- new_mapping[, -3]

colnames(new_mapping)[1] <- "primary_sector"
master_frame_venture <- merge( new_mapping, comp_venture, by = "primary_sector")


# CHECKPOINT 5 *******************************************************************************************
# Aim id to find find out the most heavily invested main sectors in each of the top three countries 
#(for funding type venture and investments range of 5-15 M USD

master_frame_venture1 <- master_frame_venture[!is.na(master_frame_venture$raised_amount_usd),]
master_frame_venture1$company_permalink <- as.character(master_frame_venture1$company_permalink)


#library(openxlsx)
write.xlsx(master_frame_venture1, file = "checkpoint-4.xlsx", sheetName = "my_data", append = FALSE)

#Create three separate data frames D1, D2 and D3 for each of the three countries

# COUNTRY 1 USA ********************************************************************
options("scipen"=100, "digits"=4)
D1 <- subset(master_frame_venture1, (country_code == "USA" & raised_amount_usd >=5000000 & raised_amount_usd <=15000000 ))
D1 <- group_by(D1, main_sector)
nrow(D1) #11149
sum(D1$raised_amount_usd)
max(D1$raised_amount_usd)

#D1$raised_amount_usd<-format(D1$raised_amount_usd, scientific=F)
#D1$raised_amount_usd<-as.numeric(D1$raised_amount_usd)


summary_D1= D1 %>%
  group_by(main_sector)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D1))

summary2_D1 = D1 %>%
  filter(main_sector  %in% c("Others","Cleantech_Semiconductors" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D1)) 


# COUNTRY 2 GBR ***************************************************************************


D2 <- subset(master_frame_venture1, (country_code == "GBR" & raised_amount_usd >=5000000 & raised_amount_usd <=15000000 ))
nrow(D2) #577
sum(D2$raised_amount_usd) #5028704358


summary_D2= D2 %>%
  group_by(main_sector)%>%
  summarise(sum_D2 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D2 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D2))

summary2_D2 = D2 %>%
  filter(main_sector  %in% c("Others", "Cleantech_Semiconductors" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D1 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D1 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D1))

# COUNTRY 3 IND ***************************************************************************


D3 <- subset(master_frame_venture1, (country_code == "IND" & raised_amount_usd >=5000000 & raised_amount_usd <=15000000 ))
nrow(D3) #299
sum(D3$raised_amount_usd) #2683537552


summary_D3= D3 %>%
  group_by(main_sector)%>%
  summarise(sum_D3 = sum(raised_amount_usd, na.rm=TRUE),lth_D3 = length(raised_amount_usd)) %>%
  arrange(desc(sum_D3))

summary2_D3 = D3 %>%
  filter(main_sector  %in% c("Others", "News_Search_Messaging" )) %>%
  group_by( main_sector,name)%>%
  summarise(sum_D3 = sum(raised_amount_usd, na.rm=TRUE),
            lth_D3 = length(raised_amount_usd)) %>%
  arrange( main_sector, desc(sum_D3))


D2 <- subset(master_frame_venture1, (country_code == "GBR" & raised_amount_usd >=5000000 & raised_amount_usd <=15000000 ))
D2 <- group_by(D2, main_sector)
nrow(D2) #577
summarise(D2, 
          length(raised_amount_usd, na.rm = T))

D3 <- subset(master_frame_venture1, (country_code == "IND" & raised_amount_usd >=5000000 & raised_amount_usd <=15000000 ))
D3 <- group_by(D3, main_sector)
nrow(D3) #299
summarise(D3, 
          sum(raised_amount_usd, na.rm = T))


# SUMMARY ***************************************************************************************************
#Below is the analysis of the countries and sectors for venture type investments that can be made in budget of 5-15 million USD

#TOP1 : USA
# Investment count : 11149 ; Worth : 99,66,15,24,549 USD
# Top Sectors : Others, Cleantech_Semiconductors, Social_Finance_Analytics_Advertising

#TOP2 : GBR
# Investment count : 577 ; Worth : 5,02,87,04,358 USD
# Top Sectors : Others, Cleantech_Semiconductors, Social_Finance_Analytics_Advertising

# TOP3 : IND
# Investment count : 299 ; Worth : 2,68,35,37,552 USD
# Top Sectors : Others, News_Search_Messaging, Social_Finance_Analytics_Advertising

#############################################################################################################33






















