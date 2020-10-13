#Load the following libraries:

install.packages("lubridate")
library(openxlsx)
library(dplyr)
library(fastDummies)
library(lubridate)
library(stringr)
library(data.table)



#Load the ecommerce revenue data:
ecomm1= fread("data/ecomm_revenue.csv")

################################ Data Processing: ################################

#create a date variable in the ecomm/online dataset:
online$date= ymd(online$Date)

#Cleaning up DMA names: (This is required as we also load population data and the DMA names need to match across the ecomm and population datasets)

#read in state abbr file as metro names have states abbr and we want to remove them:
stabbr=fread("data/50_us_states_two_letter_abbreviations.csv")

#remove state abbreviations and other non-alphanumeric characters:
online$DMA= gsub(paste(stabbr$State, collapse="|"), "", online$Metro)
online$DMA= gsub(",", "", online$DMA)

#create a function to remove leading and trailing whitespace:
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

online$DMA=trim(online$DMA)

#there are additional 59 DMAs whose names don't match exactly with DMAs in population file- so load the excel file below which has the provides DMAs with the right DMA name formats:
missingdmas=fread("data/missingdmas.csv")

#run a for loop to replace dmas withing dma in missing dmas
for (i in 1:59){
  print(i)
  online$DMA[online$DMA==missingdmas$x[i]]=missingdmas$DMA_name[i]
}

#make DMAs uppercase:
online$DMA= toupper(online$DMA)

#Load population data and format some DMA names:

dmapop=read.xlsx("data/population.xlsx", sheet=1)

#check to see if there are any DMAs in the online file that are not in the population file:
setdiff(unique(online$DMA), unique(dmapop$DMA_NAME))

#join the online data with population info:
online_pop=left_join(online, dmapop, by=c("DMA"="DMA_NAME")) 

total=online_pop



#Order dates within each DMA:

#group by DMA and order dates within each group/DMA (ordering by date is important when calculating correlation between the records of 2 DMAs):
data= total %>% group_by(total$DMA) %>% arrange(total$date, .by_group = TRUE) %>% ungroup() %>% dplyr::select(date, DMA, Revenue, Population)



#Each DMA has records only when there is non-zero revenue. We need to fill in for dates where they saw no revenue, so that the correlation can be calculated between each DMA-date combination.

library(tidyr)

#not all dates exist- add rows for all dates
all_dates_dma <- expand(data, nesting(DMA), date) 
complete_data=merge(data, all_dates_dma, by=c("DMA", "date"), all=TRUE)

detach("package:tidyr", unload=TRUE)

#check to see that all DMAs have the same number of rows- they should because we have included all dates
complete_data %>% group_by(DMA) %>% summarise(n=n())

#check for NAs- population and revenue have NAs now because we included all dates
colSums(is.na(complete_data))

#fill in missing population values for each DMA:
library(tidyr)
complete_data=complete_data %>% 
  group_by(DMA) %>% 
  fill(Population) %>% #default direction down
  fill(Population,.direction = "up")

detach("package:tidyr", unload=TRUE)

#making missing revenues=0
complete_data$Revenue[is.na(complete_data$Revenue)==TRUE]=0

#check for NAs 
colSums(is.na(complete_data))


#Map DMAs to US Census regions:


#load master mapping file. This file tells us all the states that could be within a DMA. Eventually, we will map from states to US Census regions. 
mapping=read.xlsx("data/Master_State_DMA_Mapping.xlsx", sheet=1)


#DMA names in the master mapping file don't match exactly with our DMA names.
#create a function that does a fuzzy string match:
f<-function (a, blist)  {
  adist(a, blist)
  which.min(adist(a, blist)) } #you should care about ties.
idx=sapply(unique(mapping$DMA.NAME), f, unique(complete_data$DMA))


#create a new column with the fuzzy string match from complete_data:
for (i in 1:209){
  print(i)
  mapping$DMA[mapping$DMA.NAME==names(idx)[i]]= unique(complete_data$DMA)[idx[[i]]]
}


#check names
mapping %>% group_by(DMA.NAME) %>% summarise(DMA=unique(DMA))


#Import region data from US Census. There are 4 major regions:

#creating region vectors which show us the states within these regions:
reg1= c("Connecticut", "Maine" , "Massachusetts", "New Hampshire" , "Rhode Island" , "Vermont", "New Jersey", "New York", "Pennsylvania")
reg2= c("Indiana","Illinois", "Michigan", "Ohio","Wisconsin","Iowa", "Nebraska", "Kansas", "North Dakota", "Minnesota", "South Dakota","Missouri")
reg3= c("Delaware", "District of Columbia", "Florida","Georgia","Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
reg4=c("Arizona", "Colorado", "Idaho" , "New Mexico", "Montana", "Utah", "Nevada", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")  


#create a column with which shows all the states in a given DMA:
mapping= mapping %>% select(DMA, StateName) %>% group_by(DMA) %>% summarise(states= paste(unique(StateName), collapse="_")) %>% ungroup()

#create empty columns (to be filled):
mapping$region1=rep(NA, nrow(mapping))
mapping$region2=rep(NA, nrow(mapping))
mapping$region3=rep(NA, nrow(mapping))
mapping$region4=rep(NA, nrow(mapping))


#loop through states in the mapping file to find region matches in the region vectors we created above:
for(i in 1:length(mapping$states)){
  print(i)
  mapping$region1[i]= ifelse(sum(grepl(paste(unlist(strsplit(mapping$states[i], "_")), collapse="|"), (reg1 )))>0, 1, 0)
  mapping$region2[i]= ifelse(sum(grepl(paste(unlist(strsplit(mapping$states[i], "_")), collapse="|"), (reg2 )))>0, 1, 0)
  mapping$region3[i]= ifelse(sum(grepl(paste(unlist(strsplit(mapping$states[i], "_")), collapse="|"), (reg3 )))>0, 1, 0)
  mapping$region4[i]= ifelse(sum(grepl(paste(unlist(strsplit(mapping$states[i], "_")), collapse="|"), (reg4 )))>0, 1, 0)}

#sum needs to be min 1 (each DMA should be given atleast 1 region)
mapping$sum= mapping$region1+ mapping$region2+mapping$region3+mapping$region4 

#left_join complete_data with mapping to get the region details:
region_data=left_join(complete_data, mapping, by=c("DMA"="DMA"))




#The data can be subsetted to include records from region 1, 2,3, and 4. 

corr_data= region_data %>% filter(date<"2020-01-01" & region3==1)

#we have 730 days to match DMAs across:
corr_data%>% group_by(DMA)%>% summarise(n=n())



#Matching algorithm:

#get a list of online_revenue for each DMA (the elements of the list are revenue for a DMA):
DMA_revenue= lapply(seq_along(unique(corr_data$DMA)), function(i) corr_data$Revenue[corr_data$DMA==unique(corr_data$DMA)[i]]) 
names(DMA_revenue)= unique(corr_data$DMA)

# Get the pairwise combinations of names of list elements:
nms <- combn( names(DMA_revenue) , 2 , FUN = paste0 , collapse = " :&: " , simplify = FALSE )
# Make the combinations of list elements
ll <- combn( DMA_revenue , 2 , simplify = FALSE )

#compute the pairwise correlation for each combination :
cor_list= lapply(ll, function(x) cor(x[[1]] , x[[2]] ))

#create a dataframe from cor_list and name them with 'nms':
corrs=data.frame(unlist(setNames( cor_list , nms)))

#create an additional column that indicate the DMA pairs cleanly:
corrs= corrs %>% mutate(DMA1=str_split_fixed(row.names(corrs), " :&: ", 2)[,1], DMA2= str_split_fixed(row.names(corrs), " :&: ", 2)[,2]) 

#rename the columns:
colnames(corrs)= c("correlation", "DMA1", "DMA2")

corrs$dmas=  paste(corrs$DMA1, corrs$DMA2, sep=",")



#merge with population:
pairs=merge(corrs, dmapop[,c("DMA_NAME", "Population")], by.x="DMA1", by.y="DMA_NAME", all.x=TRUE)
pairs=merge(pairs, dmapop[,c("DMA_NAME", "Population")], by.x="DMA2", by.y="DMA_NAME")
pairs=rename(pairs, pop_DMA1=Population.x)
pairs=rename(pairs, pop_DMA2=Population.y)

#calculate population difference and create standardized metrics for correlation and population differences to create a composite score (range should be between -1 and 1)
pairs$pop_difference= abs(pairs$pop_DMA1- pairs$pop_DMA2)
pairs$std_pop_diff= (pairs$pop_difference - min(pairs$pop_difference))/(max(pairs$pop_difference)- min(pairs$pop_difference))
pairs$std_correlation= (pairs$correlation - min(pairs$correlation))/(max(pairs$correlation)- min(pairs$correlation))
pairs$composite_score= pairs$std_correlation- pairs$std_pop_diff

#order by the composite_score in a descending fashion:
pairs=pairs[order(desc(pairs$composite_score)),]

##rank pairs and sequentially select unique pairs by looping across DMA pairs:
x=list()

for (i in 1:dim(pairs)[1]){
  x[i]= if(length(intersect(unlist(strsplit(pairs$dmas[i], ",")), unlist(x[i-1])) >0)) {x[i-1]} else {list(c(unlist(x[i-1]), unlist(strsplit(pairs$dmas[i], ","))))
  }}

pairs$uniques= x
pairs$length=unlist(lapply(pairs$uniques, length))
pairs= pairs %>%  mutate(diff = length - lag(length))
qcorr= pairs[pairs$diff>0, ]
qcorr[1,]= pairs[1,]

#final list
qcorr








