library(dplyr)
library(readxl)
library(ip2location.io)
library(geosphere)

#import data
setwd("insert your directory")
df = read.csv("insert your csv file (main data file)")

#checking screenouts from ip2location 
no_attn = sum(df$attn == "") #if set up correctly, participants with no entries in attention check should be those who accessed study but were blocked from doing it
percent_filtered_out = (no_attn/nrow(df))*100 #make sure the rows in your df only contain your data. if importing from qualtrics please drop first 2 rows beforehand

nonuk = sum(df$country_code != "GB") #change GB to relevant country code as needed
table(df$country_code) #should give the number of people for different country codes
datacentre = sum(df$usage_type == "DCH") #should give the number of people who accessed study had datacentre use type
icloud = sum(df$domain == "icloud.com") #should give the number of people with datacentre use type who were using icloud relay
proxy = sum(df$is_proxy == "true") #should give the number of people who accessed study were using some form of proxy
table(df$is_proxy == "true", df$proxy.proxy_type) #should give the different proxy types people were using. RES is likely malicious actors

#dropping screenouts
df = subset(df, df$attn=="2") #dropping participants who failed attn check and who accessed study but were blocked.

#checking and dropping participants with bot scores <0.5
failed_bot = sum(df$Q_RecaptchaScore<0.5) #should give the number of people with bot score of less than 0.5
df = subset(df, !df$Q_RecaptchaScore<0.5) #should drop the people with bot score <0.5

#checking participants who are bots
table(df$honeypot) #should be 0. if any responses, indicative of bot and should be dropped.

#checking and dropping participants who fell for GenAI whitefont
dplyr::filter(df, grepl("high horse", whitefont)) #should give the number of people who used our whitefont term "high horse" in response to open question, item designed to catch use of GenAI. retain their responseIDs
GenAI = c("ResponseId of participants in the above line of code")

`%notin%` = function(x,y) !(x %in% y) #specify a %notin% operator since that seems like it will be useful

df = subset(df, df$ResponseId%notin%GenAI,) #drop participants we caught

#checking participants for duplicates as detected by qtrics
df[df$Q_DuplicateRespondent%in%TRUE,] #should give the number of people detected as duplicate respondents by qtrics. if exist, drop

#checking for other duplicates
n_occur = data.frame(table(df$Prolific_ID)) #create a dataframe with a list of IDs and number it occurs
n_occur[n_occur$Freq > 1,] #check if any IDs occur more than once. if so, drop

#checking and dropping for Prolific authenticity checks
prolific = read_xlsx("your Prolific demographic report.xlsx", col_names = T) #import data from Prolific with authenticity check statistics
table(prolific$`Authenticity check: Bots`) #should give the number of participants who scored high, medium, and low on Prolific auth checks
prolific[prolific$`Authenticity check: Bots`%in%"low",] #should show the Prolific IDs of participants who scored low on auth checks
df = df[df$Prolific_ID%notin%"Prolific_ID of participants in low in the above line of code",] #drop participants who scored low

#setup for ip2location.io
ip_addresses = df$IPAddress #extract IPAddresses in dataset
api_key = "API_KEY_HERE" #enter unique APIkey per account. updated 20_10_26 to remove API key.
ip_data = get_ip_data(ip_addresses, api_key) #use function to get data for the ip addresses. document date this was done as ip data is perishable

write.csv(ip_data, paste0("IP2location [name of your study] Study ", Sys.Date(), ".csv"), row.names = FALSE) #create a csv file with ip data, labelled with date as per laptop system date

ip_data = read.csv("name of the .csv file you saved above") #import the .csv file you saved above. comment out lines 53-57 (ctrl+shift+c in windows)
table(df$IPAddress) #check the number of times participants responded from the same ip address - easiest sign you have respondents from the same ip is mismatched obs length in df and ip_data

qtrics = df #create a temp dataframe for ip2location filtering

#only to be used if duplicate ips exist. if so, highlight codes lines 65-73 and uncomment them (ctrl+shift+c on windows)
# ip_occur = data.frame(table(df$IPAddress)) #create a dataframe with a list of IPs and number it occurs
# ip_occur[ip_occur$Freq > 1,] #check if any IPs occured more than once - yes, retain their IPs
# duplicate_ips = c("IP Addresses from above") #create a vector to store the duplicate ips
# qtrics = qtrics %>% mutate( #create a row col to keep track of which row duplicates are in
#   row = row_number()
# )
# qtrics = relocate(qtrics, row) #relocate row to front
# qtrics[qtrics$IPAddress%in%duplicate_ips,] #checking which rows duplicates are in. for the purpose of running the ip2location.io checks we will drop from the temp df but not full df, drop the second entry for each ip
# qtrics = qtrics[-c("row numbers from line above"),] #drop duplicate ips
ip_data_ordered = ip_data[match(qtrics$IPAddress, ip_data$ip), ] #reorder ip_data to match qtrics file. added 20_10_26 since we discovered ip2location.io scrambles ip addresses sometimes

#ip2location.io part 1: compare geolocation discrepancies
qtrics_geo = data.frame(qtrics$LocationLongitude, qtrics$LocationLatitude) #extract qtrics geolocation estimates to dataframe
ip_data_geo = data.frame(ip_data_ordered$longitude, ip_data_ordered$latitude) #extract ip2location.io geolocation estimates to dataframe. updated 20_10_26 to match line 74.

colnames(qtrics_geo) = c("longitude", "latitude") #set colnames for ease in function
colnames(ip_data_geo) = c("longitude", "latitude") #set colnames for ease in function

qtrics_geo$longitude = as.numeric(qtrics_geo$longitude) #convert to numeric
qtrics_geo$latitude = as.numeric(qtrics_geo$latitude) #convert to numeric

compute_geo = function(df1, df2){ #create a function that iterates over 2 dataframes to compute distance
  if (nrow(df1) != nrow(df2)) { #check that both dataframes have the same number of rows
    stop("Dataframes must have the same number of rows")
  }
  
  if (ncol(df1) < 2 || ncol(df2) < 2) { #check that both dataframes have at least 2 cols
    stop("Both dataframes must have at least 2 columns (longitude, latitude)")
  }
  
  distances = sapply(1:nrow(df1), function(i) { #iterate over each row
    distVincentyEllipsoid( #apply the distVincentyEllipsoid function to calculate geolocation estimate differences
      c(df1[i, "longitude"], df1[i, "latitude"]), #for the first element in function use longitude and latitude from df1
      c(df2[i, "longitude"], df2[i, "latitude"])  #for the second element in function use longitude and latitude from df2
    )
  })
  return(distances/1000) #output calculation with conversion to km
}

qtrics$geo_comparison = compute_geo(qtrics_geo, ip_data_geo) #save the output to a dataframe

qtrics$geo_comparison >250 #check how many participants have an estimated >250km difference between geolocation estimates. 
sum(qtrics$geo_comparison >250)
qtrics$geo_comparison >500 #check how many participants have an estimated >500km difference between geolocation estimates. 
sum(qtrics$geo_comparison >500)
qtrics$geo_comparison >600 #check how many participants have an estimated >600km difference between geolocation estimates. 
sum(qtrics$geo_comparison >600)

#ip2location.io part 1.1: drop geolocation fails
qtrics$geo_exclude = qtrics$geo_comparison>500 #create a new column with a logical variable of whether participant would be dropped. if your criteria is not 500km of distance, change value
qtrics[qtrics$IPAddress%in%duplicate_ips,] #check if any of the duplicate ips were dropped here - if no, then no issue, if yes, make sure you drop both participants later
geo_exclude = subset(qtrics, qtrics$geo_exclude == "TRUE") #create a temporary dataframe which consists of the info of the participants we want to drop
geo_fails = geo_exclude$ResponseId #store the ResponseIDs in vector
df = subset(df, df$ResponseId%notin%geo_fails) #drop geolocation criteria fails

#ip2location.io part 2: check country code
country = data.frame(ip_data$country_code, ip_data$country_name) #create a dataframe for country data
table(country) #should give the country code and name you specified, if not, then drop
country_exclude = subset(ip_data, ip_data$country_code != "your country code") #create temp df for country fails
country_fails = country_exclude$ip #store IP in vector

#ip2location.io part 2.1: drop country code fails
df = subset(df, df$IPAddress%notin%country_fails) #drop country criteria fails

#ip2location.io part 3: check proxy use
proxy = ip_data[ip_data$is_proxy%in%TRUE,] #should give number of participants whose ip is associated with proxy use but didn't get filtered out
proxy_exclude = subset(ip_data, ip_data$is_proxy == "TRUE") #create temp df for proxy fails
proxy_fails = proxy_exclude$ip #store the IPs in vector

#ip2location.io part 3.1: drop proxy use fails
df = subset(df, df$IPAddress%notin%proxy_fails) #drop proxy criteria fails

#ip2location.io part 4: check usage type for datacentres
dch = ip_data[ip_data$usage_type%in%"DCH",] #should give number of participants whose ip is associated with datacentre use
dch_exclude = subset(ip_data, ip_data$usage_type=="DCH") #create temp df for DCH fails
dch_fails = dch_exclude$ip #store the IPs in vector

#ip2location.io part 4.1: drop datacentre fails
df = subset(df, df$IPAddress%notin%dch_fails) #drop datacentre criteria fails

#ip2location.io part 5: check for patterns in geolocation
geo_patterns = data.frame(geo_exclude$latitude, geo_exclude$longitude) #try to identify patterns in geolocation
country_patterns = data.frame(country_exclude$latitude, country_exclude$longitude) #try to identify patterns in geolocation
proxy_patterns = data.frame(proxy_exclude$latitude, proxy_exclude$longitude) #try to identify patterns in geolocation
dch_patterns = data.frame(dch_exclude$latitude, dch_exclude$longitude) #try to identify patterns in geolocation

patterns = rbind(geo_patterns,country_patterns,proxy_patterns,dch_patterns)