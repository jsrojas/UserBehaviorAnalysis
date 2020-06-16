# This script is intended to summarize the consumption behavior of the users by creating a new dataset where each instance is a user.
# The idea is to obtain the Users' devices IP addresses in both network and decimal format and then count the number of flows per OTT application 
# and obtain the mean values of certain attributes in order to classify the users on a certain category
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# This is to clean the memory from stored data
rm(list = ls())
rm(x)
gc()

# Load some required imports
library(foreign)
library(DMwR)
library(iptools)
library(fitdistrplus)
library(tidyverse)
library(dplyr)
require(reshape2)
require(ggplot2)
library(corrplot)

# Turn off scientific notation of numbers to show decimals
options(scipen = 999)


# Load the dataset from a csv file
dataset<- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/30-04-2019-labeled.csv")
df <- data.frame(dataset, stringsAsFactors = F)

#--------------------------------------------------------CONVERT SRC IP FROM NETWORK TO DECIMAL FORMAT AND LEAVE USERS IP ONLY--------------------------------------------------------
# Convert the src ip column to a string type instead of integer
df[, c("src_ip")] <- as.character(df[, c("src_ip")])

# Convert src ip adresses from network format to decimal format
df[, c("src_ip_numeric")] <- ip_to_numeric(df[, c("src_ip")])

# Reorder the columns to put the decimal IPs in the second column 
df <- df[c(1,49,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48)]

# Order the rows in ascending order based on the value of the src ip decimal number
df <- df[order(df[,c("src_ip_numeric")]),]

# Leave only the IP addresses of user devices - between 192.168.121.0 (3232266496 in decimal) and 192.168.129.255
df <- subset(df, src_ip_numeric >= 3232266496 & src_ip_numeric <= 3232268799)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------REPLACE NAMES OF APPLICATION LABELS -----------------------------------------------------------------------------------------------------
# Show the different levels or labels of the column
levels(df$application_name)
# Add a new level or label to the ones that the dataframe already has so it can be used as replacement
levels(df$application_name) <- c(levels(df$application_name), "104", "AJP","Amazon","AmazonVideo","Apple","AppleiCloud","AppleiTunes","ApplePush", "AppleStore","BitTorrent","CiscoSkinny", "CiscoVPN","Citrix",
                                 "Cloudflare","CNN","CSGO","DataSaver","DHCP","DNS","DNSoverHTTPS","Dropbox","eBay", "Facebook", "FTP_CONTROL","FTP_DATA","Github", "GMail","Google","GoogleDocs", "GoogleDrive",
                                 "GoogleHangoutDuo", "GoogleMaps", "GooglePlus", "GoogleServices","HTTP","HTTP_Proxy","IAX", "ICMP","IMAPS" ,"IMO","IPsec","Instagram","LDAP", "LinkedIn", "Messenger", "Microsoft",
                                 "MQTT","MS_OneDrive","MsSQL-TDS","NetBIOS", "NetFlix","NTP", "Office365", "Ookla","OpenDNS", "Oracle", "Playstation", "PlayStore","PostgreSQL", "PS_VUE", "QQ","QUIC","Radius", 
                                 "RDP","RTMP","RX","SAP","sFlow","Signal", "Sina(Weibo)", "SIP", "Skype", "Slack","SMBv1","SMBv23","SMTP","Snapchat","SNMP","SOCKS", "Spotify","SSDP","SSH","Starcraft", "Steam", 
                                 "STUN","Syslog","Targus_Dataspeed","TeamViewer","Telegram", "Teredo","TikTok","TLS", "Twitter", "UbuntuONE","Unencrypted_Jabber","Unknown", "Viber", "VNC","Webex", "WeChat", 
                                 "WhatsApp", "WhatsAppFiles", "Wikipedia", "WindowsUpdate", "Xbox", "Yahoo", "YouTube")

# Replace the label names with the ones just added
df$application_name[df$application_name=="DNS.Amazon"] <- "Amazon"
df$application_name[df$application_name=="DNS.AmazonVideo"] <- "AmazonVideo"
df$application_name[df$application_name=="DNS.Apple"] <- "Apple"
df$application_name[df$application_name=="DNS.AppleiCloud"] <- "AppleiCloud"
df$application_name[df$application_name=="DNS.AppleiTunes"] <- "AppleiTunes"
df$application_name[df$application_name=="DNS.ApplePush"] <- "ApplePush"
df$application_name[df$application_name=="DNS.AppleStore"] <- "AppleStore"
df$application_name[df$application_name=="DNS.CNN"] <- "CNN"
df$application_name[df$application_name=="DNS.DataSaver"] <- "DataSaver"
df$application_name[df$application_name=="DNS.DNSoverHTTPS"] <- "DNSoverHTTPS"
df$application_name[df$application_name=="DNS.Dropbox"] <- "Dropbox"
df$application_name[df$application_name=="DNS.eBay"] <- "eBay"
df$application_name[df$application_name=="DNS.Facebook"] <- "Facebook"
df$application_name[df$application_name=="DNS.Github"] <- "Github"
df$application_name[df$application_name=="DNS.GMail"] <- "GMail"
df$application_name[df$application_name=="DNS.Google"] <- "Google"
df$application_name[df$application_name=="DNS.GoogleDocs"] <- "GoogleDocs"
df$application_name[df$application_name=="DNS.GoogleDrive"] <- "GoogleDrive"
df$application_name[df$application_name=="DNS.GoogleMaps"] <- "GoogleMaps"
df$application_name[df$application_name=="DNS.GooglePlus"] <- "GooglePlus"
df$application_name[df$application_name=="DNS.Instagram"] <- "Instagram"
df$application_name[df$application_name=="DNS.LinkedIn"] <- "LinkedIn"
df$application_name[df$application_name=="DNS.Messenger"] <- "Messenger"
df$application_name[df$application_name=="DNS.Microsoft"] <- "Microsoft"
df$application_name[df$application_name=="DNS.MS_OneDrive"] <- "MS_OneDrive"
df$application_name[df$application_name=="DNS.MSN"] <- "Messenger"
df$application_name[df$application_name=="DNS.NetFlix"] <- "NetFlix"
df$application_name[df$application_name=="DNS.Office365"] <- "Office365"
df$application_name[df$application_name=="DNS.OpenDNS"] <- "OpenDNS"
df$application_name[df$application_name=="DNS.Playstation"] <- "Playstation"
df$application_name[df$application_name=="DNS.PlayStore"] <- "PlayStore"
df$application_name[df$application_name=="DNS.QQ"] <- "QQ"
df$application_name[df$application_name=="DNS.Sina(Weibo)"] <- "Sina(Weibo)"
df$application_name[df$application_name=="DNS.Skype"] <- "Skype"
df$application_name[df$application_name=="DNS.Slack"] <- "Slack"
df$application_name[df$application_name=="DNS.Spotify"] <- "Spotify"
df$application_name[df$application_name=="DNS.Steam"] <- "Steam"
df$application_name[df$application_name=="DNS.Telegram"] <- "Telegram"
df$application_name[df$application_name=="DNS.TikTok"] <- "TikTok"
df$application_name[df$application_name=="DNS.Twitter"] <- "Twitter"
df$application_name[df$application_name=="DNS.UbuntuONE"] <- "UbuntuONE"
df$application_name[df$application_name=="DNS.Webex"] <- "Webex"
df$application_name[df$application_name=="DNS.WhatsApp"] <- "WhatsApp"
df$application_name[df$application_name=="DNS.WhatsAppFiles"] <- "WhatsAppFiles"
df$application_name[df$application_name=="DNS.Wikipedia"] <- "Wikipedia"
df$application_name[df$application_name=="DNS.WindowsUpdate"] <- "WindowsUpdate"
df$application_name[df$application_name=="DNS.Xbox"] <- "Xbox"
df$application_name[df$application_name=="DNS.Yahoo"] <- "Yahoo"
df$application_name[df$application_name=="DNS.YouTube"] <- "YouTube"
df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"
df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"
df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"
df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"
df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------COUNT THE NUMBER OF FLOWS PER OTT APPLICATION------------------------------------------------------------
# Obtain a summary of how many instances are per application label for each src ip
x <- summarise(group_by(df, src_ip, application_name), count = n())

# Another way to count the flows per OTT application with DPLYR library
x<-df%>%
  group_by(src_ip_numeric, src_ip)%>%
  count(application_name)
#-------------------------------------------------------------------------------------------------------------------------------------------------------

# Obtain the unique source IP addresses from the flows to avoid repetition in the summarization
final_dataset <- as.data.frame(unique(df$src_ip))

# Rename the column
names(final_dataset)[names(final_dataset) == "unique(df$src_ip)"] <- "src_ip"

# Convert the src ip column to a string type instead of integer
final_dataset[, c("src_ip")] <- as.character(final_dataset[, c("src_ip")])

# Convert src ip adresses from network format to decimal format
final_dataset[, c("src_ip_numeric")] <- ip_to_numeric(final_dataset[, c("src_ip")])

# Order the rows in ascending order based on the value of the src ip decimal number
final_dataset <- final_dataset[order(final_dataset[,c("src_ip_numeric")]),]

# Reorder the columns so the decimal form of the src ip is first
final_dataset <- final_dataset[c(2,1)]

# Extract the IP addresses of user devices only - between 192.168.121.0 and 192.168.129.255
final_dataset <- subset(final_dataset, src_ip_numeric >= 3232266496 & src_ip_numeric <= 3232268799)
#---------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------MEAN OCTET TOTAL COUNT--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP decimal address and application name
mean_octet_total_count.df <- as.data.frame(tapply(df$octetTotalCount, list(df$src_ip_decimal, df$application_name), mean))
# Create a new column with the corresponding IP decimal number
mean_octet_total_count.df <- cbind(src_ip_decimal = rownames(mean_octet_total_count.df), mean_octet_total_count.df)
# Set the row indexes as numbers
rownames(mean_octet_total_count.df) <- 1:nrow(mean_octet_total_count.df)
# Replace NA with 0
mean_octet_total_count.df[is.na((mean_octet_total_count.df))] <- 0
# Set the column names
colnames(mean_octet_total_count.df) <- c("src_ip_decimal", "avg_octet_total_count")
#-------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------MEAN FLOW DURATION--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP decimal address and application name
mean_flow_duration.df <- as.data.frame(tapply(df$flowDuration, list(df$src_ip_decimal, df$application_name), mean))
# Create a new column with the corresponding IP decimal number
mean_flow_duration.df <- cbind(src_ip_decimal = rownames(mean_flow_duration.df), mean_flow_duration.df)
# Set the row indexes as numbers
rownames(mean_flow_duration.df) <- 1:nrow(mean_flow_duration.df)
# Replace NA with 0
mean_flow_duration.df[is.na((mean_flow_duration.df))] <- 0
# Set the column names
colnames(mean_flow_duration.df) <- c("src_ip_decimal", "avg_octet_total_count")
#-------------------------------------------------------------------------------------------------------------------------------------------------------
