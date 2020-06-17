# This script is intended to summarize the consumption behavior of the users by creating a new dataset where each instance is a user.
# The idea is to obtain the Users' devices IP addresses in both network and decimal format and then count the number of flows per OTT application 
# and obtain the mean values of certain attributes in order to classify the users on a certain category
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# This is to clean the memory from stored data
rm(list = ls())
rm(dataset)
rm(mean_minimum_packetSize_per_app)
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
library(factoextra)
library(NbClust)
library(pryr)
library(fpc)
library(dbscan)


# Turn off scientific notation of numbers to show decimals
options(scipen = 999)

#--------------------------------------------------------LOAD THE DATASET FROM A CSV FILE--------------------------------------------------------

# Load the dataset from a csv file
dataset<- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Datasets by days/Unicauca-dataset-April-June-2019-Users_flows-SeparatedLabels.csv")

# Set the string columns as type factor so it can be handled more easily
df <- data.frame(dataset, stringsAsFactors = F)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------CONVERT SRC IP FROM NETWORK TO DECIMAL FORMAT AND LEAVE USERS IPs ONLY--------------------------------------------------------
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

# Separate strings of application labels using the dot into two columns
library(stringr)
labels <- str_split_fixed(df$application_name, "[.]", 2)
colnames(labels) <- c("application_protocol", "web_service")
df$application_name <- NULL
df <- cbind(df, labels)

write.csv(df, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Datasets by days/05-06-2019-flows-SepparatedLabels.csv", row.names = FALSE, quote = FALSE, sep=",")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------CHECK APPLICATION LABELS -----------------------------------------------------------------------------------------------------
# Show the different levels or labels of the column
levels(df$web_service)
# Show the number of applications found in the dataset
length(levels(df$web_service))

# Check the different levels or tags of a column (YouTube, Facebook, etc)
levels(df$web_service)

# Remove unused labels from the levels of the application_name column
# so it does not generate more columns in the following steps
df$application_name <- factor(df$application_name)
df$web_service <- factor(df$web_service)

summary(df$application_name)
summary(df$web_service)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------COUNT THE NUMBER OF FLOWS PER OTT APPLICATION------------------------------------------------------------
# Obtain a summary of how many instances are per application label for each src ip
library(data.table)
#flows_per_app <- dcast(setDT(df), src_ip~application_name, length)
flows_per_app <- dcast(setDT(df), src_ip~web_service, length)
head(flows_per_app)



# Set the column names in their order - 99 applications - 22-04-2020 - from 2pm until 4 pm
colnames(flows_per_app) <- c("src_ip", 
                             "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows", "AppleStore_flows",
                             "BitTorrent_flows", "CiscoVPN_flows", "Cloudflare_flows", "DataSaver_flows", "DHCP_flows", "DNS_flows", "DNSoverHTTPS_flows",
                             "Dropbox_flows", "eBay_flows", "Facebook_flows", "Github_flows", "GMail_flows", "Google_flows", "GoogleDocs_flows",
                             "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows", "H323_flows", "HotspotShield_flows",
                             "HTTP_flows", "HTTP_Proxy_flows", "ICMP_flows", "IMAPS_flows", "IMO_flows", "Instagram_flows", "IPsec_flows",
                             "IRC_flows", "LDAP_flows", "LinkedIn_flows", "Messenger_flows", "Microsoft_flows",  "Mining_flows", "MQTT_flows",
                             "MS_OneDrive_flows", "MSN_flows", "MsSQL-TDS_flows", "NetBIOS_flows_flows", "NetFlix_flows", "NTP_flows", "Office365_flows",
                             "Ookla_flows", "OpenDNS_flows", "Oracle_flows", "Pando_Media_Booster_flows", "Playstation_flows", "PlayStore_flows", "PS_VUE_flows",
                             "QQ_flows", "QUIC_flows", "RDP_flows", "RTMP_flows", "RTP_flows", "RX_flows", "Signal_flows",
                             "SIP_flows", "Skype_flows", "SkypeCall_flows", "Slack_flows", "SMBv23_flows", "SMTP_flows", "Snapchat_flows",
                             "SNMP_flows", "Spotify_flows", "SSDP_flows", "SSH_flows", "Starcraft_flows", "Steam_flows", "STUN_flows",
                             "Syslog_flows", "TeamViewer_flows", "Telegram_flows", "Teredo_flows", "TikTok_flows", "TLS_flows", "Tor_flows",
                             "Twitter_flows", "UbuntuONE_flows", "Unencrypted_Jabber_flows", "Unknown_flows", "VNC_flows", "Webex_flows", "WeChat_flows", 
                             "WhatsApp_flows", "WhatsAppCall_flows", "Whois-DAS_flows", "Wikipedia_flows", "WindowsUpdate_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")

# Set the column names in their order - 100 applications - 23-04-2020 - from 2pm until 4 pm
colnames(flows_per_app) <- c("src_ip",  
                             "104_flows","AJP_flows", "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows", "AppleStore_flows",
                             "BitTorrent_flows", "CiscoVPN_flows", "Cloudflare_flows", "DataSaver_flows", "DHCP_flows", "Direct_Download_Link_flows","DNS_flows",
                             "Dropbox_flows", "eBay_flows","eDonkey_flows", "Facebook_flows", "FTP_DATA_flows", "Github_flows", "GMail_flows", "Google_flows", "GoogleDocs_flows",
                             "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows", "H323_flows", "HotspotShield_flows",
                             "HTTP_flows", "HTTP_Proxy_flows", "ICMP_flows", "IMAPS_flows", "IMO_flows", "Instagram_flows", "IPsec_flows",
                             "LDAP_flows", "LinkedIn_flows", "Messenger_flows", "Microsoft_flows",  "Mining_flows", "MQTT_flows",
                             "MS_OneDrive_flows", "MSN_flows", "MsSQL-TDS_flows", "NetBIOS_flows_flows", "NetFlix_flows", "NTP_flows", "Office365_flows",
                             "Ookla_flows", "OpenVPN_flows", "Oracle_flows", "Pando_Media_Booster_flows", "Playstation_flows", "PlayStore_flows", "PS_VUE_flows",
                             "QQ_flows", "QUIC_flows", "Radius_flows","RDP_flows", "RTMP_flows", "RX_flows", "Signal_flows",
                             "SIP_flows", "Skype_flows", "SkypeCall_flows", "SMBv23_flows", "SMTP_flows", "Snapchat_flows",
                             "SNMP_flows", "SoundCloud_flows","Spotify_flows", "SSH_flows", "Starcraft_flows", "Steam_flows", "STUN_flows",
                             "Syslog_flows", "TeamViewer_flows", "Telegram_flows", "Teredo_flows", "TLS_flows", "Twitch_flows",
                             "Twitter_flows", "UbuntuONE_flows", "Unencrypted_Jabber_flows", "Unknown_flows", "UPnP_flows", "VNC_flows", "Webex_flows", "WeChat_flows", 
                             "WhatsApp_flows", "WhatsAppCall_flows", "Wikipedia_flows", "WindowsUpdate_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")

# Set the column names in their order - 99 applications - 26-04-2020 - from 2pm until 4 pm
colnames(flows_per_app) <- c("src_ip",  
                             "AJP_flows", "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows", "AppleStore_flows",        
                             "BitTorrent_flows", "CiscoVPN_flows", "Citrix_flows", "Cloudflare_flows", "CNN_flows", "DataSaver_flows", "DHCP_flows", "DNP3_flows",              
                             "DNS_flows", "Dropbox_flows", "Facebook_flows", "Github_flows", "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows",       
                             "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows", "HTTP_flows", "HTTP_Proxy_flows", "ICMP_flows", "IMAPS_flows",             
                             "Instagram_flows", "IPsec_flows", "LDAP_flows", "LinkedIn_flows", "Messenger_flows", "Microsoft_flows", "Mining_flows", "MQTT_flows",              
                             "MS_OneDrive_flows", "MSN_flows", "MsSQL-TDS_flows", "MySQL_flows", "NestLogSink_flows", "NetBIOS_flows", "NetFlix_flows", "NFS_flows",               
                             "NTP_flows", "Office365_flows", "Ookla_flows", "OpenDNS_flows", "Oracle_flows", "Playstation_flows", "PlayStore_flows", "QQ_flows",                
                             "QUIC_flows", "Radius_flows", "RDP_flows", "RTMP_flows", "RTP_flows", "RX_flows", "sFlow_flows", "SIP_flows",               
                             "Skype_flows", "SkypeCall_flows", "Slack_flows", "SMBv23_flows", "SMTP_flows", "SMTPS_flows", "Snapchat_flows", "SNMP_flows",              
                             "SoundCloud_flows", "Spotify_flows", "SSH_flows", "Starcraft_flows", "STUN_flows", "Syslog_flows", "TeamViewer_flows", "Telegram_flows",          
                             "Teredo_flows", "TikTok_flows", "TLS_flows", "Twitch_flows", "Twitter_flows", "UbuntuONE_flows", "Unencrypted_Jabber_flows", "Unknown_flows",           
                             "Viber_flows", "VNC_flows", "Webex_flows", "WeChat_flows", "WhatsApp_flows", "WhatsAppCall_flows", "Wikipedia_flows", "WindowsUpdate_flows",     
                             "Xbox_flows", "Yahoo_flows", "YouTube_flows")

# Set the column names in their order - 98 applications - 04-06-2020 - from 2pm until 4 pm
colnames(flows_per_app) <- c("src_ip",  
                             "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows", "AppleStore_flows",
                             "BitTorrent_flows", "CiscoVPN_flows", "Citrix_flows", "Cloudflare_flows", "DataSaver_flows", "Deezer_flows", "DHCP_flows",
                             "DNP3_flows", "DNS_flows", "DNSoverHTTPS_flows", "Dropbox_flows", "Facebook_flows", "FTP_DATA_flows", "Github_flows",
                             "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GoogleServices_flows",
                             "GTP_flows", "HTTP_flows", "HTTP_Proxy_flows", "IAX_flows", "ICMP_flows", "IMAPS_flows", "IMO_flows",                
                             "Instagram_flows","IPsec_flows", "LDAP_flows", "LinkedIn_flows", "LotusNotes_flows", "Messenger_flows", "Microsoft_flows",          
                             "MQTT_flows", "MS_OneDrive_flows", "MSN_flows", "MsSQL-TDS_flows", "NetBIOS_flows", "NetFlix_flows", "NTP_flows",                
                             "Office365_flows", "Ookla_flows", "OpenDNS_flows", "Oracle_flows", "Pando_Media_Booster_flows", "Playstation_flows", "PlayStore_flows",          
                             "QQ_flows", "QUIC_flows", "RDP_flows", "RTMP_flows", "RTP_flows", "RTSP_flows", "RX_flows",      
                             "SAP_flows", "sFlow_flows", "SIP_flows", "Skype_flows", "SkypeCall_flows", "SMBv1_flows", "SMBv23_flows",             
                             "SMTP_flows", "Snapchat_flows", "SNMP_flows", "SOMEIP_flows", "Spotify_flows", "SSDP_flows", "SSH_flows",                
                             "Steam_flows", "STUN_flows", "Syslog_flows", "TeamViewer_flows", "Telegram_flows", "Teredo_flows", "TLS_flows",                
                             "Twitch_flows", "Twitter_flows", "UbuntuONE_flows", "Unencrypted_Jabber_flows", "Unknown_flows", "VNC_flows", "WhatsApp_flows",           
                             "WhatsAppCall_flows", "Whois-DAS_flows", "Wikipedia_flows", "WindowsUpdate_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")

# Set the column names in their order - 141 applications - Complete dataset
colnames(flows_per_app) <- c("src_ip",
                             "104_flows",                  "AJP_flows",                  "Amazon_flows",               "AmazonVideo_flows",          "Apple_flows",                "AppleiCloud_flows",          "AppleiTunes_flows",          "ApplePush_flows",           
                                     "AppleStore_flows",           "BGP_flows",                  "BitTorrent_flows",           "BJNP_flows",                 "CiscoSkinny_flows",          "CiscoVPN_flows",             "Citrix_flows",               "Cloudflare_flows",          
                                     "CNN_flows",                  "DataSaver_flows",            "Deezer_flows",               "DHCP_flows",                 "Direct_Download_Link_flows", "DNP3_flows",                 "DNS_flows",                  "DNSoverHTTPS_flows",        
                                     "Dropbox_flows",              "eBay_flows",                 "eDonkey_flows",              "Facebook_flows",             "FTP_CONTROL_flows",          "FTP_DATA_flows",             "Github_flows",               "GMail_flows",               
                                     "Google_flows",               "GoogleDocs_flows",           "GoogleDrive_flows",          "GoogleHangoutDuo_flows",     "GoogleMaps_flows",           "GooglePlus_flows",           "GoogleServices_flows",       "GTP_flows",                 
                                     "H323_flows",                 "HotspotShield_flows",        "HTTP_flows",                 "HTTP_Proxy_flows",           "IAX_flows",                  "ICMP_flows",                 "IMAPS_flows",                "IMO_flows",                 
                                     "Instagram_flows",            "IPsec_flows",                "IRC_flows",                  "LDAP_flows",                 "LinkedIn_flows",             "LotusNotes_flows",           "MDNS_flows",                 "Messenger_flows",           
                                     "Microsoft_flows",            "Mining_flows",               "MQTT_flows",                 "MS_OneDrive_flows",          "MSN_flows",                  "MsSQL-TDS_flows",            "MySQL_flows",                "NestLogSink_flows",         
                                     "NetBIOS_flows",              "NetFlix_flows",              "NFS_flows",                  "NTP_flows",                  "Office365_flows",            "Ookla_flows",                "OpenDNS_flows",              "OpenVPN_flows",             
                                     "Oracle_flows",               "Pando_Media_Booster_flows",  "Playstation_flows",          "PlayStore_flows",            "POP3_flows",                 "PostgreSQL_flows",           "PS_VUE_flows",               "QQ_flows",                  
                                     "QUIC_flows",                 "Radius_flows",               "RDP_flows",                  "RTMP_flows",                 "RTP_flows",                  "RTSP_flows",                 "RX_flows",                   "SAP_flows",                 
                                     "sFlow_flows",                "Signal_flows",               "Sina(Weibo)_flows",          "SIP_flows",                  "Skype_flows",                "SkypeCall_flows",            "Slack_flows",                "SMBv1_flows",               
                                     "SMBv23_flows",               "SMTP_flows",                 "SMTPS_flows",                "Snapchat_flows",             "SNMP_flows",                 "SOCKS_flows",                "SOMEIP_flows",               "SoundCloud_flows",          
                                     "Spotify_flows",              "SSDP_flows",                 "SSH_flows",                  "Starcraft_flows",            "Steam_flows",                "STUN_flows",                 "Syslog_flows",               "Targus_Dataspeed_flows",    
                                     "TeamViewer_flows",           "Telegram_flows",             "Teredo_flows",               "TikTok_flows",               "TLS_flows",                  "Tor_flows",                  "Tuenti_flows",               "Twitch_flows",              
                                     "Twitter_flows",              "UBNTAC2_flows",              "UbuntuONE_flows",            "Unencrypted_Jabber_flows",   "Unknown_flows",              "UPnP_flows",                 "Viber_flows",                "VNC_flows",                 
                                     "Waze_flows",                 "Webex_flows",                "WeChat_flows",               "WhatsApp_flows",             "WhatsAppCall_flows",         "WhatsAppFiles_flows",        "Whois-DAS_flows",            "Wikipedia_flows",           
                                     "WindowsUpdate_flows",        "Xbox_flows",                 "Yahoo_flows",                "YouTube_flows",              "Zoom_flows")

# Count the number of flows per application - useful for graphs
df %>% count(web_service)

#-----------------------------------------------------OBTAIN THE MEAN OCTET TOTAL COUNT--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP address and application name
mean_octet_total_count.df <- as.data.frame(tapply(df$octetTotalCount, list(df$src_ip, df$web_service), mean)) # REVISAR SI ES MEJOR USAR LA COLUMNA avg_ps!!!!!
# Set the row indexes as numbers
rownames(mean_octet_total_count.df) <- 1:nrow(mean_octet_total_count.df)
# Replace NA with 0
mean_octet_total_count.df[is.na((mean_octet_total_count.df))] <- 0

head(mean_octet_total_count.df)


# Set the column names in their order - 99 applications - 22-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("Amazon_data_occupation", "AmazonVideo_data_occupation", "Apple_data_occupation", "AppleiCloud_data_occupation", "AppleiTunes_data_occupation", "ApplePush_data_occupation", "AppleStore_data_occupation",
                             "BitTorrent_data_occupation", "CiscoVPN_data_occupation", "Cloudflare_data_occupation", "DataSaver_data_occupation", "DHCP_data_occupation", "DNS_data_occupation", "DNSoverHTTPS_data_occupation",
                             "Dropbox_data_occupation", "eBay_data_occupation", "Facebook_data_occupation", "Github_data_occupation", "GMail_data_occupation", "Google_data_occupation", "GoogleDocs_data_occupation",
                             "GoogleDrive_data_occupation", "GoogleHangoutDuo_data_occupation", "GoogleMaps_data_occupation", "GooglePlus_data_occupation", "GoogleServices_data_occupation", "H323_data_occupation", "HotspotShield_data_occupation",
                             "HTTP_data_occupation", "HTTP_Proxy_data_occupation", "ICMP_data_occupation", "IMAPS_data_occupation", "IMO_data_occupation", "Instagram_data_occupation", "IPsec_data_occupation",
                             "IRC_data_occupation", "LDAP_data_occupation", "LinkedIn_data_occupation", "Messenger_data_occupation", "Microsoft_data_occupation",  "Mining_data_occupation", "MQTT_data_occupation",
                             "MS_OneDrive_data_occupation", "MSN_data_occupation", "MsSQL-TDS_data_occupation", "NetBIOS_data_occupation_data_occupation", "NetFlix_data_occupation", "NTP_data_occupation", "Office365_data_occupation",
                             "Ookla_data_occupation", "OpenDNS_data_occupation", "Oracle_data_occupation", "Pando_Media_Booster_data_occupation", "Playstation_data_occupation", "PlayStore_data_occupation", "PS_VUE_data_occupation",
                             "QQ_data_occupation", "QUIC_data_occupation", "RDP_data_occupation", "RTMP_data_occupation", "RTP_data_occupation", "RX_data_occupation", "Signal_data_occupation",
                             "SIP_data_occupation", "Skype_data_occupation", "SkypeCall_data_occupation", "Slack_data_occupation", "SMBv23_data_occupation", "SMTP_data_occupation", "Snapchat_data_occupation",
                             "SNMP_data_occupation", "Spotify_data_occupation", "SSDP_data_occupation", "SSH_data_occupation", "Starcraft_data_occupation", "Steam_data_occupation", "STUN_data_occupation",
                             "Syslog_data_occupation", "TeamViewer_data_occupation", "Telegram_data_occupation", "Teredo_data_occupation", "TikTok_data_occupation", "TLS_data_occupation", "Tor_data_occupation",
                             "Twitter_data_occupation", "UbuntuONE_data_occupation", "Unencrypted_Jabber_data_occupation", "Unknown_data_occupation", "VNC_data_occupation", "Webex_data_occupation", "WeChat_data_occupation", 
                             "WhatsApp_data_occupation", "WhatsAppCall_data_occupation", "Whois-DAS_data_occupation", "Wikipedia_data_occupation", "WindowsUpdate_data_occupation", "Xbox_data_occupation", "Yahoo_data_occupation", "YouTube_data_occupation")

# Set the column names in their order - 100 applications - 23-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("104_data_occupation","AJP_data_occupation", "Amazon_data_occupation", "AmazonVideo_data_occupation", "Apple_data_occupation", "AppleiCloud_data_occupation", "AppleiTunes_data_occupation", "ApplePush_data_occupation", "AppleStore_data_occupation",
                             "BitTorrent_data_occupation", "CiscoVPN_data_occupation", "Cloudflare_data_occupation", "DataSaver_data_occupation", "DHCP_data_occupation", "Direct_Download_Link_data_occupation","DNS_data_occupation",
                             "Dropbox_data_occupation", "eBay_data_occupation","eDonkey_data_occupation", "Facebook_data_occupation", "FTP_DATA_data_occupation", "Github_data_occupation", "GMail_data_occupation", "Google_data_occupation", "GoogleDocs_data_occupation",
                             "GoogleDrive_data_occupation", "GoogleHangoutDuo_data_occupation", "GoogleMaps_data_occupation", "GooglePlus_data_occupation", "GoogleServices_data_occupation", "H323_data_occupation", "HotspotShield_data_occupation",
                             "HTTP_data_occupation", "HTTP_Proxy_data_occupation", "ICMP_data_occupation", "IMAPS_data_occupation", "IMO_data_occupation", "Instagram_data_occupation", "IPsec_data_occupation",
                             "LDAP_data_occupation", "LinkedIn_data_occupation", "Messenger_data_occupation", "Microsoft_data_occupation",  "Mining_data_occupation", "MQTT_data_occupation",
                             "MS_OneDrive_data_occupation", "MSN_data_occupation", "MsSQL-TDS_data_occupation", "NetBIOS_data_occupation_data_occupation", "NetFlix_data_occupation", "NTP_data_occupation", "Office365_data_occupation",
                             "Ookla_data_occupation", "OpenVPN_data_occupation", "Oracle_data_occupation", "Pando_Media_Booster_data_occupation", "Playstation_data_occupation", "PlayStore_data_occupation", "PS_VUE_data_occupation",
                             "QQ_data_occupation", "QUIC_data_occupation", "Radius_data_occupation","RDP_data_occupation", "RTMP_data_occupation", "RX_data_occupation", "Signal_data_occupation",
                             "SIP_data_occupation", "Skype_data_occupation", "SkypeCall_data_occupation", "SMBv23_data_occupation", "SMTP_data_occupation", "Snapchat_data_occupation",
                             "SNMP_data_occupation", "SoundCloud_data_occupation","Spotify_data_occupation", "SSH_data_occupation", "Starcraft_data_occupation", "Steam_data_occupation", "STUN_data_occupation",
                             "Syslog_data_occupation", "TeamViewer_data_occupation", "Telegram_data_occupation", "Teredo_data_occupation", "TLS_data_occupation", "Twitch_data_occupation",
                             "Twitter_data_occupation", "UbuntuONE_data_occupation", "Unencrypted_Jabber_data_occupation", "Unknown_data_occupation", "UPnP_data_occupation", "VNC_data_occupation", "Webex_data_occupation", "WeChat_data_occupation", 
                             "WhatsApp_data_occupation", "WhatsAppCall_data_occupation", "Wikipedia_data_occupation", "WindowsUpdate_data_occupation", "Xbox_data_occupation", "Yahoo_data_occupation", "YouTube_data_occupation")

# Set the column names in their order - 99 applications - 26-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("AJP_data_occupation", "Amazon_data_occupation", "AmazonVideo_data_occupation", "Apple_data_occupation", "AppleiCloud_data_occupation", "AppleiTunes_data_occupation", "ApplePush_data_occupation", "AppleStore_data_occupation",        
                             "BitTorrent_data_occupation", "CiscoVPN_data_occupation", "Citrix_data_occupation", "Cloudflare_data_occupation", "CNN_data_occupation", "DataSaver_data_occupation", "DHCP_data_occupation", "DNP3_data_occupation",              
                             "DNS_data_occupation", "Dropbox_data_occupation", "Facebook_data_occupation", "Github_data_occupation", "GMail_data_occupation", "Google_data_occupation", "GoogleDocs_data_occupation", "GoogleDrive_data_occupation",       
                             "GoogleHangoutDuo_data_occupation", "GoogleMaps_data_occupation", "GooglePlus_data_occupation", "GoogleServices_data_occupation", "HTTP_data_occupation", "HTTP_Proxy_data_occupation", "ICMP_data_occupation", "IMAPS_data_occupation",             
                             "Instagram_data_occupation", "IPsec_data_occupation", "LDAP_data_occupation", "LinkedIn_data_occupation", "Messenger_data_occupation", "Microsoft_data_occupation", "Mining_data_occupation", "MQTT_data_occupation",              
                             "MS_OneDrive_data_occupation", "MSN_data_occupation", "MsSQL-TDS_data_occupation", "MySQL_data_occupation", "NestLogSink_data_occupation", "NetBIOS_data_occupation", "NetFlix_data_occupation", "NFS_data_occupation",               
                             "NTP_data_occupation", "Office365_data_occupation", "Ookla_data_occupation", "OpenDNS_data_occupation", "Oracle_data_occupation", "Playstation_data_occupation", "PlayStore_data_occupation", "QQ_data_occupation",                
                             "QUIC_data_occupation", "Radius_data_occupation", "RDP_data_occupation", "RTMP_data_occupation", "RTP_data_occupation", "RX_data_occupation", "sFlow_data_occupation", "SIP_data_occupation",               
                             "Skype_data_occupation", "SkypeCall_data_occupation", "Slack_data_occupation", "SMBv23_data_occupation", "SMTP_data_occupation", "SMTPS_data_occupation", "Snapchat_data_occupation", "SNMP_data_occupation",              
                             "SoundCloud_data_occupation", "Spotify_data_occupation", "SSH_data_occupation", "Starcraft_data_occupation", "STUN_data_occupation", "Syslog_data_occupation", "TeamViewer_data_occupation", "Telegram_data_occupation",          
                             "Teredo_data_occupation", "TikTok_data_occupation", "TLS_data_occupation", "Twitch_data_occupation", "Twitter_data_occupation", "UbuntuONE_data_occupation", "Unencrypted_Jabber_data_occupation", "Unknown_data_occupation",           
                             "Viber_data_occupation", "VNC_data_occupation", "Webex_data_occupation", "WeChat_data_occupation", "WhatsApp_data_occupation", "WhatsAppCall_data_occupation", "Wikipedia_data_occupation", "WindowsUpdate_data_occupation",     
                             "Xbox_data_occupation", "Yahoo_data_occupation", "YouTube_data_occupation")

# Set the column names in their order - 98 applications - 04-06-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("Amazon_data_occupation", "AmazonVideo_data_occupation", "Apple_data_occupation", "AppleiCloud_data_occupation", "AppleiTunes_data_occupation", "ApplePush_data_occupation", "AppleStore_data_occupation",
                             "BitTorrent_data_occupation", "CiscoVPN_data_occupation", "Citrix_data_occupation", "Cloudflare_data_occupation", "DataSaver_data_occupation", "Deezer_data_occupation", "DHCP_data_occupation",
                             "DNP3_data_occupation", "DNS_data_occupation", "DNSoverHTTPS_data_occupation", "Dropbox_data_occupation", "Facebook_data_occupation", "FTP_DATA_data_occupation", "Github_data_occupation",
                             "GMail_data_occupation", "Google_data_occupation", "GoogleDocs_data_occupation", "GoogleDrive_data_occupation", "GoogleHangoutDuo_data_occupation", "GoogleMaps_data_occupation", "GoogleServices_data_occupation",
                             "GTP_data_occupation", "HTTP_data_occupation", "HTTP_Proxy_data_occupation", "IAX_data_occupation", "ICMP_data_occupation", "IMAPS_data_occupation", "IMO_data_occupation",                
                             "Instagram_data_occupation","IPsec_data_occupation", "LDAP_data_occupation", "LinkedIn_data_occupation", "LotusNotes_data_occupation", "Messenger_data_occupation", "Microsoft_data_occupation",          
                             "MQTT_data_occupation", "MS_OneDrive_data_occupation", "MSN_data_occupation", "MsSQL-TDS_data_occupation", "NetBIOS_data_occupation", "NetFlix_data_occupation", "NTP_data_occupation",                
                             "Office365_data_occupation", "Ookla_data_occupation", "OpenDNS_data_occupation", "Oracle_data_occupation", "Pando_Media_Booster_data_occupation", "Playstation_data_occupation", "PlayStore_data_occupation",          
                             "QQ_data_occupation", "QUIC_data_occupation", "RDP_data_occupation", "RTMP_data_occupation", "RTP_data_occupation", "RTSP_data_occupation", "RX_data_occupation",      
                             "SAP_data_occupation", "sFlow_data_occupation", "SIP_data_occupation", "Skype_data_occupation", "SkypeCall_data_occupation", "SMBv1_data_occupation", "SMBv23_data_occupation",             
                             "SMTP_data_occupation", "Snapchat_data_occupation", "SNMP_data_occupation", "SOMEIP_data_occupation", "Spotify_data_occupation", "SSDP_data_occupation", "SSH_data_occupation",                
                             "Steam_data_occupation", "STUN_data_occupation", "Syslog_data_occupation", "TeamViewer_data_occupation", "Telegram_data_occupation", "Teredo_data_occupation", "TLS_data_occupation",                
                             "Twitch_data_occupation", "Twitter_data_occupation", "UbuntuONE_data_occupation", "Unencrypted_Jabber_data_occupation", "Unknown_data_occupation", "VNC_data_occupation", "WhatsApp_data_occupation",           
                             "WhatsAppCall_data_occupation", "Whois-DAS_data_occupation", "Wikipedia_data_occupation", "WindowsUpdate_data_occupation", "Xbox_data_occupation", "Yahoo_data_occupation", "YouTube_data_occupation")

# Set the column names in their order - 141 applications - Complete dataset
colnames(mean_octet_total_count.df) <- c("104_data_occupation",                  "AJP_data_occupation",                  "Amazon_data_occupation",               "AmazonVideo_data_occupation",          "Apple_data_occupation",                "AppleiCloud_data_occupation",          "AppleiTunes_data_occupation",          "ApplePush_data_occupation",           
                                     "AppleStore_data_occupation",           "BGP_data_occupation",                  "BitTorrent_data_occupation",           "BJNP_data_occupation",                 "CiscoSkinny_data_occupation",          "CiscoVPN_data_occupation",             "Citrix_data_occupation",               "Cloudflare_data_occupation",          
                                     "CNN_data_occupation",                  "DataSaver_data_occupation",            "Deezer_data_occupation",               "DHCP_data_occupation",                 "Direct_Download_Link_data_occupation", "DNP3_data_occupation",                 "DNS_data_occupation",                  "DNSoverHTTPS_data_occupation",        
                                     "Dropbox_data_occupation",              "eBay_data_occupation",                 "eDonkey_data_occupation",              "Facebook_data_occupation",             "FTP_CONTROL_data_occupation",          "FTP_DATA_data_occupation",             "Github_data_occupation",               "GMail_data_occupation",               
                                     "Google_data_occupation",               "GoogleDocs_data_occupation",           "GoogleDrive_data_occupation",          "GoogleHangoutDuo_data_occupation",     "GoogleMaps_data_occupation",           "GooglePlus_data_occupation",           "GoogleServices_data_occupation",       "GTP_data_occupation",                 
                                     "H323_data_occupation",                 "HotspotShield_data_occupation",        "HTTP_data_occupation",                 "HTTP_Proxy_data_occupation",           "IAX_data_occupation",                  "ICMP_data_occupation",                 "IMAPS_data_occupation",                "IMO_data_occupation",                 
                                     "Instagram_data_occupation",            "IPsec_data_occupation",                "IRC_data_occupation",                  "LDAP_data_occupation",                 "LinkedIn_data_occupation",             "LotusNotes_data_occupation",           "MDNS_data_occupation",                 "Messenger_data_occupation",           
                                     "Microsoft_data_occupation",            "Mining_data_occupation",               "MQTT_data_occupation",                 "MS_OneDrive_data_occupation",          "MSN_data_occupation",                  "MsSQL-TDS_data_occupation",            "MySQL_data_occupation",                "NestLogSink_data_occupation",         
                                     "NetBIOS_data_occupation",              "NetFlix_data_occupation",              "NFS_data_occupation",                  "NTP_data_occupation",                  "Office365_data_occupation",            "Ookla_data_occupation",                "OpenDNS_data_occupation",              "OpenVPN_data_occupation",             
                                     "Oracle_data_occupation",               "Pando_Media_Booster_data_occupation",  "Playstation_data_occupation",          "PlayStore_data_occupation",            "POP3_data_occupation",                 "PostgreSQL_data_occupation",           "PS_VUE_data_occupation",               "QQ_data_occupation",                  
                                     "QUIC_data_occupation",                 "Radius_data_occupation",               "RDP_data_occupation",                  "RTMP_data_occupation",                 "RTP_data_occupation",                  "RTSP_data_occupation",                 "RX_data_occupation",                   "SAP_data_occupation",                 
                                     "sFlow_data_occupation",                "Signal_data_occupation",               "Sina(Weibo)_data_occupation",          "SIP_data_occupation",                  "Skype_data_occupation",                "SkypeCall_data_occupation",            "Slack_data_occupation",                "SMBv1_data_occupation",               
                                     "SMBv23_data_occupation",               "SMTP_data_occupation",                 "SMTPS_data_occupation",                "Snapchat_data_occupation",             "SNMP_data_occupation",                 "SOCKS_data_occupation",                "SOMEIP_data_occupation",               "SoundCloud_data_occupation",          
                                     "Spotify_data_occupation",              "SSDP_data_occupation",                 "SSH_data_occupation",                  "Starcraft_data_occupation",            "Steam_data_occupation",                "STUN_data_occupation",                 "Syslog_data_occupation",               "Targus Dataspeed_data_occupation",    
                                     "TeamViewer_data_occupation",           "Telegram_data_occupation",             "Teredo_data_occupation",               "TikTok_data_occupation",               "TLS_data_occupation",                  "Tor_data_occupation",                  "Tuenti_data_occupation",               "Twitch_data_occupation",              
                                     "Twitter_data_occupation",              "UBNTAC2_data_occupation",              "UbuntuONE_data_occupation",            "Unencrypted_Jabber_data_occupation",   "Unknown_data_occupation",              "UPnP_data_occupation",                 "Viber_data_occupation",                "VNC_data_occupation",                 
                                     "Waze_data_occupation",                 "Webex_data_occupation",                "WeChat_data_occupation",               "WhatsApp_data_occupation",             "WhatsAppCall_data_occupation",         "WhatsAppFiles_data_occupation",        "Whois-DAS_data_occupation",            "Wikipedia_data_occupation",           
                                     "WindowsUpdate_data_occupation",        "Xbox_data_occupation",                 "Yahoo_data_occupation",                "YouTube_data_occupation",              "Zoom_data_occupation")

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------OBTAIN THE MEAN FLOW DURATION PER APPLICATION--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP decimal address and application name
mean_flow_duration.df <- as.data.frame(tapply(df$flowDuration, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_flow_duration.df) <- 1:nrow(mean_flow_duration.df)
# Replace NA with 0
mean_flow_duration.df[is.na((mean_flow_duration.df))] <- 0

head(mean_flow_duration.df)

# Set the column names in their order - 99 applications - 22-04-2020 - from 2pm until 4 pm
colnames(mean_flow_duration.df) <- c("Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration", "AppleStore_mean_flow_duration",
                                         "BitTorrent_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Cloudflare_mean_flow_duration", "DataSaver_mean_flow_duration", "DHCP_mean_flow_duration", "DNS_mean_flow_duration", "DNSoverHTTPS_mean_flow_duration",
                                         "Dropbox_mean_flow_duration", "eBay_mean_flow_duration", "Facebook_mean_flow_duration", "Github_mean_flow_duration", "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration",
                                         "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration", "H323_mean_flow_duration", "HotspotShield_mean_flow_duration",
                                         "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration", "IMO_mean_flow_duration", "Instagram_mean_flow_duration", "IPsec_mean_flow_duration",
                                         "IRC_mean_flow_duration", "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration",  "Mining_mean_flow_duration", "MQTT_mean_flow_duration",
                                         "MS_OneDrive_mean_flow_duration", "MSN_mean_flow_duration", "MsSQL-TDS_mean_flow_duration", "NetBIOS_mean_flow_duration_mean_flow_duration", "NetFlix_mean_flow_duration", "NTP_mean_flow_duration", "Office365_mean_flow_duration",
                                         "Ookla_mean_flow_duration", "OpenDNS_mean_flow_duration", "Oracle_mean_flow_duration", "Pando_Media_Booster_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration", "PS_VUE_mean_flow_duration",
                                         "QQ_mean_flow_duration", "QUIC_mean_flow_duration", "RDP_mean_flow_duration", "RTMP_mean_flow_duration", "RTP_mean_flow_duration", "RX_mean_flow_duration", "Signal_mean_flow_duration",
                                         "SIP_mean_flow_duration", "Skype_mean_flow_duration", "SkypeCall_mean_flow_duration", "Slack_mean_flow_duration", "SMBv23_mean_flow_duration", "SMTP_mean_flow_duration", "Snapchat_mean_flow_duration",
                                         "SNMP_mean_flow_duration", "Spotify_mean_flow_duration", "SSDP_mean_flow_duration", "SSH_mean_flow_duration", "Starcraft_mean_flow_duration", "Steam_mean_flow_duration", "STUN_mean_flow_duration",
                                         "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "Teredo_mean_flow_duration", "TikTok_mean_flow_duration", "TLS_mean_flow_duration", "Tor_mean_flow_duration",
                                         "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unencrypted_Jabber_mean_flow_duration", "Unknown_mean_flow_duration", "VNC_mean_flow_duration", "Webex_mean_flow_duration", "WeChat_mean_flow_duration", 
                                         "WhatsApp_mean_flow_duration", "WhatsAppCall_mean_flow_duration", "Whois-DAS_mean_flow_duration", "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")

# Set the column names in their order - 100 applications - 23-04-2020 - from 2pm until 4 pm
colnames(mean_flow_duration.df) <- c("104_mean_flow_duration","AJP_mean_flow_duration", "Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration", "AppleStore_mean_flow_duration",
                                         "BitTorrent_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Cloudflare_mean_flow_duration", "DataSaver_mean_flow_duration", "DHCP_mean_flow_duration", "Direct_Download_Link_mean_flow_duration","DNS_mean_flow_duration",
                                         "Dropbox_mean_flow_duration", "eBay_mean_flow_duration","eDonkey_mean_flow_duration", "Facebook_mean_flow_duration", "FTP_DATA_mean_flow_duration", "Github_mean_flow_duration", "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration",
                                         "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration", "H323_mean_flow_duration", "HotspotShield_mean_flow_duration",
                                         "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration", "IMO_mean_flow_duration", "Instagram_mean_flow_duration", "IPsec_mean_flow_duration",
                                         "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration",  "Mining_mean_flow_duration", "MQTT_mean_flow_duration",
                                         "MS_OneDrive_mean_flow_duration", "MSN_mean_flow_duration", "MsSQL-TDS_mean_flow_duration", "NetBIOS_mean_flow_duration_mean_flow_duration", "NetFlix_mean_flow_duration", "NTP_mean_flow_duration", "Office365_mean_flow_duration",
                                         "Ookla_mean_flow_duration", "OpenVPN_mean_flow_duration", "Oracle_mean_flow_duration", "Pando_Media_Booster_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration", "PS_VUE_mean_flow_duration",
                                         "QQ_mean_flow_duration", "QUIC_mean_flow_duration", "Radius_mean_flow_duration","RDP_mean_flow_duration", "RTMP_mean_flow_duration", "RX_mean_flow_duration", "Signal_mean_flow_duration",
                                         "SIP_mean_flow_duration", "Skype_mean_flow_duration", "SkypeCall_mean_flow_duration", "SMBv23_mean_flow_duration", "SMTP_mean_flow_duration", "Snapchat_mean_flow_duration",
                                         "SNMP_mean_flow_duration", "SoundCloud_mean_flow_duration","Spotify_mean_flow_duration", "SSH_mean_flow_duration", "Starcraft_mean_flow_duration", "Steam_mean_flow_duration", "STUN_mean_flow_duration",
                                         "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "Teredo_mean_flow_duration", "TLS_mean_flow_duration", "Twitch_mean_flow_duration",
                                         "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unencrypted_Jabber_mean_flow_duration", "Unknown_mean_flow_duration", "UPnP_mean_flow_duration", "VNC_mean_flow_duration", "Webex_mean_flow_duration", "WeChat_mean_flow_duration", 
                                         "WhatsApp_mean_flow_duration", "WhatsAppCall_mean_flow_duration", "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")

# Set the column names in their order - 99 applications - 26-04-2020 - from 2pm until 4 pm
colnames(mean_flow_duration.df) <- c("AJP_mean_flow_duration", "Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration", "AppleStore_mean_flow_duration",        
                             "BitTorrent_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Citrix_mean_flow_duration", "Cloudflare_mean_flow_duration", "CNN_mean_flow_duration", "DataSaver_mean_flow_duration", "DHCP_mean_flow_duration", "DNP3_mean_flow_duration",              
                             "DNS_mean_flow_duration", "Dropbox_mean_flow_duration", "Facebook_mean_flow_duration", "Github_mean_flow_duration", "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration",       
                             "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration", "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration",             
                             "Instagram_mean_flow_duration", "IPsec_mean_flow_duration", "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration", "Mining_mean_flow_duration", "MQTT_mean_flow_duration",              
                             "MS_OneDrive_mean_flow_duration", "MSN_mean_flow_duration", "MsSQL-TDS_mean_flow_duration", "MySQL_mean_flow_duration", "NestLogSink_mean_flow_duration", "NetBIOS_mean_flow_duration", "NetFlix_mean_flow_duration", "NFS_mean_flow_duration",               
                             "NTP_mean_flow_duration", "Office365_mean_flow_duration", "Ookla_mean_flow_duration", "OpenDNS_mean_flow_duration", "Oracle_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration", "QQ_mean_flow_duration",                
                             "QUIC_mean_flow_duration", "Radius_mean_flow_duration", "RDP_mean_flow_duration", "RTMP_mean_flow_duration", "RTP_mean_flow_duration", "RX_mean_flow_duration", "sFlow_mean_flow_duration", "SIP_mean_flow_duration",               
                             "Skype_mean_flow_duration", "SkypeCall_mean_flow_duration", "Slack_mean_flow_duration", "SMBv23_mean_flow_duration", "SMTP_mean_flow_duration", "SMTPS_mean_flow_duration", "Snapchat_mean_flow_duration", "SNMP_mean_flow_duration",              
                             "SoundCloud_mean_flow_duration", "Spotify_mean_flow_duration", "SSH_mean_flow_duration", "Starcraft_mean_flow_duration", "STUN_mean_flow_duration", "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration",          
                             "Teredo_mean_flow_duration", "TikTok_mean_flow_duration", "TLS_mean_flow_duration", "Twitch_mean_flow_duration", "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unencrypted_Jabber_mean_flow_duration", "Unknown_mean_flow_duration",           
                             "Viber_mean_flow_duration", "VNC_mean_flow_duration", "Webex_mean_flow_duration", "WeChat_mean_flow_duration", "WhatsApp_mean_flow_duration", "WhatsAppCall_mean_flow_duration", "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration",     
                             "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")

# Set the column names in their order - 98 applications - 04-06-2020 - from 2pm until 4 pm
colnames(mean_flow_duration.df) <- c("Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration", "AppleStore_mean_flow_duration",
                             "BitTorrent_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Citrix_mean_flow_duration", "Cloudflare_mean_flow_duration", "DataSaver_mean_flow_duration", "Deezer_mean_flow_duration", "DHCP_mean_flow_duration",
                             "DNP3_mean_flow_duration", "DNS_mean_flow_duration", "DNSoverHTTPS_mean_flow_duration", "Dropbox_mean_flow_duration", "Facebook_mean_flow_duration", "FTP_DATA_mean_flow_duration", "Github_mean_flow_duration",
                             "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GoogleServices_mean_flow_duration",
                             "GTP_mean_flow_duration", "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "IAX_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration", "IMO_mean_flow_duration",                
                             "Instagram_mean_flow_duration","IPsec_mean_flow_duration", "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "LotusNotes_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration",          
                             "MQTT_mean_flow_duration", "MS_OneDrive_mean_flow_duration", "MSN_mean_flow_duration", "MsSQL-TDS_mean_flow_duration", "NetBIOS_mean_flow_duration", "NetFlix_mean_flow_duration", "NTP_mean_flow_duration",                
                             "Office365_mean_flow_duration", "Ookla_mean_flow_duration", "OpenDNS_mean_flow_duration", "Oracle_mean_flow_duration", "Pando_Media_Booster_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration",          
                             "QQ_mean_flow_duration", "QUIC_mean_flow_duration", "RDP_mean_flow_duration", "RTMP_mean_flow_duration", "RTP_mean_flow_duration", "RTSP_mean_flow_duration", "RX_mean_flow_duration",      
                             "SAP_mean_flow_duration", "sFlow_mean_flow_duration", "SIP_mean_flow_duration", "Skype_mean_flow_duration", "SkypeCall_mean_flow_duration", "SMBv1_mean_flow_duration", "SMBv23_mean_flow_duration",             
                             "SMTP_mean_flow_duration", "Snapchat_mean_flow_duration", "SNMP_mean_flow_duration", "SOMEIP_mean_flow_duration", "Spotify_mean_flow_duration", "SSDP_mean_flow_duration", "SSH_mean_flow_duration",                
                             "Steam_mean_flow_duration", "STUN_mean_flow_duration", "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "Teredo_mean_flow_duration", "TLS_mean_flow_duration",                
                             "Twitch_mean_flow_duration", "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unencrypted_Jabber_mean_flow_duration", "Unknown_mean_flow_duration", "VNC_mean_flow_duration", "WhatsApp_mean_flow_duration",           
                             "WhatsAppCall_mean_flow_duration", "Whois-DAS_mean_flow_duration", "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")

# Set the column names in their order - 141 applications - Complete dataset
colnames(mean_flow_duration.df) <- c("104_mean_flow_duration",                  "AJP_mean_flow_duration",                  "Amazon_mean_flow_duration",               "AmazonVideo_mean_flow_duration",          "Apple_mean_flow_duration",                "AppleiCloud_mean_flow_duration",          "AppleiTunes_mean_flow_duration",          "ApplePush_mean_flow_duration",           
                                     "AppleStore_mean_flow_duration",           "BGP_mean_flow_duration",                  "BitTorrent_mean_flow_duration",           "BJNP_mean_flow_duration",                 "CiscoSkinny_mean_flow_duration",          "CiscoVPN_mean_flow_duration",             "Citrix_mean_flow_duration",               "Cloudflare_mean_flow_duration",          
                                     "CNN_mean_flow_duration",                  "DataSaver_mean_flow_duration",            "Deezer_mean_flow_duration",               "DHCP_mean_flow_duration",                 "Direct_Download_Link_mean_flow_duration", "DNP3_mean_flow_duration",                 "DNS_mean_flow_duration",                  "DNSoverHTTPS_mean_flow_duration",        
                                     "Dropbox_mean_flow_duration",              "eBay_mean_flow_duration",                 "eDonkey_mean_flow_duration",              "Facebook_mean_flow_duration",             "FTP_CONTROL_mean_flow_duration",          "FTP_DATA_mean_flow_duration",             "Github_mean_flow_duration",               "GMail_mean_flow_duration",               
                                     "Google_mean_flow_duration",               "GoogleDocs_mean_flow_duration",           "GoogleDrive_mean_flow_duration",          "GoogleHangoutDuo_mean_flow_duration",     "GoogleMaps_mean_flow_duration",           "GooglePlus_mean_flow_duration",           "GoogleServices_mean_flow_duration",       "GTP_mean_flow_duration",                 
                                     "H323_mean_flow_duration",                 "HotspotShield_mean_flow_duration",        "HTTP_mean_flow_duration",                 "HTTP_Proxy_mean_flow_duration",           "IAX_mean_flow_duration",                  "ICMP_mean_flow_duration",                 "IMAPS_mean_flow_duration",                "IMO_mean_flow_duration",                 
                                     "Instagram_mean_flow_duration",            "IPsec_mean_flow_duration",                "IRC_mean_flow_duration",                  "LDAP_mean_flow_duration",                 "LinkedIn_mean_flow_duration",             "LotusNotes_mean_flow_duration",           "MDNS_mean_flow_duration",                 "Messenger_mean_flow_duration",           
                                     "Microsoft_mean_flow_duration",            "Mining_mean_flow_duration",               "MQTT_mean_flow_duration",                 "MS_OneDrive_mean_flow_duration",          "MSN_mean_flow_duration",                  "MsSQL-TDS_mean_flow_duration",            "MySQL_mean_flow_duration",                "NestLogSink_mean_flow_duration",         
                                     "NetBIOS_mean_flow_duration",              "NetFlix_mean_flow_duration",              "NFS_mean_flow_duration",                  "NTP_mean_flow_duration",                  "Office365_mean_flow_duration",            "Ookla_mean_flow_duration",                "OpenDNS_mean_flow_duration",              "OpenVPN_mean_flow_duration",             
                                     "Oracle_mean_flow_duration",               "Pando_Media_Booster_mean_flow_duration",  "Playstation_mean_flow_duration",          "PlayStore_mean_flow_duration",            "POP3_mean_flow_duration",                 "PostgreSQL_mean_flow_duration",           "PS_VUE_mean_flow_duration",               "QQ_mean_flow_duration",                  
                                     "QUIC_mean_flow_duration",                 "Radius_mean_flow_duration",               "RDP_mean_flow_duration",                  "RTMP_mean_flow_duration",                 "RTP_mean_flow_duration",                  "RTSP_mean_flow_duration",                 "RX_mean_flow_duration",                   "SAP_mean_flow_duration",                 
                                     "sFlow_mean_flow_duration",                "Signal_mean_flow_duration",               "Sina(Weibo)_mean_flow_duration",          "SIP_mean_flow_duration",                  "Skype_mean_flow_duration",                "SkypeCall_mean_flow_duration",            "Slack_mean_flow_duration",                "SMBv1_mean_flow_duration",               
                                     "SMBv23_mean_flow_duration",               "SMTP_mean_flow_duration",                 "SMTPS_mean_flow_duration",                "Snapchat_mean_flow_duration",             "SNMP_mean_flow_duration",                 "SOCKS_mean_flow_duration",                "SOMEIP_mean_flow_duration",               "SoundCloud_mean_flow_duration",          
                                     "Spotify_mean_flow_duration",              "SSDP_mean_flow_duration",                 "SSH_mean_flow_duration",                  "Starcraft_mean_flow_duration",            "Steam_mean_flow_duration",                "STUN_mean_flow_duration",                 "Syslog_mean_flow_duration",               "Targus Dataspeed_mean_flow_duration",    
                                     "TeamViewer_mean_flow_duration",           "Telegram_mean_flow_duration",             "Teredo_mean_flow_duration",               "TikTok_mean_flow_duration",               "TLS_mean_flow_duration",                  "Tor_mean_flow_duration",                  "Tuenti_mean_flow_duration",               "Twitch_mean_flow_duration",              
                                     "Twitter_mean_flow_duration",              "UBNTAC2_mean_flow_duration",              "UbuntuONE_mean_flow_duration",            "Unencrypted_Jabber_mean_flow_duration",   "Unknown_mean_flow_duration",              "UPnP_mean_flow_duration",                 "Viber_mean_flow_duration",                "VNC_mean_flow_duration",                 
                                     "Waze_mean_flow_duration",                 "Webex_mean_flow_duration",                "WeChat_mean_flow_duration",               "WhatsApp_mean_flow_duration",             "WhatsAppCall_mean_flow_duration",         "WhatsAppFiles_mean_flow_duration",        "Whois-DAS_mean_flow_duration",            "Wikipedia_mean_flow_duration",           
                                     "WindowsUpdate_mean_flow_duration",        "Xbox_mean_flow_duration",                 "Yahoo_mean_flow_duration",                "YouTube_mean_flow_duration",              "Zoom_mean_flow_duration")

#-------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN NUMBER OF PACKETS SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_packetsNumber_per_app <- as.data.frame(tapply(df$pktTotalCount, list(df$src_ip, df$web_service), sum))
# Set the row indexes as numbers
rownames(mean_packetsNumber_per_app) <- 1:nrow(mean_packetsNumber_per_app)
#Replace NA with 0
mean_packetsNumber_per_app[is.na((mean_packetsNumber_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_packetsNumber_per_app) <- c("104_mean_packetsNumber",                  "AJP_mean_packetsNumber",                  "Amazon_mean_packetsNumber",               "AmazonVideo_mean_packetsNumber",          "Apple_mean_packetsNumber",                "AppleiCloud_mean_packetsNumber",          "AppleiTunes_mean_packetsNumber",          "ApplePush_mean_packetsNumber",           
                                      "AppleStore_mean_packetsNumber",           "BGP_mean_packetsNumber",                  "BitTorrent_mean_packetsNumber",           "BJNP_mean_packetsNumber",                 "CiscoSkinny_mean_packetsNumber",          "CiscoVPN_mean_packetsNumber",             "Citrix_mean_packetsNumber",               "Cloudflare_mean_packetsNumber",          
                                      "CNN_mean_packetsNumber",                  "DataSaver_mean_packetsNumber",            "Deezer_mean_packetsNumber",               "DHCP_mean_packetsNumber",                 "Direct_Download_Link_mean_packetsNumber", "DNP3_mean_packetsNumber",                 "DNS_mean_packetsNumber",                  "DNSoverHTTPS_mean_packetsNumber",        
                                      "Dropbox_mean_packetsNumber",              "eBay_mean_packetsNumber",                 "eDonkey_mean_packetsNumber",              "Facebook_mean_packetsNumber",             "FTP_CONTROL_mean_packetsNumber",          "FTP_DATA_mean_packetsNumber",             "Github_mean_packetsNumber",               "GMail_mean_packetsNumber",               
                                      "Google_mean_packetsNumber",               "GoogleDocs_mean_packetsNumber",           "GoogleDrive_mean_packetsNumber",          "GoogleHangoutDuo_mean_packetsNumber",     "GoogleMaps_mean_packetsNumber",           "GooglePlus_mean_packetsNumber",           "GoogleServices_mean_packetsNumber",       "GTP_mean_packetsNumber",                 
                                      "H323_mean_packetsNumber",                 "HotspotShield_mean_packetsNumber",        "HTTP_mean_packetsNumber",                 "HTTP_Proxy_mean_packetsNumber",           "IAX_mean_packetsNumber",                  "ICMP_mean_packetsNumber",                 "IMAPS_mean_packetsNumber",                "IMO_mean_packetsNumber",                 
                                      "Instagram_mean_packetsNumber",            "IPsec_mean_packetsNumber",                "IRC_mean_packetsNumber",                  "LDAP_mean_packetsNumber",                 "LinkedIn_mean_packetsNumber",             "LotusNotes_mean_packetsNumber",           "MDNS_mean_packetsNumber",                 "Messenger_mean_packetsNumber",           
                                      "Microsoft_mean_packetsNumber",            "Mining_mean_packetsNumber",               "MQTT_mean_packetsNumber",                 "MS_OneDrive_mean_packetsNumber",          "MSN_mean_packetsNumber",                  "MsSQL-TDS_mean_packetsNumber",            "MySQL_mean_packetsNumber",                "NestLogSink_mean_packetsNumber",         
                                      "NetBIOS_mean_packetsNumber",              "NetFlix_mean_packetsNumber",              "NFS_mean_packetsNumber",                  "NTP_mean_packetsNumber",                  "Office365_mean_packetsNumber",            "Ookla_mean_packetsNumber",                "OpenDNS_mean_packetsNumber",              "OpenVPN_mean_packetsNumber",             
                                      "Oracle_mean_packetsNumber",               "Pando_Media_Booster_mean_packetsNumber",  "Playstation_mean_packetsNumber",          "PlayStore_mean_packetsNumber",            "POP3_mean_packetsNumber",                 "PostgreSQL_mean_packetsNumber",           "PS_VUE_mean_packetsNumber",               "QQ_mean_packetsNumber",                  
                                      "QUIC_mean_packetsNumber",                 "Radius_mean_packetsNumber",               "RDP_mean_packetsNumber",                  "RTMP_mean_packetsNumber",                 "RTP_mean_packetsNumber",                  "RTSP_mean_packetsNumber",                 "RX_mean_packetsNumber",                   "SAP_mean_packetsNumber",                 
                                      "sFlow_mean_packetsNumber",                "Signal_mean_packetsNumber",               "Sina(Weibo)_mean_packetsNumber",          "SIP_mean_packetsNumber",                  "Skype_mean_packetsNumber",                "SkypeCall_mean_packetsNumber",            "Slack_mean_packetsNumber",                "SMBv1_mean_packetsNumber",               
                                      "SMBv23_mean_packetsNumber",               "SMTP_mean_packetsNumber",                 "SMTPS_mean_packetsNumber",                "Snapchat_mean_packetsNumber",             "SNMP_mean_packetsNumber",                 "SOCKS_mean_packetsNumber",                "SOMEIP_mean_packetsNumber",               "SoundCloud_mean_packetsNumber",          
                                      "Spotify_mean_packetsNumber",              "SSDP_mean_packetsNumber",                 "SSH_mean_packetsNumber",                  "Starcraft_mean_packetsNumber",            "Steam_mean_packetsNumber",                "STUN_mean_packetsNumber",                 "Syslog_mean_packetsNumber",               "Targus Dataspeed_mean_packetsNumber",    
                                      "TeamViewer_mean_packetsNumber",           "Telegram_mean_packetsNumber",             "Teredo_mean_packetsNumber",               "TikTok_mean_packetsNumber",               "TLS_mean_packetsNumber",                  "Tor_mean_packetsNumber",                  "Tuenti_mean_packetsNumber",               "Twitch_mean_packetsNumber",              
                                      "Twitter_mean_packetsNumber",              "UBNTAC2_mean_packetsNumber",              "UbuntuONE_mean_packetsNumber",            "Unencrypted_Jabber_mean_packetsNumber",   "Unknown_mean_packetsNumber",              "UPnP_mean_packetsNumber",                 "Viber_mean_packetsNumber",                "VNC_mean_packetsNumber",                 
                                      "Waze_mean_packetsNumber",                 "Webex_mean_packetsNumber",                "WeChat_mean_packetsNumber",               "WhatsApp_mean_packetsNumber",             "WhatsAppCall_mean_packetsNumber",         "WhatsAppFiles_mean_packetsNumber",        "Whois-DAS_mean_packetsNumber",            "Wikipedia_mean_packetsNumber",           
                                      "WindowsUpdate_mean_packetsNumber",        "Xbox_mean_packetsNumber",                 "Yahoo_mean_packetsNumber",                "YouTube_mean_packetsNumber",              "Zoom_mean_packetsNumber")
 
head(mean_packetsNumber_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN MINIMUM PACKET SIZE SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_min_packetSize_per_app <- as.data.frame(tapply(df$min_ps, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_min_packetSize_per_app) <- 1:nrow(mean_min_packetSize_per_app)
#Replace NA with 0
mean_min_packetSize_per_app[is.na((mean_min_packetSize_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_min_packetSize_per_app) <- c("104_mean_minimum_PacketSize",                  "AJP_mean_minimum_PacketSize",                  "Amazon_mean_minimum_PacketSize",               "AmazonVideo_mean_minimum_PacketSize",          "Apple_mean_minimum_PacketSize",                "AppleiCloud_mean_minimum_PacketSize",          "AppleiTunes_mean_minimum_PacketSize",          "ApplePush_mean_minimum_PacketSize",           
                                          "AppleStore_mean_minimum_PacketSize",           "BGP_mean_minimum_PacketSize",                  "BitTorrent_mean_minimum_PacketSize",           "BJNP_mean_minimum_PacketSize",                 "CiscoSkinny_mean_minimum_PacketSize",          "CiscoVPN_mean_minimum_PacketSize",             "Citrix_mean_minimum_PacketSize",               "Cloudflare_mean_minimum_PacketSize",          
                                          "CNN_mean_minimum_PacketSize",                  "DataSaver_mean_minimum_PacketSize",            "Deezer_mean_minimum_PacketSize",               "DHCP_mean_minimum_PacketSize",                 "Direct_Download_Link_mean_minimum_PacketSize", "DNP3_mean_minimum_PacketSize",                 "DNS_mean_minimum_PacketSize",                  "DNSoverHTTPS_mean_minimum_PacketSize",        
                                          "Dropbox_mean_minimum_PacketSize",              "eBay_mean_minimum_PacketSize",                 "eDonkey_mean_minimum_PacketSize",              "Facebook_mean_minimum_PacketSize",             "FTP_CONTROL_mean_minimum_PacketSize",          "FTP_DATA_mean_minimum_PacketSize",             "Github_mean_minimum_PacketSize",               "GMail_mean_minimum_PacketSize",               
                                          "Google_mean_minimum_PacketSize",               "GoogleDocs_mean_minimum_PacketSize",           "GoogleDrive_mean_minimum_PacketSize",          "GoogleHangoutDuo_mean_minimum_PacketSize",     "GoogleMaps_mean_minimum_PacketSize",           "GooglePlus_mean_minimum_PacketSize",           "GoogleServices_mean_minimum_PacketSize",       "GTP_mean_minimum_PacketSize",                 
                                          "H323_mean_minimum_PacketSize",                 "HotspotShield_mean_minimum_PacketSize",        "HTTP_mean_minimum_PacketSize",                 "HTTP_Proxy_mean_minimum_PacketSize",           "IAX_mean_minimum_PacketSize",                  "ICMP_mean_minimum_PacketSize",                 "IMAPS_mean_minimum_PacketSize",                "IMO_mean_minimum_PacketSize",                 
                                          "Instagram_mean_minimum_PacketSize",            "IPsec_mean_minimum_PacketSize",                "IRC_mean_minimum_PacketSize",                  "LDAP_mean_minimum_PacketSize",                 "LinkedIn_mean_minimum_PacketSize",             "LotusNotes_mean_minimum_PacketSize",           "MDNS_mean_minimum_PacketSize",                 "Messenger_mean_minimum_PacketSize",           
                                          "Microsoft_mean_minimum_PacketSize",            "Mining_mean_minimum_PacketSize",               "MQTT_mean_minimum_PacketSize",                 "MS_OneDrive_mean_minimum_PacketSize",          "MSN_mean_minimum_PacketSize",                  "MsSQL-TDS_mean_minimum_PacketSize",            "MySQL_mean_minimum_PacketSize",                "NestLogSink_mean_minimum_PacketSize",         
                                          "NetBIOS_mean_minimum_PacketSize",              "NetFlix_mean_minimum_PacketSize",              "NFS_mean_minimum_PacketSize",                  "NTP_mean_minimum_PacketSize",                  "Office365_mean_minimum_PacketSize",            "Ookla_mean_minimum_PacketSize",                "OpenDNS_mean_minimum_PacketSize",              "OpenVPN_mean_minimum_PacketSize",             
                                          "Oracle_mean_minimum_PacketSize",               "Pando_Media_Booster_mean_minimum_PacketSize",  "Playstation_mean_minimum_PacketSize",          "PlayStore_mean_minimum_PacketSize",            "POP3_mean_minimum_PacketSize",                 "PostgreSQL_mean_minimum_PacketSize",           "PS_VUE_mean_minimum_PacketSize",               "QQ_mean_minimum_PacketSize",                  
                                          "QUIC_mean_minimum_PacketSize",                 "Radius_mean_minimum_PacketSize",               "RDP_mean_minimum_PacketSize",                  "RTMP_mean_minimum_PacketSize",                 "RTP_mean_minimum_PacketSize",                  "RTSP_mean_minimum_PacketSize",                 "RX_mean_minimum_PacketSize",                   "SAP_mean_minimum_PacketSize",                 
                                          "sFlow_mean_minimum_PacketSize",                "Signal_mean_minimum_PacketSize",               "Sina(Weibo)_mean_minimum_PacketSize",          "SIP_mean_minimum_PacketSize",                  "Skype_mean_minimum_PacketSize",                "SkypeCall_mean_minimum_PacketSize",            "Slack_mean_minimum_PacketSize",                "SMBv1_mean_minimum_PacketSize",               
                                          "SMBv23_mean_minimum_PacketSize",               "SMTP_mean_minimum_PacketSize",                 "SMTPS_mean_minimum_PacketSize",                "Snapchat_mean_minimum_PacketSize",             "SNMP_mean_minimum_PacketSize",                 "SOCKS_mean_minimum_PacketSize",                "SOMEIP_mean_minimum_PacketSize",               "SoundCloud_mean_minimum_PacketSize",          
                                          "Spotify_mean_minimum_PacketSize",              "SSDP_mean_minimum_PacketSize",                 "SSH_mean_minimum_PacketSize",                  "Starcraft_mean_minimum_PacketSize",            "Steam_mean_minimum_PacketSize",                "STUN_mean_minimum_PacketSize",                 "Syslog_mean_minimum_PacketSize",               "Targus Dataspeed_mean_minimum_PacketSize",    
                                          "TeamViewer_mean_minimum_PacketSize",           "Telegram_mean_minimum_PacketSize",             "Teredo_mean_minimum_PacketSize",               "TikTok_mean_minimum_PacketSize",               "TLS_mean_minimum_PacketSize",                  "Tor_mean_minimum_PacketSize",                  "Tuenti_mean_minimum_PacketSize",               "Twitch_mean_minimum_PacketSize",              
                                          "Twitter_mean_minimum_PacketSize",              "UBNTAC2_mean_minimum_PacketSize",              "UbuntuONE_mean_minimum_PacketSize",            "Unencrypted_Jabber_mean_minimum_PacketSize",   "Unknown_mean_minimum_PacketSize",              "UPnP_mean_minimum_PacketSize",                 "Viber_mean_minimum_PacketSize",                "VNC_mean_minimum_PacketSize",                 
                                          "Waze_mean_minimum_PacketSize",                 "Webex_mean_minimum_PacketSize",                "WeChat_mean_minimum_PacketSize",               "WhatsApp_mean_minimum_PacketSize",             "WhatsAppCall_mean_minimum_PacketSize",         "WhatsAppFiles_mean_minimum_PacketSize",        "Whois-DAS_mean_minimum_PacketSize",            "Wikipedia_mean_minimum_PacketSize",           
                                          "WindowsUpdate_mean_minimum_PacketSize",        "Xbox_mean_minimum_PacketSize",                 "Yahoo_mean_minimum_PacketSize",                "YouTube_mean_minimum_PacketSize",              "Zoom_mean_minimum_PacketSize")

head(mean_min_packetSize_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN MAXIMUM PACKET SIZE SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_max_packetSize_per_app <- as.data.frame(tapply(df$max_ps, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_max_packetSize_per_app) <- 1:nrow(mean_max_packetSize_per_app)
#Replace NA with 0
mean_max_packetSize_per_app[is.na((mean_max_packetSize_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_max_packetSize_per_app) <- c("104_mean_maximum_PacketSize",                  "AJP_mean_maximum_PacketSize",                  "Amazon_mean_maximum_PacketSize",               "AmazonVideo_mean_maximum_PacketSize",          "Apple_mean_maximum_PacketSize",                "AppleiCloud_mean_maximum_PacketSize",          "AppleiTunes_mean_maximum_PacketSize",          "ApplePush_mean_maximum_PacketSize",           
                                               "AppleStore_mean_maximum_PacketSize",           "BGP_mean_maximum_PacketSize",                  "BitTorrent_mean_maximum_PacketSize",           "BJNP_mean_maximum_PacketSize",                 "CiscoSkinny_mean_maximum_PacketSize",          "CiscoVPN_mean_maximum_PacketSize",             "Citrix_mean_maximum_PacketSize",               "Cloudflare_mean_maximum_PacketSize",          
                                               "CNN_mean_maximum_PacketSize",                  "DataSaver_mean_maximum_PacketSize",            "Deezer_mean_maximum_PacketSize",               "DHCP_mean_maximum_PacketSize",                 "Direct_Download_Link_mean_maximum_PacketSize", "DNP3_mean_maximum_PacketSize",                 "DNS_mean_maximum_PacketSize",                  "DNSoverHTTPS_mean_maximum_PacketSize",        
                                               "Dropbox_mean_maximum_PacketSize",              "eBay_mean_maximum_PacketSize",                 "eDonkey_mean_maximum_PacketSize",              "Facebook_mean_maximum_PacketSize",             "FTP_CONTROL_mean_maximum_PacketSize",          "FTP_DATA_mean_maximum_PacketSize",             "Github_mean_maximum_PacketSize",               "GMail_mean_maximum_PacketSize",               
                                               "Google_mean_maximum_PacketSize",               "GoogleDocs_mean_maximum_PacketSize",           "GoogleDrive_mean_maximum_PacketSize",          "GoogleHangoutDuo_mean_maximum_PacketSize",     "GoogleMaps_mean_maximum_PacketSize",           "GooglePlus_mean_maximum_PacketSize",           "GoogleServices_mean_maximum_PacketSize",       "GTP_mean_maximum_PacketSize",                 
                                               "H323_mean_maximum_PacketSize",                 "HotspotShield_mean_maximum_PacketSize",        "HTTP_mean_maximum_PacketSize",                 "HTTP_Proxy_mean_maximum_PacketSize",           "IAX_mean_maximum_PacketSize",                  "ICMP_mean_maximum_PacketSize",                 "IMAPS_mean_maximum_PacketSize",                "IMO_mean_maximum_PacketSize",                 
                                               "Instagram_mean_maximum_PacketSize",            "IPsec_mean_maximum_PacketSize",                "IRC_mean_maximum_PacketSize",                  "LDAP_mean_maximum_PacketSize",                 "LinkedIn_mean_maximum_PacketSize",             "LotusNotes_mean_maximum_PacketSize",           "MDNS_mean_maximum_PacketSize",                 "Messenger_mean_maximum_PacketSize",           
                                               "Microsoft_mean_maximum_PacketSize",            "Mining_mean_maximum_PacketSize",               "MQTT_mean_maximum_PacketSize",                 "MS_OneDrive_mean_maximum_PacketSize",          "MSN_mean_maximum_PacketSize",                  "MsSQL-TDS_mean_maximum_PacketSize",            "MySQL_mean_maximum_PacketSize",                "NestLogSink_mean_maximum_PacketSize",         
                                               "NetBIOS_mean_maximum_PacketSize",              "NetFlix_mean_maximum_PacketSize",              "NFS_mean_maximum_PacketSize",                  "NTP_mean_maximum_PacketSize",                  "Office365_mean_maximum_PacketSize",            "Ookla_mean_maximum_PacketSize",                "OpenDNS_mean_maximum_PacketSize",              "OpenVPN_mean_maximum_PacketSize",             
                                               "Oracle_mean_maximum_PacketSize",               "Pando_Media_Booster_mean_maximum_PacketSize",  "Playstation_mean_maximum_PacketSize",          "PlayStore_mean_maximum_PacketSize",            "POP3_mean_maximum_PacketSize",                 "PostgreSQL_mean_maximum_PacketSize",           "PS_VUE_mean_maximum_PacketSize",               "QQ_mean_maximum_PacketSize",                  
                                               "QUIC_mean_maximum_PacketSize",                 "Radius_mean_maximum_PacketSize",               "RDP_mean_maximum_PacketSize",                  "RTMP_mean_maximum_PacketSize",                 "RTP_mean_maximum_PacketSize",                  "RTSP_mean_maximum_PacketSize",                 "RX_mean_maximum_PacketSize",                   "SAP_mean_maximum_PacketSize",                 
                                               "sFlow_mean_maximum_PacketSize",                "Signal_mean_maximum_PacketSize",               "Sina(Weibo)_mean_maximum_PacketSize",          "SIP_mean_maximum_PacketSize",                  "Skype_mean_maximum_PacketSize",                "SkypeCall_mean_maximum_PacketSize",            "Slack_mean_maximum_PacketSize",                "SMBv1_mean_maximum_PacketSize",               
                                               "SMBv23_mean_maximum_PacketSize",               "SMTP_mean_maximum_PacketSize",                 "SMTPS_mean_maximum_PacketSize",                "Snapchat_mean_maximum_PacketSize",             "SNMP_mean_maximum_PacketSize",                 "SOCKS_mean_maximum_PacketSize",                "SOMEIP_mean_maximum_PacketSize",               "SoundCloud_mean_maximum_PacketSize",          
                                               "Spotify_mean_maximum_PacketSize",              "SSDP_mean_maximum_PacketSize",                 "SSH_mean_maximum_PacketSize",                  "Starcraft_mean_maximum_PacketSize",            "Steam_mean_maximum_PacketSize",                "STUN_mean_maximum_PacketSize",                 "Syslog_mean_maximum_PacketSize",               "Targus Dataspeed_mean_maximum_PacketSize",    
                                               "TeamViewer_mean_maximum_PacketSize",           "Telegram_mean_maximum_PacketSize",             "Teredo_mean_maximum_PacketSize",               "TikTok_mean_maximum_PacketSize",               "TLS_mean_maximum_PacketSize",                  "Tor_mean_maximum_PacketSize",                  "Tuenti_mean_maximum_PacketSize",               "Twitch_mean_maximum_PacketSize",              
                                               "Twitter_mean_maximum_PacketSize",              "UBNTAC2_mean_maximum_PacketSize",              "UbuntuONE_mean_maximum_PacketSize",            "Unencrypted_Jabber_mean_maximum_PacketSize",   "Unknown_mean_maximum_PacketSize",              "UPnP_mean_maximum_PacketSize",                 "Viber_mean_maximum_PacketSize",                "VNC_mean_maximum_PacketSize",                 
                                               "Waze_mean_maximum_PacketSize",                 "Webex_mean_maximum_PacketSize",                "WeChat_mean_maximum_PacketSize",               "WhatsApp_mean_maximum_PacketSize",             "WhatsAppCall_mean_maximum_PacketSize",         "WhatsAppFiles_mean_maximum_PacketSize",        "Whois-DAS_mean_maximum_PacketSize",            "Wikipedia_mean_maximum_PacketSize",           
                                               "WindowsUpdate_mean_maximum_PacketSize",        "Xbox_mean_maximum_PacketSize",                 "Yahoo_mean_maximum_PacketSize",                "YouTube_mean_maximum_PacketSize",              "Zoom_mean_maximum_PacketSize")

head(mean_max_packetSize_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF MEAN PACKET SIZE SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_avg_packetSize_per_app <- as.data.frame(tapply(df$avg_ps, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_avg_packetSize_per_app) <- 1:nrow(mean_avg_packetSize_per_app)
#Replace NA with 0
mean_avg_packetSize_per_app[is.na((mean_avg_packetSize_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_avg_packetSize_per_app) <- c("104_mean_avg_packetSize",                  "AJP_mean_avg_packetSize",                  "Amazon_mean_avg_packetSize",               "AmazonVideo_mean_avg_packetSize",          "Apple_mean_avg_packetSize",                "AppleiCloud_mean_avg_packetSize",          "AppleiTunes_mean_avg_packetSize",          "ApplePush_mean_avg_packetSize",           
                                               "AppleStore_mean_avg_packetSize",           "BGP_mean_avg_packetSize",                  "BitTorrent_mean_avg_packetSize",           "BJNP_mean_avg_packetSize",                 "CiscoSkinny_mean_avg_packetSize",          "CiscoVPN_mean_avg_packetSize",             "Citrix_mean_avg_packetSize",               "Cloudflare_mean_avg_packetSize",          
                                               "CNN_mean_avg_packetSize",                  "DataSaver_mean_avg_packetSize",            "Deezer_mean_avg_packetSize",               "DHCP_mean_avg_packetSize",                 "Direct_Download_Link_mean_avg_packetSize", "DNP3_mean_avg_packetSize",                 "DNS_mean_avg_packetSize",                  "DNSoverHTTPS_mean_avg_packetSize",        
                                               "Dropbox_mean_avg_packetSize",              "eBay_mean_avg_packetSize",                 "eDonkey_mean_avg_packetSize",              "Facebook_mean_avg_packetSize",             "FTP_CONTROL_mean_avg_packetSize",          "FTP_DATA_mean_avg_packetSize",             "Github_mean_avg_packetSize",               "GMail_mean_avg_packetSize",               
                                               "Google_mean_avg_packetSize",               "GoogleDocs_mean_avg_packetSize",           "GoogleDrive_mean_avg_packetSize",          "GoogleHangoutDuo_mean_avg_packetSize",     "GoogleMaps_mean_avg_packetSize",           "GooglePlus_mean_avg_packetSize",           "GoogleServices_mean_avg_packetSize",       "GTP_mean_avg_packetSize",                 
                                               "H323_mean_avg_packetSize",                 "HotspotShield_mean_avg_packetSize",        "HTTP_mean_avg_packetSize",                 "HTTP_Proxy_mean_avg_packetSize",           "IAX_mean_avg_packetSize",                  "ICMP_mean_avg_packetSize",                 "IMAPS_mean_avg_packetSize",                "IMO_mean_avg_packetSize",                 
                                               "Instagram_mean_avg_packetSize",            "IPsec_mean_avg_packetSize",                "IRC_mean_avg_packetSize",                  "LDAP_mean_avg_packetSize",                 "LinkedIn_mean_avg_packetSize",             "LotusNotes_mean_avg_packetSize",           "MDNS_mean_avg_packetSize",                 "Messenger_mean_avg_packetSize",           
                                               "Microsoft_mean_avg_packetSize",            "Mining_mean_avg_packetSize",               "MQTT_mean_avg_packetSize",                 "MS_OneDrive_mean_avg_packetSize",          "MSN_mean_avg_packetSize",                  "MsSQL-TDS_mean_avg_packetSize",            "MySQL_mean_avg_packetSize",                "NestLogSink_mean_avg_packetSize",         
                                               "NetBIOS_mean_avg_packetSize",              "NetFlix_mean_avg_packetSize",              "NFS_mean_avg_packetSize",                  "NTP_mean_avg_packetSize",                  "Office365_mean_avg_packetSize",            "Ookla_mean_avg_packetSize",                "OpenDNS_mean_avg_packetSize",              "OpenVPN_mean_avg_packetSize",             
                                               "Oracle_mean_avg_packetSize",               "Pando_Media_Booster_mean_avg_packetSize",  "Playstation_mean_avg_packetSize",          "PlayStore_mean_avg_packetSize",            "POP3_mean_avg_packetSize",                 "PostgreSQL_mean_avg_packetSize",           "PS_VUE_mean_avg_packetSize",               "QQ_mean_avg_packetSize",                  
                                               "QUIC_mean_avg_packetSize",                 "Radius_mean_avg_packetSize",               "RDP_mean_avg_packetSize",                  "RTMP_mean_avg_packetSize",                 "RTP_mean_avg_packetSize",                  "RTSP_mean_avg_packetSize",                 "RX_mean_avg_packetSize",                   "SAP_mean_avg_packetSize",                 
                                               "sFlow_mean_avg_packetSize",                "Signal_mean_avg_packetSize",               "Sina(Weibo)_mean_avg_packetSize",          "SIP_mean_avg_packetSize",                  "Skype_mean_avg_packetSize",                "SkypeCall_mean_avg_packetSize",            "Slack_mean_avg_packetSize",                "SMBv1_mean_avg_packetSize",               
                                               "SMBv23_mean_avg_packetSize",               "SMTP_mean_avg_packetSize",                 "SMTPS_mean_avg_packetSize",                "Snapchat_mean_avg_packetSize",             "SNMP_mean_avg_packetSize",                 "SOCKS_mean_avg_packetSize",                "SOMEIP_mean_avg_packetSize",               "SoundCloud_mean_avg_packetSize",          
                                               "Spotify_mean_avg_packetSize",              "SSDP_mean_avg_packetSize",                 "SSH_mean_avg_packetSize",                  "Starcraft_mean_avg_packetSize",            "Steam_mean_avg_packetSize",                "STUN_mean_avg_packetSize",                 "Syslog_mean_avg_packetSize",               "Targus Dataspeed_mean_avg_packetSize",    
                                               "TeamViewer_mean_avg_packetSize",           "Telegram_mean_avg_packetSize",             "Teredo_mean_avg_packetSize",               "TikTok_mean_avg_packetSize",               "TLS_mean_avg_packetSize",                  "Tor_mean_avg_packetSize",                  "Tuenti_mean_avg_packetSize",               "Twitch_mean_avg_packetSize",              
                                               "Twitter_mean_avg_packetSize",              "UBNTAC2_mean_avg_packetSize",              "UbuntuONE_mean_avg_packetSize",            "Unencrypted_Jabber_mean_avg_packetSize",   "Unknown_mean_avg_packetSize",              "UPnP_mean_avg_packetSize",                 "Viber_mean_avg_packetSize",                "VNC_mean_avg_packetSize",                 
                                               "Waze_mean_avg_packetSize",                 "Webex_mean_avg_packetSize",                "WeChat_mean_avg_packetSize",               "WhatsApp_mean_avg_packetSize",             "WhatsAppCall_mean_avg_packetSize",         "WhatsAppFiles_mean_avg_packetSize",        "Whois-DAS_mean_avg_packetSize",            "Wikipedia_mean_avg_packetSize",           
                                               "WindowsUpdate_mean_avg_packetSize",        "Xbox_mean_avg_packetSize",                 "Yahoo_mean_avg_packetSize",                "YouTube_mean_avg_packetSize",              "Zoom_mean_avg_packetSize")

head(mean_avg_packetSize_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF STANDARD DEVIATION PACKET SIZE SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_std_dev_packetSize_per_app <- as.data.frame(tapply(df$std_dev_ps, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_std_dev_packetSize_per_app) <- 1:nrow(mean_std_dev_packetSize_per_app)
#Replace NA with 0
mean_std_dev_packetSize_per_app[is.na((mean_std_dev_packetSize_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_std_dev_packetSize_per_app) <- c("104_mean_std_dev_packetSize",                  "AJP_mean_std_dev_packetSize",                  "Amazon_mean_std_dev_packetSize",               "AmazonVideo_mean_std_dev_packetSize",          "Apple_mean_std_dev_packetSize",                "AppleiCloud_mean_std_dev_packetSize",          "AppleiTunes_mean_std_dev_packetSize",          "ApplePush_mean_std_dev_packetSize",           
                                           "AppleStore_mean_std_dev_packetSize",           "BGP_mean_std_dev_packetSize",                  "BitTorrent_mean_std_dev_packetSize",           "BJNP_mean_std_dev_packetSize",                 "CiscoSkinny_mean_std_dev_packetSize",          "CiscoVPN_mean_std_dev_packetSize",             "Citrix_mean_std_dev_packetSize",               "Cloudflare_mean_std_dev_packetSize",          
                                           "CNN_mean_std_dev_packetSize",                  "DataSaver_mean_std_dev_packetSize",            "Deezer_mean_std_dev_packetSize",               "DHCP_mean_std_dev_packetSize",                 "Direct_Download_Link_mean_std_dev_packetSize", "DNP3_mean_std_dev_packetSize",                 "DNS_mean_std_dev_packetSize",                  "DNSoverHTTPS_mean_std_dev_packetSize",        
                                           "Dropbox_mean_std_dev_packetSize",              "eBay_mean_std_dev_packetSize",                 "eDonkey_mean_std_dev_packetSize",              "Facebook_mean_std_dev_packetSize",             "FTP_CONTROL_mean_std_dev_packetSize",          "FTP_DATA_mean_std_dev_packetSize",             "Github_mean_std_dev_packetSize",               "GMail_mean_std_dev_packetSize",               
                                           "Google_mean_std_dev_packetSize",               "GoogleDocs_mean_std_dev_packetSize",           "GoogleDrive_mean_std_dev_packetSize",          "GoogleHangoutDuo_mean_std_dev_packetSize",     "GoogleMaps_mean_std_dev_packetSize",           "GooglePlus_mean_std_dev_packetSize",           "GoogleServices_mean_std_dev_packetSize",       "GTP_mean_std_dev_packetSize",                 
                                           "H323_mean_std_dev_packetSize",                 "HotspotShield_mean_std_dev_packetSize",        "HTTP_mean_std_dev_packetSize",                 "HTTP_Proxy_mean_std_dev_packetSize",           "IAX_mean_std_dev_packetSize",                  "ICMP_mean_std_dev_packetSize",                 "IMAPS_mean_std_dev_packetSize",                "IMO_mean_std_dev_packetSize",                 
                                           "Instagram_mean_std_dev_packetSize",            "IPsec_mean_std_dev_packetSize",                "IRC_mean_std_dev_packetSize",                  "LDAP_mean_std_dev_packetSize",                 "LinkedIn_mean_std_dev_packetSize",             "LotusNotes_mean_std_dev_packetSize",           "MDNS_mean_std_dev_packetSize",                 "Messenger_mean_std_dev_packetSize",           
                                           "Microsoft_mean_std_dev_packetSize",            "Mining_mean_std_dev_packetSize",               "MQTT_mean_std_dev_packetSize",                 "MS_OneDrive_mean_std_dev_packetSize",          "MSN_mean_std_dev_packetSize",                  "MsSQL-TDS_mean_std_dev_packetSize",            "MySQL_mean_std_dev_packetSize",                "NestLogSink_mean_std_dev_packetSize",         
                                           "NetBIOS_mean_std_dev_packetSize",              "NetFlix_mean_std_dev_packetSize",              "NFS_mean_std_dev_packetSize",                  "NTP_mean_std_dev_packetSize",                  "Office365_mean_std_dev_packetSize",            "Ookla_mean_std_dev_packetSize",                "OpenDNS_mean_std_dev_packetSize",              "OpenVPN_mean_std_dev_packetSize",             
                                           "Oracle_mean_std_dev_packetSize",               "Pando_Media_Booster_mean_std_dev_packetSize",  "Playstation_mean_std_dev_packetSize",          "PlayStore_mean_std_dev_packetSize",            "POP3_mean_std_dev_packetSize",                 "PostgreSQL_mean_std_dev_packetSize",           "PS_VUE_mean_std_dev_packetSize",               "QQ_mean_std_dev_packetSize",                  
                                           "QUIC_mean_std_dev_packetSize",                 "Radius_mean_std_dev_packetSize",               "RDP_mean_std_dev_packetSize",                  "RTMP_mean_std_dev_packetSize",                 "RTP_mean_std_dev_packetSize",                  "RTSP_mean_std_dev_packetSize",                 "RX_mean_std_dev_packetSize",                   "SAP_mean_std_dev_packetSize",                 
                                           "sFlow_mean_std_dev_packetSize",                "Signal_mean_std_dev_packetSize",               "Sina(Weibo)_mean_std_dev_packetSize",          "SIP_mean_std_dev_packetSize",                  "Skype_mean_std_dev_packetSize",                "SkypeCall_mean_std_dev_packetSize",            "Slack_mean_std_dev_packetSize",                "SMBv1_mean_std_dev_packetSize",               
                                           "SMBv23_mean_std_dev_packetSize",               "SMTP_mean_std_dev_packetSize",                 "SMTPS_mean_std_dev_packetSize",                "Snapchat_mean_std_dev_packetSize",             "SNMP_mean_std_dev_packetSize",                 "SOCKS_mean_std_dev_packetSize",                "SOMEIP_mean_std_dev_packetSize",               "SoundCloud_mean_std_dev_packetSize",          
                                           "Spotify_mean_std_dev_packetSize",              "SSDP_mean_std_dev_packetSize",                 "SSH_mean_std_dev_packetSize",                  "Starcraft_mean_std_dev_packetSize",            "Steam_mean_std_dev_packetSize",                "STUN_mean_std_dev_packetSize",                 "Syslog_mean_std_dev_packetSize",               "Targus Dataspeed_mean_std_dev_packetSize",    
                                           "TeamViewer_mean_std_dev_packetSize",           "Telegram_mean_std_dev_packetSize",             "Teredo_mean_std_dev_packetSize",               "TikTok_mean_std_dev_packetSize",               "TLS_mean_std_dev_packetSize",                  "Tor_mean_std_dev_packetSize",                  "Tuenti_mean_std_dev_packetSize",               "Twitch_mean_std_dev_packetSize",              
                                           "Twitter_mean_std_dev_packetSize",              "UBNTAC2_mean_std_dev_packetSize",              "UbuntuONE_mean_std_dev_packetSize",            "Unencrypted_Jabber_mean_std_dev_packetSize",   "Unknown_mean_std_dev_packetSize",              "UPnP_mean_std_dev_packetSize",                 "Viber_mean_std_dev_packetSize",                "VNC_mean_std_dev_packetSize",                 
                                           "Waze_mean_std_dev_packetSize",                 "Webex_mean_std_dev_packetSize",                "WeChat_mean_std_dev_packetSize",               "WhatsApp_mean_std_dev_packetSize",             "WhatsAppCall_mean_std_dev_packetSize",         "WhatsAppFiles_mean_std_dev_packetSize",        "Whois-DAS_mean_std_dev_packetSize",            "Wikipedia_mean_std_dev_packetSize",           
                                           "WindowsUpdate_mean_std_dev_packetSize",        "Xbox_mean_std_dev_packetSize",                 "Yahoo_mean_std_dev_packetSize",                "YouTube_mean_std_dev_packetSize",              "Zoom_mean_std_dev_packetSize")

head(mean_std_dev_packetSize_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF MINIMUM PACKET IAT SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_min_piat_per_app <- as.data.frame(tapply(df$min_piat, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_min_piat_per_app) <- 1:nrow(mean_min_piat_per_app)
#Replace NA with 0
mean_min_piat_per_app[is.na((mean_min_piat_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_min_piat_per_app) <- c("104_mean_min_piat",                  "AJP_mean_min_piat",                  "Amazon_mean_min_piat",               "AmazonVideo_mean_min_piat",          "Apple_mean_min_piat",                "AppleiCloud_mean_min_piat",          "AppleiTunes_mean_min_piat",          "ApplePush_mean_min_piat",           
                                               "AppleStore_mean_min_piat",           "BGP_mean_min_piat",                  "BitTorrent_mean_min_piat",           "BJNP_mean_min_piat",                 "CiscoSkinny_mean_min_piat",          "CiscoVPN_mean_min_piat",             "Citrix_mean_min_piat",               "Cloudflare_mean_min_piat",          
                                               "CNN_mean_min_piat",                  "DataSaver_mean_min_piat",            "Deezer_mean_min_piat",               "DHCP_mean_min_piat",                 "Direct_Download_Link_mean_min_piat", "DNP3_mean_min_piat",                 "DNS_mean_min_piat",                  "DNSoverHTTPS_mean_min_piat",        
                                               "Dropbox_mean_min_piat",              "eBay_mean_min_piat",                 "eDonkey_mean_min_piat",              "Facebook_mean_min_piat",             "FTP_CONTROL_mean_min_piat",          "FTP_DATA_mean_min_piat",             "Github_mean_min_piat",               "GMail_mean_min_piat",               
                                               "Google_mean_min_piat",               "GoogleDocs_mean_min_piat",           "GoogleDrive_mean_min_piat",          "GoogleHangoutDuo_mean_min_piat",     "GoogleMaps_mean_min_piat",           "GooglePlus_mean_min_piat",           "GoogleServices_mean_min_piat",       "GTP_mean_min_piat",                 
                                               "H323_mean_min_piat",                 "HotspotShield_mean_min_piat",        "HTTP_mean_min_piat",                 "HTTP_Proxy_mean_min_piat",           "IAX_mean_min_piat",                  "ICMP_mean_min_piat",                 "IMAPS_mean_min_piat",                "IMO_mean_min_piat",                 
                                               "Instagram_mean_min_piat",            "IPsec_mean_min_piat",                "IRC_mean_min_piat",                  "LDAP_mean_min_piat",                 "LinkedIn_mean_min_piat",             "LotusNotes_mean_min_piat",           "MDNS_mean_min_piat",                 "Messenger_mean_min_piat",           
                                               "Microsoft_mean_min_piat",            "Mining_mean_min_piat",               "MQTT_mean_min_piat",                 "MS_OneDrive_mean_min_piat",          "MSN_mean_min_piat",                  "MsSQL-TDS_mean_min_piat",            "MySQL_mean_min_piat",                "NestLogSink_mean_min_piat",         
                                               "NetBIOS_mean_min_piat",              "NetFlix_mean_min_piat",              "NFS_mean_min_piat",                  "NTP_mean_min_piat",                  "Office365_mean_min_piat",            "Ookla_mean_min_piat",                "OpenDNS_mean_min_piat",              "OpenVPN_mean_min_piat",             
                                               "Oracle_mean_min_piat",               "Pando_Media_Booster_mean_min_piat",  "Playstation_mean_min_piat",          "PlayStore_mean_min_piat",            "POP3_mean_min_piat",                 "PostgreSQL_mean_min_piat",           "PS_VUE_mean_min_piat",               "QQ_mean_min_piat",                  
                                               "QUIC_mean_min_piat",                 "Radius_mean_min_piat",               "RDP_mean_min_piat",                  "RTMP_mean_min_piat",                 "RTP_mean_min_piat",                  "RTSP_mean_min_piat",                 "RX_mean_min_piat",                   "SAP_mean_min_piat",                 
                                               "sFlow_mean_min_piat",                "Signal_mean_min_piat",               "Sina(Weibo)_mean_min_piat",          "SIP_mean_min_piat",                  "Skype_mean_min_piat",                "SkypeCall_mean_min_piat",            "Slack_mean_min_piat",                "SMBv1_mean_min_piat",               
                                               "SMBv23_mean_min_piat",               "SMTP_mean_min_piat",                 "SMTPS_mean_min_piat",                "Snapchat_mean_min_piat",             "SNMP_mean_min_piat",                 "SOCKS_mean_min_piat",                "SOMEIP_mean_min_piat",               "SoundCloud_mean_min_piat",          
                                               "Spotify_mean_min_piat",              "SSDP_mean_min_piat",                 "SSH_mean_min_piat",                  "Starcraft_mean_min_piat",            "Steam_mean_min_piat",                "STUN_mean_min_piat",                 "Syslog_mean_min_piat",               "Targus Dataspeed_mean_min_piat",    
                                               "TeamViewer_mean_min_piat",           "Telegram_mean_min_piat",             "Teredo_mean_min_piat",               "TikTok_mean_min_piat",               "TLS_mean_min_piat",                  "Tor_mean_min_piat",                  "Tuenti_mean_min_piat",               "Twitch_mean_min_piat",              
                                               "Twitter_mean_min_piat",              "UBNTAC2_mean_min_piat",              "UbuntuONE_mean_min_piat",            "Unencrypted_Jabber_mean_min_piat",   "Unknown_mean_min_piat",              "UPnP_mean_min_piat",                 "Viber_mean_min_piat",                "VNC_mean_min_piat",                 
                                               "Waze_mean_min_piat",                 "Webex_mean_min_piat",                "WeChat_mean_min_piat",               "WhatsApp_mean_min_piat",             "WhatsAppCall_mean_min_piat",         "WhatsAppFiles_mean_min_piat",        "Whois-DAS_mean_min_piat",            "Wikipedia_mean_min_piat",           
                                               "WindowsUpdate_mean_min_piat",        "Xbox_mean_min_piat",                 "Yahoo_mean_min_piat",                "YouTube_mean_min_piat",              "Zoom_mean_min_piat")

head(mean_min_piat_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF MAXIMUM PACKET IAT SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_max_piat_per_app <- as.data.frame(tapply(df$max_piat, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_max_piat_per_app) <- 1:nrow(mean_max_piat_per_app)
#Replace NA with 0
mean_max_piat_per_app[is.na((mean_max_piat_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_max_piat_per_app) <- c("104_mean_max_piat",                  "AJP_mean_max_piat",                  "Amazon_mean_max_piat",               "AmazonVideo_mean_max_piat",          "Apple_mean_max_piat",                "AppleiCloud_mean_max_piat",          "AppleiTunes_mean_max_piat",          "ApplePush_mean_max_piat",           
                                     "AppleStore_mean_max_piat",           "BGP_mean_max_piat",                  "BitTorrent_mean_max_piat",           "BJNP_mean_max_piat",                 "CiscoSkinny_mean_max_piat",          "CiscoVPN_mean_max_piat",             "Citrix_mean_max_piat",               "Cloudflare_mean_max_piat",          
                                     "CNN_mean_max_piat",                  "DataSaver_mean_max_piat",            "Deezer_mean_max_piat",               "DHCP_mean_max_piat",                 "Direct_Download_Link_mean_max_piat", "DNP3_mean_max_piat",                 "DNS_mean_max_piat",                  "DNSoverHTTPS_mean_max_piat",        
                                     "Dropbox_mean_max_piat",              "eBay_mean_max_piat",                 "eDonkey_mean_max_piat",              "Facebook_mean_max_piat",             "FTP_CONTROL_mean_max_piat",          "FTP_DATA_mean_max_piat",             "Github_mean_max_piat",               "GMail_mean_max_piat",               
                                     "Google_mean_max_piat",               "GoogleDocs_mean_max_piat",           "GoogleDrive_mean_max_piat",          "GoogleHangoutDuo_mean_max_piat",     "GoogleMaps_mean_max_piat",           "GooglePlus_mean_max_piat",           "GoogleServices_mean_max_piat",       "GTP_mean_max_piat",                 
                                     "H323_mean_max_piat",                 "HotspotShield_mean_max_piat",        "HTTP_mean_max_piat",                 "HTTP_Proxy_mean_max_piat",           "IAX_mean_max_piat",                  "ICMP_mean_max_piat",                 "IMAPS_mean_max_piat",                "IMO_mean_max_piat",                 
                                     "Instagram_mean_max_piat",            "IPsec_mean_max_piat",                "IRC_mean_max_piat",                  "LDAP_mean_max_piat",                 "LinkedIn_mean_max_piat",             "LotusNotes_mean_max_piat",           "MDNS_mean_max_piat",                 "Messenger_mean_max_piat",           
                                     "Microsoft_mean_max_piat",            "Mining_mean_max_piat",               "MQTT_mean_max_piat",                 "MS_OneDrive_mean_max_piat",          "MSN_mean_max_piat",                  "MsSQL-TDS_mean_max_piat",            "MySQL_mean_max_piat",                "NestLogSink_mean_max_piat",         
                                     "NetBIOS_mean_max_piat",              "NetFlix_mean_max_piat",              "NFS_mean_max_piat",                  "NTP_mean_max_piat",                  "Office365_mean_max_piat",            "Ookla_mean_max_piat",                "OpenDNS_mean_max_piat",              "OpenVPN_mean_max_piat",             
                                     "Oracle_mean_max_piat",               "Pando_Media_Booster_mean_max_piat",  "Playstation_mean_max_piat",          "PlayStore_mean_max_piat",            "POP3_mean_max_piat",                 "PostgreSQL_mean_max_piat",           "PS_VUE_mean_max_piat",               "QQ_mean_max_piat",                  
                                     "QUIC_mean_max_piat",                 "Radius_mean_max_piat",               "RDP_mean_max_piat",                  "RTMP_mean_max_piat",                 "RTP_mean_max_piat",                  "RTSP_mean_max_piat",                 "RX_mean_max_piat",                   "SAP_mean_max_piat",                 
                                     "sFlow_mean_max_piat",                "Signal_mean_max_piat",               "Sina(Weibo)_mean_max_piat",          "SIP_mean_max_piat",                  "Skype_mean_max_piat",                "SkypeCall_mean_max_piat",            "Slack_mean_max_piat",                "SMBv1_mean_max_piat",               
                                     "SMBv23_mean_max_piat",               "SMTP_mean_max_piat",                 "SMTPS_mean_max_piat",                "Snapchat_mean_max_piat",             "SNMP_mean_max_piat",                 "SOCKS_mean_max_piat",                "SOMEIP_mean_max_piat",               "SoundCloud_mean_max_piat",          
                                     "Spotify_mean_max_piat",              "SSDP_mean_max_piat",                 "SSH_mean_max_piat",                  "Starcraft_mean_max_piat",            "Steam_mean_max_piat",                "STUN_mean_max_piat",                 "Syslog_mean_max_piat",               "Targus Dataspeed_mean_max_piat",    
                                     "TeamViewer_mean_max_piat",           "Telegram_mean_max_piat",             "Teredo_mean_max_piat",               "TikTok_mean_max_piat",               "TLS_mean_max_piat",                  "Tor_mean_max_piat",                  "Tuenti_mean_max_piat",               "Twitch_mean_max_piat",              
                                     "Twitter_mean_max_piat",              "UBNTAC2_mean_max_piat",              "UbuntuONE_mean_max_piat",            "Unencrypted_Jabber_mean_max_piat",   "Unknown_mean_max_piat",              "UPnP_mean_max_piat",                 "Viber_mean_max_piat",                "VNC_mean_max_piat",                 
                                     "Waze_mean_max_piat",                 "Webex_mean_max_piat",                "WeChat_mean_max_piat",               "WhatsApp_mean_max_piat",             "WhatsAppCall_mean_max_piat",         "WhatsAppFiles_mean_max_piat",        "Whois-DAS_mean_max_piat",            "Wikipedia_mean_max_piat",           
                                     "WindowsUpdate_mean_max_piat",        "Xbox_mean_max_piat",                 "Yahoo_mean_max_piat",                "YouTube_mean_max_piat",              "Zoom_mean_max_piat")

head(mean_max_piat_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF MEANS PACKET IAT SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_avg_piat_per_app <- as.data.frame(tapply(df$avg_piat, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_avg_piat_per_app) <- 1:nrow(mean_avg_piat_per_app)
#Replace NA with 0
mean_avg_piat_per_app[is.na((mean_avg_piat_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_avg_piat_per_app) <- c("104_mean_avg_piat",                  "AJP_mean_avg_piat",                  "Amazon_mean_avg_piat",               "AmazonVideo_mean_avg_piat",          "Apple_mean_avg_piat",                "AppleiCloud_mean_avg_piat",          "AppleiTunes_mean_avg_piat",          "ApplePush_mean_avg_piat",           
                                     "AppleStore_mean_avg_piat",           "BGP_mean_avg_piat",                  "BitTorrent_mean_avg_piat",           "BJNP_mean_avg_piat",                 "CiscoSkinny_mean_avg_piat",          "CiscoVPN_mean_avg_piat",             "Citrix_mean_avg_piat",               "Cloudflare_mean_avg_piat",          
                                     "CNN_mean_avg_piat",                  "DataSaver_mean_avg_piat",            "Deezer_mean_avg_piat",               "DHCP_mean_avg_piat",                 "Direct_Download_Link_mean_avg_piat", "DNP3_mean_avg_piat",                 "DNS_mean_avg_piat",                  "DNSoverHTTPS_mean_avg_piat",        
                                     "Dropbox_mean_avg_piat",              "eBay_mean_avg_piat",                 "eDonkey_mean_avg_piat",              "Facebook_mean_avg_piat",             "FTP_CONTROL_mean_avg_piat",          "FTP_DATA_mean_avg_piat",             "Github_mean_avg_piat",               "GMail_mean_avg_piat",               
                                     "Google_mean_avg_piat",               "GoogleDocs_mean_avg_piat",           "GoogleDrive_mean_avg_piat",          "GoogleHangoutDuo_mean_avg_piat",     "GoogleMaps_mean_avg_piat",           "GooglePlus_mean_avg_piat",           "GoogleServices_mean_avg_piat",       "GTP_mean_avg_piat",                 
                                     "H323_mean_avg_piat",                 "HotspotShield_mean_avg_piat",        "HTTP_mean_avg_piat",                 "HTTP_Proxy_mean_avg_piat",           "IAX_mean_avg_piat",                  "ICMP_mean_avg_piat",                 "IMAPS_mean_avg_piat",                "IMO_mean_avg_piat",                 
                                     "Instagram_mean_avg_piat",            "IPsec_mean_avg_piat",                "IRC_mean_avg_piat",                  "LDAP_mean_avg_piat",                 "LinkedIn_mean_avg_piat",             "LotusNotes_mean_avg_piat",           "MDNS_mean_avg_piat",                 "Messenger_mean_avg_piat",           
                                     "Microsoft_mean_avg_piat",            "Mining_mean_avg_piat",               "MQTT_mean_avg_piat",                 "MS_OneDrive_mean_avg_piat",          "MSN_mean_avg_piat",                  "MsSQL-TDS_mean_avg_piat",            "MySQL_mean_avg_piat",                "NestLogSink_mean_avg_piat",         
                                     "NetBIOS_mean_avg_piat",              "NetFlix_mean_avg_piat",              "NFS_mean_avg_piat",                  "NTP_mean_avg_piat",                  "Office365_mean_avg_piat",            "Ookla_mean_avg_piat",                "OpenDNS_mean_avg_piat",              "OpenVPN_mean_avg_piat",             
                                     "Oracle_mean_avg_piat",               "Pando_Media_Booster_mean_avg_piat",  "Playstation_mean_avg_piat",          "PlayStore_mean_avg_piat",            "POP3_mean_avg_piat",                 "PostgreSQL_mean_avg_piat",           "PS_VUE_mean_avg_piat",               "QQ_mean_avg_piat",                  
                                     "QUIC_mean_avg_piat",                 "Radius_mean_avg_piat",               "RDP_mean_avg_piat",                  "RTMP_mean_avg_piat",                 "RTP_mean_avg_piat",                  "RTSP_mean_avg_piat",                 "RX_mean_avg_piat",                   "SAP_mean_avg_piat",                 
                                     "sFlow_mean_avg_piat",                "Signal_mean_avg_piat",               "Sina(Weibo)_mean_avg_piat",          "SIP_mean_avg_piat",                  "Skype_mean_avg_piat",                "SkypeCall_mean_avg_piat",            "Slack_mean_avg_piat",                "SMBv1_mean_avg_piat",               
                                     "SMBv23_mean_avg_piat",               "SMTP_mean_avg_piat",                 "SMTPS_mean_avg_piat",                "Snapchat_mean_avg_piat",             "SNMP_mean_avg_piat",                 "SOCKS_mean_avg_piat",                "SOMEIP_mean_avg_piat",               "SoundCloud_mean_avg_piat",          
                                     "Spotify_mean_avg_piat",              "SSDP_mean_avg_piat",                 "SSH_mean_avg_piat",                  "Starcraft_mean_avg_piat",            "Steam_mean_avg_piat",                "STUN_mean_avg_piat",                 "Syslog_mean_avg_piat",               "Targus Dataspeed_mean_avg_piat",    
                                     "TeamViewer_mean_avg_piat",           "Telegram_mean_avg_piat",             "Teredo_mean_avg_piat",               "TikTok_mean_avg_piat",               "TLS_mean_avg_piat",                  "Tor_mean_avg_piat",                  "Tuenti_mean_avg_piat",               "Twitch_mean_avg_piat",              
                                     "Twitter_mean_avg_piat",              "UBNTAC2_mean_avg_piat",              "UbuntuONE_mean_avg_piat",            "Unencrypted_Jabber_mean_avg_piat",   "Unknown_mean_avg_piat",              "UPnP_mean_avg_piat",                 "Viber_mean_avg_piat",                "VNC_mean_avg_piat",                 
                                     "Waze_mean_avg_piat",                 "Webex_mean_avg_piat",                "WeChat_mean_avg_piat",               "WhatsApp_mean_avg_piat",             "WhatsAppCall_mean_avg_piat",         "WhatsAppFiles_mean_avg_piat",        "Whois-DAS_mean_avg_piat",            "Wikipedia_mean_avg_piat",           
                                     "WindowsUpdate_mean_avg_piat",        "Xbox_mean_avg_piat",                 "Yahoo_mean_avg_piat",                "YouTube_mean_avg_piat",              "Zoom_mean_avg_piat")

head(mean_avg_piat_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------OBTAIN THE MEAN OF STANDARD DEVIATIONS OF PACKET IAT SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# # Obtain a summary of the total number of packets sent in both directions per application label for each src ip
mean_std_dev_piat_per_app <- as.data.frame(tapply(df$std_dev_piat, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_std_dev_piat_per_app) <- 1:nrow(mean_std_dev_piat_per_app)
#Replace NA with 0
mean_std_dev_piat_per_app[is.na((mean_std_dev_piat_per_app))] <- 0

#Set the column names in their order - 141 applications - Complete dataset
colnames(mean_std_dev_piat_per_app) <- c("104_mean_std_dev_piat",                  "AJP_mean_std_dev_piat",                  "Amazon_mean_std_dev_piat",               "AmazonVideo_mean_std_dev_piat",          "Apple_mean_std_dev_piat",                "AppleiCloud_mean_std_dev_piat",          "AppleiTunes_mean_std_dev_piat",          "ApplePush_mean_std_dev_piat",           
                                     "AppleStore_mean_std_dev_piat",           "BGP_mean_std_dev_piat",                  "BitTorrent_mean_std_dev_piat",           "BJNP_mean_std_dev_piat",                 "CiscoSkinny_mean_std_dev_piat",          "CiscoVPN_mean_std_dev_piat",             "Citrix_mean_std_dev_piat",               "Cloudflare_mean_std_dev_piat",          
                                     "CNN_mean_std_dev_piat",                  "DataSaver_mean_std_dev_piat",            "Deezer_mean_std_dev_piat",               "DHCP_mean_std_dev_piat",                 "Direct_Download_Link_mean_std_dev_piat", "DNP3_mean_std_dev_piat",                 "DNS_mean_std_dev_piat",                  "DNSoverHTTPS_mean_std_dev_piat",        
                                     "Dropbox_mean_std_dev_piat",              "eBay_mean_std_dev_piat",                 "eDonkey_mean_std_dev_piat",              "Facebook_mean_std_dev_piat",             "FTP_CONTROL_mean_std_dev_piat",          "FTP_DATA_mean_std_dev_piat",             "Github_mean_std_dev_piat",               "GMail_mean_std_dev_piat",               
                                     "Google_mean_std_dev_piat",               "GoogleDocs_mean_std_dev_piat",           "GoogleDrive_mean_std_dev_piat",          "GoogleHangoutDuo_mean_std_dev_piat",     "GoogleMaps_mean_std_dev_piat",           "GooglePlus_mean_std_dev_piat",           "GoogleServices_mean_std_dev_piat",       "GTP_mean_std_dev_piat",                 
                                     "H323_mean_std_dev_piat",                 "HotspotShield_mean_std_dev_piat",        "HTTP_mean_std_dev_piat",                 "HTTP_Proxy_mean_std_dev_piat",           "IAX_mean_std_dev_piat",                  "ICMP_mean_std_dev_piat",                 "IMAPS_mean_std_dev_piat",                "IMO_mean_std_dev_piat",                 
                                     "Instagram_mean_std_dev_piat",            "IPsec_mean_std_dev_piat",                "IRC_mean_std_dev_piat",                  "LDAP_mean_std_dev_piat",                 "LinkedIn_mean_std_dev_piat",             "LotusNotes_mean_std_dev_piat",           "MDNS_mean_std_dev_piat",                 "Messenger_mean_std_dev_piat",           
                                     "Microsoft_mean_std_dev_piat",            "Mining_mean_std_dev_piat",               "MQTT_mean_std_dev_piat",                 "MS_OneDrive_mean_std_dev_piat",          "MSN_mean_std_dev_piat",                  "MsSQL-TDS_mean_std_dev_piat",            "MySQL_mean_std_dev_piat",                "NestLogSink_mean_std_dev_piat",         
                                     "NetBIOS_mean_std_dev_piat",              "NetFlix_mean_std_dev_piat",              "NFS_mean_std_dev_piat",                  "NTP_mean_std_dev_piat",                  "Office365_mean_std_dev_piat",            "Ookla_mean_std_dev_piat",                "OpenDNS_mean_std_dev_piat",              "OpenVPN_mean_std_dev_piat",             
                                     "Oracle_mean_std_dev_piat",               "Pando_Media_Booster_mean_std_dev_piat",  "Playstation_mean_std_dev_piat",          "PlayStore_mean_std_dev_piat",            "POP3_mean_std_dev_piat",                 "PostgreSQL_mean_std_dev_piat",           "PS_VUE_mean_std_dev_piat",               "QQ_mean_std_dev_piat",                  
                                     "QUIC_mean_std_dev_piat",                 "Radius_mean_std_dev_piat",               "RDP_mean_std_dev_piat",                  "RTMP_mean_std_dev_piat",                 "RTP_mean_std_dev_piat",                  "RTSP_mean_std_dev_piat",                 "RX_mean_std_dev_piat",                   "SAP_mean_std_dev_piat",                 
                                     "sFlow_mean_std_dev_piat",                "Signal_mean_std_dev_piat",               "Sina(Weibo)_mean_std_dev_piat",          "SIP_mean_std_dev_piat",                  "Skype_mean_std_dev_piat",                "SkypeCall_mean_std_dev_piat",            "Slack_mean_std_dev_piat",                "SMBv1_mean_std_dev_piat",               
                                     "SMBv23_mean_std_dev_piat",               "SMTP_mean_std_dev_piat",                 "SMTPS_mean_std_dev_piat",                "Snapchat_mean_std_dev_piat",             "SNMP_mean_std_dev_piat",                 "SOCKS_mean_std_dev_piat",                "SOMEIP_mean_std_dev_piat",               "SoundCloud_mean_std_dev_piat",          
                                     "Spotify_mean_std_dev_piat",              "SSDP_mean_std_dev_piat",                 "SSH_mean_std_dev_piat",                  "Starcraft_mean_std_dev_piat",            "Steam_mean_std_dev_piat",                "STUN_mean_std_dev_piat",                 "Syslog_mean_std_dev_piat",               "Targus Dataspeed_mean_std_dev_piat",    
                                     "TeamViewer_mean_std_dev_piat",           "Telegram_mean_std_dev_piat",             "Teredo_mean_std_dev_piat",               "TikTok_mean_std_dev_piat",               "TLS_mean_std_dev_piat",                  "Tor_mean_std_dev_piat",                  "Tuenti_mean_std_dev_piat",               "Twitch_mean_std_dev_piat",              
                                     "Twitter_mean_std_dev_piat",              "UBNTAC2_mean_std_dev_piat",              "UbuntuONE_mean_std_dev_piat",            "Unencrypted_Jabber_mean_std_dev_piat",   "Unknown_mean_std_dev_piat",              "UPnP_mean_std_dev_piat",                 "Viber_mean_std_dev_piat",                "VNC_mean_std_dev_piat",                 
                                     "Waze_mean_std_dev_piat",                 "Webex_mean_std_dev_piat",                "WeChat_mean_std_dev_piat",               "WhatsApp_mean_std_dev_piat",             "WhatsAppCall_mean_std_dev_piat",         "WhatsAppFiles_mean_std_dev_piat",        "Whois-DAS_mean_std_dev_piat",            "Wikipedia_mean_std_dev_piat",           
                                     "WindowsUpdate_mean_std_dev_piat",        "Xbox_mean_std_dev_piat",                 "Yahoo_mean_std_dev_piat",                "YouTube_mean_std_dev_piat",              "Zoom_mean_std_dev_piat")

head(mean_std_dev_piat_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------JOIN COLUMNS AND GENERATE THE SUMMARIZED DATASET (TOTAL FLOWS; MEAN PKT SIZE; MEAN FLOW DURATION; TOTAL PACKETS) ------------------------------------------------------------------------------------
summarized_dataset <- cbind(flows_per_app, mean_octet_total_count.df)
summarized_dataset <- cbind(summarized_dataset, mean_flow_duration.df)
summarized_dataset <- cbind(summarized_dataset, mean_packetsNumber_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_min_packetSize_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_max_packetSize_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_avg_packetSize_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_std_dev_packetSize_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_min_piat_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_max_piat_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_avg_piat_per_app)
summarized_dataset <- cbind(summarized_dataset, mean_std_dev_piat_per_app)

# Convert src ip adresses from network format to decimal format
summarized_dataset[, c("src_ip_numeric")] <- unique(df[, c("src_ip_numeric")])

# Order the rows in ascending order based on the value of the src ip decimal number
summarized_dataset <- summarized_dataset[order(summarized_dataset[,c("src_ip_numeric")]),]

# Reorder the columns so the decimal form of the src ip is first
summarized_dataset <- subset(summarized_dataset, select=c(1694,1:1693))


write.csv(summarized_dataset, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Summarized Dataset 2019 - all Apps - Analysis/Unicauca_Users-summarized-dataset-April_June_2019-All_Attributes-AllApps.csv", row.names = FALSE, quote = FALSE, sep=",")
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------OBTAIN CONSUMPTION INTENSITY OF THE NETWORK---------------------------------------------------------------------------------------------------------
# Remove the unused variables
rm(dataset)
rm(df)
rm(flows_per_app)
rm(labels)
rm(mean_octet_total_count.df)
rm(mean_flow_duration.df)
rm(total_packets_per_app)
gc()

summarized_dataset <- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Summarized Dataset 2019 - all Apps - Analysis/Unicauca_Users-summarized-dataset-April_June_2019-All_Attributes-AllApps.csv")

# Obtain the occupation of the network in terms of time (seconds)
'104_time_occupation' <- summarized_dataset$X104_flows * summarized_dataset$X104_mean_flow_duration
AJP_time_occupation <- summarized_dataset$AJP_flows * summarized_dataset$AJP_mean_flow_duration
Amazon_time_occupation <- summarized_dataset$Amazon_flows * summarized_dataset$Amazon_mean_flow_duration
AmazonVideo_time_occupation <- summarized_dataset$AmazonVideo_flows * summarized_dataset$Amazon_mean_flow_duration          
Apple_time_occupation <- summarized_dataset$Apple_flows * summarized_dataset$Apple_mean_flow_duration                
AppleiCloud_time_occupation <- summarized_dataset$AppleiCloud_flows * summarized_dataset$AppleiCloud_mean_flow_duration          
AppleiTunes_time_occupation <- summarized_dataset$AppleiTunes_flows * summarized_dataset$AppleiTunes_mean_flow_duration          
ApplePush_time_occupation <- summarized_dataset$ApplePush_flows * summarized_dataset$ApplePush_mean_flow_duration           
AppleStore_time_occupation <- summarized_dataset$AppleStore_flows * summarized_dataset$AppleStore_mean_flow_duration           
BGP_time_occupation <- summarized_dataset$BGP_flows * summarized_dataset$BGP_mean_flow_duration                  
BitTorrent_time_occupation <- summarized_dataset$BitTorrent_flows * summarized_dataset$BitTorrent_mean_flow_duration           
BJNP_time_occupation <- summarized_dataset$BJNP_flows * summarized_dataset$BJNP_mean_flow_duration                 
CiscoSkinny_time_occupation <- summarized_dataset$CiscoSkinny_flows * summarized_dataset$CiscoSkinny_mean_flow_duration          
CiscoVPN_time_occupation <- summarized_dataset$CiscoVPN_flows * summarized_dataset$CiscoVPN_mean_flow_duration             
Citrix_time_occupation <- summarized_dataset$Citrix_flows * summarized_dataset$Citrix_mean_flow_duration               
Cloudflare_time_occupation <- summarized_dataset$Cloudflare_flows * summarized_dataset$Cloudflare_mean_flow_duration          
CNN_time_occupation <- summarized_dataset$CNN_flows * summarized_dataset$CNN_mean_flow_duration                  
DataSaver_time_occupation <- summarized_dataset$DataSaver_flows * summarized_dataset$DataSaver_mean_flow_duration            
Deezer_time_occupation <- summarized_dataset$Deezer_flows * summarized_dataset$Deezer_mean_flow_duration             
DHCP_time_occupation  <- summarized_dataset$DHCP_flows * summarized_dataset$DHCP_mean_flow_duration               
Direct_Download_Link_time_occupation <- summarized_dataset$Direct_Download_Link_flows * summarized_dataset$Direct_Download_Link_mean_flow_duration
DNP3_time_occupation <- summarized_dataset$DNP3_flows * summarized_dataset$DNP3_mean_flow_duration
DNS_time_occupation <- summarized_dataset$DNS_flows * summarized_dataset$DNS_mean_flow_duration
DNSoverHTTPS_time_occupation <- summarized_dataset$DNSoverHTTPS_flows * summarized_dataset$DNSoverHTTPS_mean_flow_duration
Dropbox_time_occupation <- summarized_dataset$Dropbox_flows * summarized_dataset$Dropbox_mean_flow_duration
eBay_time_occupation <- summarized_dataset$eBay_flows * summarized_dataset$eBay_mean_flow_duration
eDonkey_time_occupation <- summarized_dataset$eDonkey_flows * summarized_dataset$eDonkey_mean_flow_duration
Facebook_time_occupation <- summarized_dataset$Facebook_flows * summarized_dataset$Facebook_mean_flow_duration            
FTP_CONTROL_time_occupation <- summarized_dataset$FTP_CONTROL_flows * summarized_dataset$FTP_CONTROL_mean_flow_duration         
FTP_DATA_time_occupation <- summarized_dataset$FTP_DATA_flows * summarized_dataset$FTP_DATA_mean_flow_duration            
Github_time_occupation <- summarized_dataset$Github_flows * summarized_dataset$Github_mean_flow_duration              
GMail_time_occupation <- summarized_dataset$GMail_flows * summarized_dataset$GMail_mean_flow_duration             
Google_time_occupation  <- summarized_dataset$Google_flows * summarized_dataset$Google_mean_flow_duration            
GoogleDocs_time_occupation <- summarized_dataset$GoogleDocs_flows * summarized_dataset$GoogleDocs_mean_flow_duration           
GoogleDrive_time_occupation <- summarized_dataset$GoogleDrive_flows * summarized_dataset$GoogleDrive_mean_flow_duration          
GoogleHangoutDuo_time_occupation <- summarized_dataset$GoogleHangoutDuo_flows * summarized_dataset$GoogleHangoutDuo_mean_flow_duration     
GoogleMaps_time_occupation <- summarized_dataset$GoogleMaps_flows * summarized_dataset$GoogleMaps_mean_flow_duration          
GooglePlus_time_occupation <- summarized_dataset$GooglePlus_flows * summarized_dataset$GooglePlus_mean_flow_duration          
GoogleServices_time_occupation <- summarized_dataset$GoogleServices_flows * summarized_dataset$GoogleServices_mean_flow_duration      
GTP_time_occupation <- summarized_dataset$GTP_flows * summarized_dataset$GTP_mean_flow_duration               
H323_time_occupation <- summarized_dataset$H323_flows * summarized_dataset$H323_mean_flow_duration              
HotspotShield_time_occupation <- summarized_dataset$HotspotShield_flows * summarized_dataset$HotspotShield_mean_flow_duration       
HTTP_time_occupation <- summarized_dataset$HTTP_flows * summarized_dataset$HTTP_mean_flow_duration                 
HTTP_Proxy_time_occupation <- summarized_dataset$HTTP_Proxy_flows * summarized_dataset$HTTP_Proxy_mean_flow_duration          
IAX_time_occupation <- summarized_dataset$IAX_flows * summarized_dataset$IAX_mean_flow_duration                  
ICMP_time_occupation <- summarized_dataset$ICMP_flows * summarized_dataset$ICMP_mean_flow_duration               
IMAPS_time_occupation <- summarized_dataset$IMAPS_flows * summarized_dataset$IMAPS_mean_flow_duration               
IMO_time_occupation <- summarized_dataset$IMO_flows * summarized_dataset$IMO_mean_flow_duration                 
Instagram_time_occupation <- summarized_dataset$Instagram_flows * summarized_dataset$Instagram_mean_flow_duration            
IPsec_time_occupation <- summarized_dataset$IPsec_flows * summarized_dataset$IPsec_mean_flow_duration                
IRC_time_occupation <- summarized_dataset$IRC_flows * summarized_dataset$IRC_mean_flow_duration                 
LDAP_time_occupation <- summarized_dataset$LDAP_flows * summarized_dataset$LDAP_mean_flow_duration                
LinkedIn_time_occupation <- summarized_dataset$LinkedIn_flows * summarized_dataset$LinkedIn_mean_flow_duration           
LotusNotes_time_occupation <- summarized_dataset$LotusNotes_flows * summarized_dataset$LotusNotes_mean_flow_duration           
MDNS_time_occupation <- summarized_dataset$MDNS_flows * summarized_dataset$MDNS_mean_flow_duration                 
Messenger_time_occupation <- summarized_dataset$Messenger_flows * summarized_dataset$Messenger_mean_flow_duration          
Microsoft_time_occupation <- summarized_dataset$Microsoft_flows * summarized_dataset$Microsoft_mean_flow_duration            
Mining_time_occupation <- summarized_dataset$Mining_flows * summarized_dataset$Mining_mean_flow_duration              
MQTT_time_occupation <- summarized_dataset$MQTT_flows * summarized_dataset$MQTT_mean_flow_duration                
MS_OneDrive_time_occupation <- summarized_dataset$MS_OneDrive_flows * summarized_dataset$MS_OneDrive_mean_flow_duration        
MSN_time_occupation <- summarized_dataset$MSN_flows * summarized_dataset$MSN_mean_flow_duration             
'MsSQL-TDS_time_occupation' <- summarized_dataset$MsSQL.TDS_flows* summarized_dataset$MsSQL.TDS_mean_flow_duration          
MySQL_time_occupation <- summarized_dataset$MySQL_flows * summarized_dataset$MySQL_mean_flow_duration              
NestLogSink_time_occupation <- summarized_dataset$NestLogSink_flows * summarized_dataset$NestLogSink_mean_flow_duration        
NetBIOS_time_occupation <- summarized_dataset$NetBIOS_flows * summarized_dataset$NetBIOS_mean_flow_duration             
NetFlix_time_occupation <- summarized_dataset$NetFlix_flows * summarized_dataset$NetFlix_mean_flow_duration            
NFS_time_occupation <- summarized_dataset$NFS_flows * summarized_dataset$NFS_mean_flow_duration                
NTP_time_occupation <- summarized_dataset$NTP_flows * summarized_dataset$NTP_mean_flow_duration                
Office365_time_occupation <- summarized_dataset$Office365_flows * summarized_dataset$Office365_mean_flow_duration          
Ookla_time_occupation <- summarized_dataset$Ookla_flows * summarized_dataset$Ookla_mean_flow_duration               
OpenDNS_time_occupation <- summarized_dataset$OpenDNS_flows * summarized_dataset$OpenDNS_mean_flow_duration             
OpenVPN_time_occupation <- summarized_dataset$OpenVPN_flows * summarized_dataset$OpenVPN_mean_flow_duration             
Oracle_time_occupation <- summarized_dataset$Oracle_flows * summarized_dataset$Oracle_mean_flow_duration    
Pando_Media_Booster_time_occupation <- summarized_dataset$Pando_Media_Booster_flows * summarized_dataset$Pando_Media_Booster_mean_flow_duration 
Playstation_time_occupation <- summarized_dataset$Playstation_flows * summarized_dataset$Playstation_mean_flow_duration          
PlayStore_time_occupation <- summarized_dataset$PlayStore_flows * summarized_dataset$PlayStore_mean_flow_duration            
POP3_time_occupation <- summarized_dataset$POP3_flows * summarized_dataset$POP3_mean_flow_duration             
PostgreSQL_time_occupation <- summarized_dataset$PostgreSQL_flows * summarized_dataset$PostgreSQL_mean_flow_duration           
PS_VUE_time_occupation <- summarized_dataset$PS_VUE_flows * summarized_dataset$PS_VUE_mean_flow_duration               
QQ_time_occupation <- summarized_dataset$QQ_flows * summarized_dataset$QQ_mean_flow_duration                  
QUIC_time_occupation <- summarized_dataset$QUIC_flows * summarized_dataset$QUIC_mean_flow_duration                 
Radius_time_occupation <- summarized_dataset$Radius_flows * summarized_dataset$Radius_mean_flow_duration               
RDP_time_occupation <- summarized_dataset$RDP_flows * summarized_dataset$RDP_mean_flow_duration                  
RTMP_time_occupation <- summarized_dataset$RTMP_flows * summarized_dataset$RTMP_mean_flow_duration                 
RTP_time_occupation <- summarized_dataset$RTP_flows * summarized_dataset$RTP_mean_flow_duration
RTSP_time_occupation <- summarized_dataset$RTSP_flows * summarized_dataset$RTSP_mean_flow_duration
RX_time_occupation <- summarized_dataset$RX_flows * summarized_dataset$RX_mean_flow_duration                  
SAP_time_occupation <- summarized_dataset$SAP_flows * summarized_dataset$SAP_mean_flow_duration                 
sFlow_time_occupation <- summarized_dataset$sFlow_flows * summarized_dataset$sFlow_mean_flow_duration
Signal_time_occupation <- summarized_dataset$Signal_flows * summarized_dataset$Signal_mean_flow_duration
Sina_Weibo_time_occupation <- summarized_dataset$Sina.Weibo._flows* summarized_dataset$Sina.Weibo._mean_flow_duration
SIP_time_occupation <- summarized_dataset$SIP_flows * summarized_dataset$SIP_mean_flow_duration                  
Skype_time_occupation <- summarized_dataset$Skype_flows * summarized_dataset$Skype_mean_flow_duration
SkypeCall_time_occupation <- summarized_dataset$SkypeCall_flows * summarized_dataset$Skype_mean_flow_duration
Slack_time_occupation <- summarized_dataset$Slack_flows * summarized_dataset$Slack_mean_flow_duration
SMBv1_time_occupation <- summarized_dataset$SMBv1_flows * summarized_dataset$SMBv1_mean_flow_duration
SMBv23_time_occupation <- summarized_dataset$SMBv23_flows * summarized_dataset$SMBv23_mean_flow_duration
SMTP_time_occupation <- summarized_dataset$SMTP_flows * summarized_dataset$SMTP_mean_flow_duration                 
SMTPS_time_occupation <- summarized_dataset$SMTPS_flows * summarized_dataset$SMTPS_mean_flow_duration
Snapchat_time_occupation <- summarized_dataset$Snapchat_flows * summarized_dataset$Snapchat_mean_flow_duration
SNMP_time_occupation <- summarized_dataset$SNMP_flows * summarized_dataset$SNMP_mean_flow_duration
SOCKS_time_occupation <- summarized_dataset$SOCKS_flows * summarized_dataset$SOCKS_mean_flow_duration
SOMEIP_time_occupation <- summarized_dataset$SOMEIP_flows * summarized_dataset$SOMEIP_mean_flow_duration
SoundCloud_time_occupation <- summarized_dataset$SoundCloud_flows * summarized_dataset$SoundCloud_mean_flow_duration
Spotify_time_occupation <- summarized_dataset$Spotify_flows * summarized_dataset$Spotify_mean_flow_duration
SSDP_time_occupation <- summarized_dataset$SSDP_flows * summarized_dataset$SSDP_mean_flow_duration
SSH_time_occupation <- summarized_dataset$SSH_flows * summarized_dataset$SSH_mean_flow_duration
Starcraft_time_occupation <- summarized_dataset$Starcraft_flows * summarized_dataset$Starcraft_mean_flow_duration
Steam_time_occupation <- summarized_dataset$Steam_flows * summarized_dataset$Steam_mean_flow_duration
STUN_time_occupation <- summarized_dataset$STUN_flows * summarized_dataset$STUN_mean_flow_duration                  
Syslog_time_occupation <- summarized_dataset$Syslog_flows * summarized_dataset$Syslog_mean_flow_duration       
Targus_Dataspeed_time_occupation <- summarized_dataset$Targus_Dataspeed_flows * summarized_dataset$Targus.Dataspeed_mean_flow_duration
TeamViewer_time_occupation <- summarized_dataset$TeamViewer_flows * summarized_dataset$TeamViewer_mean_flow_duration
Telegram_time_occupation <- summarized_dataset$Telegram_flows * summarized_dataset$Telegram_mean_flow_duration             
Teredo_time_occupation <- summarized_dataset$Teredo_flows * summarized_dataset$Teredo_mean_flow_duration               
TikTok_time_occupation <- summarized_dataset$TikTok_flows * summarized_dataset$TikTok_mean_flow_duration               
TLS_time_occupation <- summarized_dataset$TLS_flows * summarized_dataset$TLS_mean_flow_duration                  
Tor_time_occupation <- summarized_dataset$Tor_flows * summarized_dataset$Tor_mean_flow_duration                 
Tuenti_time_occupation <- summarized_dataset$Tuenti_flows * summarized_dataset$Tuenti_mean_flow_duration               
Twitch_time_occupation <- summarized_dataset$Twitch_flows * summarized_dataset$Twitch_mean_flow_duration              
Twitter_time_occupation <- summarized_dataset$Twitter_flows * summarized_dataset$Twitter_mean_flow_duration              
UBNTAC2_time_occupation <- summarized_dataset$UBNTAC2_flows * summarized_dataset$UBNTAC2_mean_flow_duration              
UbuntuONE_time_occupation <- summarized_dataset$UbuntuONE_flows * summarized_dataset$UbuntuONE_mean_flow_duration     
Unencrypted_Jabber_time_occupation <- summarized_dataset$Unencrypted_Jabber_flows * summarized_dataset$Unencrypted_Jabber_mean_flow_duration   
Unknown_time_occupation <- summarized_dataset$Unknown_flows * summarized_dataset$Unknown_mean_flow_duration              
UPnP_time_occupation <- summarized_dataset$UPnP_flows * summarized_dataset$UPnP_mean_flow_duration                 
Viber_time_occupation <- summarized_dataset$Viber_flows * summarized_dataset$Viber_mean_flow_duration                
VNC_time_occupation <- summarized_dataset$VNC_flows * summarized_dataset$VNC_mean_flow_duration                 
Waze_time_occupation <- summarized_dataset$Waze_flows * summarized_dataset$Waze_mean_flow_duration                 
Webex_time_occupation <- summarized_dataset$Webex_flows * summarized_dataset$Webex_mean_flow_duration                
WeChat_time_occupation <- summarized_dataset$WeChat_flows * summarized_dataset$WeChat_mean_flow_duration               
WhatsApp_time_occupation <- summarized_dataset$WhatsApp_flows * summarized_dataset$WhatsApp_mean_flow_duration           
WhatsAppCall_time_occupation <- summarized_dataset$WhatsAppCall_flows * summarized_dataset$WhatsAppCall_mean_flow_duration         
WhatsAppFiles_time_occupation <- summarized_dataset$WhatsAppFiles_flows * summarized_dataset$WhatsAppFiles_mean_flow_duration
'Whois-DAS_time_occupation' <- summarized_dataset$Whois.DAS_flows* summarized_dataset$Whois.DAS_mean_flow_duration            
Wikipedia_time_occupation <- summarized_dataset$Wikipedia_flows * summarized_dataset$Wikipedia_mean_flow_duration         
WindowsUpdate_time_occupation <- summarized_dataset$WindowsUpdate_flows * summarized_dataset$WindowsUpdate_mean_flow_duration
Xbox_time_occupation <- summarized_dataset$Xbox_flows * summarized_dataset$Xbox_mean_flow_duration                 
Yahoo_time_occupation <- summarized_dataset$Yahoo_flows * summarized_dataset$Yahoo_mean_flow_duration               
YouTube_time_occupation <- summarized_dataset$YouTube_flows * summarized_dataset$YouTube_mean_flow_duration 
Zoom_time_occupation <- summarized_dataset$Zoom_flows * summarized_dataset$Zoom_mean_flow_duration

# Obtain the occupation of the network in terms of quantity of transferred data (bytes)
# AJP_data_occupation <- summarized_dataset$AJP_mean_pkt_size * summarized_dataset$AJP_total_packets
# Amazon_data_occupation <- summarized_dataset$Amazon_mean_pkt_size * summarized_dataset$Amazon_total_packets
# AmazonVideo_data_occupation <- summarized_dataset$AmazonVideo_mean_pkt_size * summarized_dataset$AmazonVideo_total_packets          
# Apple_data_occupation <- summarized_dataset$Apple_mean_pkt_size * summarized_dataset$Apple_total_packets                
# AppleiCloud_data_occupation <- summarized_dataset$AppleiCloud_mean_pkt_size * summarized_dataset$AppleiCloud_total_packets          
# AppleiTunes_data_occupation <- summarized_dataset$AppleiTunes_mean_pkt_size * summarized_dataset$AppleiTunes_total_packets          
# ApplePush_data_occupation <- summarized_dataset$ApplePush_mean_pkt_size * summarized_dataset$ApplePush_total_packets           
# AppleStore_data_occupation <- summarized_dataset$AppleStore_mean_pkt_size * summarized_dataset$AppleStore_total_packets           
# BGP_data_occupation <- summarized_dataset$BGP_mean_pkt_size * summarized_dataset$BGP_total_packets                  
# BitTorrent_data_occupation <- summarized_dataset$BitTorrent_mean_pkt_size * summarized_dataset$BitTorrent_total_packets           
# BJNP_data_occupation <- summarized_dataset$BJNP_mean_pkt_size * summarized_dataset$BJNP_total_packets                 
# CiscoSkinny_data_occupation <- summarized_dataset$CiscoSkinny_mean_pkt_size * summarized_dataset$CiscoSkinny_total_packets          
# CiscoVPN_data_occupation <- summarized_dataset$CiscoVPN_mean_pkt_size * summarized_dataset$CiscoVPN_total_packets             
# Citrix_data_occupation <- summarized_dataset$Citrix_mean_pkt_size * summarized_dataset$Citrix_total_packets               
# Cloudflare_data_occupation <- summarized_dataset$Cloudflare_mean_pkt_size * summarized_dataset$Cloudflare_total_packets          
# CNN_data_occupation <- summarized_dataset$CNN_mean_pkt_size * summarized_dataset$CNN_total_packets                  
# DataSaver_data_occupation <- summarized_dataset$DataSaver_mean_pkt_size * summarized_dataset$DataSaver_total_packets            
# Deezer_data_occupation <- summarized_dataset$Deezer_mean_pkt_size * summarized_dataset$Deezer_total_packets             
# DHCP_data_occupation <- summarized_dataset$DHCP_mean_pkt_size * summarized_dataset$DHCP_total_packets                 
# Direct_Download_Link_data_occupation <- summarized_dataset$Direct_Download_Link_mean_pkt_size * summarized_dataset$Direct_Download_Link_total_packets
# DNP3_data_occupation <- summarized_dataset$DNP3_mean_pkt_size * summarized_dataset$DNP3_total_packets
# DNS_data_occupation <- summarized_dataset$DNS_mean_pkt_size * summarized_dataset$DNS_total_packets
# DNSoverHTTPS_data_occupation <- summarized_dataset$DNSoverHTTPS_mean_pkt_size * summarized_dataset$DNSoverHTTPS_total_packets
# Dropbox_data_occupation <- summarized_dataset$Dropbox_mean_pkt_size * summarized_dataset$Dropbox_total_packets
# eBay_data_occupation <- summarized_dataset$eBay_mean_pkt_size * summarized_dataset$eBay_total_packets
# eDonkey_data_occupation <- summarized_dataset$eDonkey_mean_pkt_size * summarized_dataset$eDonkey_total_packets
# Facebook_data_occupation <- summarized_dataset$Facebook_mean_pkt_size * summarized_dataset$Facebook_total_packets            
# FTP_CONTROL_total_packet <- summarized_dataset$FTP_CONTROL_mean_pkt_size * summarized_dataset$FTP_CONTROL_total_packets         
# FTP_DATA_total_packet <- summarized_dataset$FTP_DATA_mean_pkt_size * summarized_dataset$FTP_DATA_total_packets            
# Github_data_occupation <- summarized_dataset$Github_mean_pkt_size * summarized_dataset$Github_total_packets            
# GMail_data_occupation <- summarized_dataset$GMail_mean_pkt_size * summarized_dataset$GMail_total_packets              
# Google_data_occupation <- summarized_dataset$Google_mean_pkt_size * summarized_dataset$Google_total_packets              
# GoogleDocs_data_occupation <- summarized_dataset$GoogleDocs_mean_pkt_size * summarized_dataset$GoogleDocs_total_packets           
# GoogleDrive_data_occupation <- summarized_dataset$GoogleDrive_mean_pkt_size * summarized_dataset$GoogleDrive_total_packets          
# GoogleHangoutDuo_data_occupation <- summarized_dataset$GoogleHangoutDuo_mean_pkt_size * summarized_dataset$GoogleHangoutDuo_total_packets     
# GoogleMaps_data_occupation <- summarized_dataset$GoogleMaps_mean_pkt_size * summarized_dataset$GoogleMaps_total_packets          
# GooglePlus_data_occupation <- summarized_dataset$GooglePlus_mean_pkt_size * summarized_dataset$GooglePlus_total_packets          
# GoogleServices_data_occupation <- summarized_dataset$GoogleServices_mean_pkt_size * summarized_dataset$GoogleServices_total_packets      
# GTP_data_occupation <- summarized_dataset$GTP_mean_pkt_size * summarized_dataset$GTP_total_packets               
# H323_data_occupation <- summarized_dataset$H323_mean_pkt_size * summarized_dataset$H323_total_packets              
# HotspotShield_data_occupation <- summarized_dataset$HotspotShield_mean_pkt_size * summarized_dataset$HotspotShield_total_packets       
# HTTP_data_occupation <- summarized_dataset$HTTP_mean_pkt_size * summarized_dataset$HTTP_total_packets                 
# HTTP_Proxy_data_occupation <- summarized_dataset$HTTP_Proxy_mean_pkt_size * summarized_dataset$HTTP_Proxy_total_packets          
# IAX_data_occupation <- summarized_dataset$IAX_mean_pkt_size * summarized_dataset$IAX_total_packets                  
# ICMP_data_occupation <- summarized_dataset$ICMP_mean_pkt_size * summarized_dataset$ICMP_total_packets               
# IMAPS_data_occupation <- summarized_dataset$IMAPS_mean_pkt_size * summarized_dataset$IMAPS_total_packets               
# IMO_data_occupation <- summarized_dataset$IMO_mean_pkt_size * summarized_dataset$IMO_total_packets                 
# Instagram_data_occupation <- summarized_dataset$Instagram_mean_pkt_size * summarized_dataset$Instagram_total_packets            
# IPsec_data_occupation <- summarized_dataset$IPsec_mean_pkt_size * summarized_dataset$IPsec_total_packets                
# IRC_data_occupation <- summarized_dataset$IRC_mean_pkt_size * summarized_dataset$IRC_total_packets                  
# LDAP_data_occupation <- summarized_dataset$LDAP_mean_pkt_size * summarized_dataset$LDAP_total_packets                
# LinkedIn_data_occupation <- summarized_dataset$LinkedIn_mean_pkt_size * summarized_dataset$LinkedIn_total_packets           
# LotusNotes_data_occupation <- summarized_dataset$LotusNotes_mean_pkt_size * summarized_dataset$LotusNotes_total_packets           
# MDNS_data_occupation <- summarized_dataset$MDNS_mean_pkt_size * summarized_dataset$MDNS_total_packets                 
# Messenger_data_occupation <- summarized_dataset$Messenger_mean_pkt_size * summarized_dataset$Messenger_total_packets          
# Microsoft_data_occupation <- summarized_dataset$Microsoft_mean_pkt_size * summarized_dataset$Microsoft_total_packets            
# Mining_data_occupation <- summarized_dataset$Mining_mean_pkt_size * summarized_dataset$Mining_total_packets              
# MQTT_data_occupation <- summarized_dataset$MQTT_mean_pkt_size * summarized_dataset$MQTT_total_packets                
# MS_OneDrive_data_occupation <- summarized_dataset$MS_OneDrive_mean_pkt_size * summarized_dataset$MS_OneDrive_total_packets        
# MSN_data_occupation <- summarized_dataset$MSN_mean_pkt_size * summarized_dataset$MSN_total_packets             
# MsSQL-TDS_data_occupation <- summarized_dataset$`MsSQL-TDS_mean_pkt_size` * summarized_dataset$`MsSQL-TDS_total_packets`          
# MySQL_data_occupation <- summarized_dataset$MySQL_mean_pkt_size * summarized_dataset$MySQL_total_packets              
# NestLogSink_data_occupation <- summarized_dataset$NestLogSink_mean_pkt_size * summarized_dataset$NestLogSink_total_packets        
# NetBIOS_data_occupation <- summarized_dataset$NetBIOS_mean_pkt_size * summarized_dataset$NetBIOS_total_packets             
# NetFlix_data_occupation <- summarized_dataset$NetFlix_mean_pkt_size * summarized_dataset$NetFlix_total_packets            
# NFS_data_occupation <- summarized_dataset$NFS_mean_pkt_size * summarized_dataset$NFS_total_packets                
# NTP_data_occupation <- summarized_dataset$NTP_mean_pkt_size * summarized_dataset$NTP_total_packets
# Office365_data_occupation <- summarized_dataset$Office365_mean_pkt_size * summarized_dataset$Office365_total_packets          
# Ookla_data_occupation <- summarized_dataset$Ookla_mean_pkt_size * summarized_dataset$Ookla_total_packets               
# OpenDNS_data_occupation <- summarized_dataset$OpenDNS_mean_pkt_size * summarized_dataset$OpenDNS_total_packets             
# OpenVPN_data_occupation <- summarized_dataset$OpenVPN_mean_pkt_size * summarized_dataset$OpenVPN_total_packets             
# Oracle_data_occupation <- summarized_dataset$Oracle_mean_pkt_size * summarized_dataset$Oracle_total_packets    
# Pando_Media_Booster_data_occupation <- summarized_dataset$Pando_Media_Booster_mean_pkt_size * summarized_dataset$Pando_Media_Booster_total_packets 
# Playstation_data_occupation <- summarized_dataset$Playstation_mean_pkt_size * summarized_dataset$Playstation_total_packets          
# PlayStore_data_occupation <- summarized_dataset$PlayStore_mean_pkt_size * summarized_dataset$PlayStore_total_packets            
# POP3_data_occupation <- summarized_dataset$POP3_mean_pkt_size * summarized_dataset$POP3_total_packets             
# PostgreSQL_data_occupation <- summarized_dataset$PostgreSQL_mean_pkt_size * summarized_dataset$PostgreSQL_total_packets           
# PS_VUE_data_occupation <- summarized_dataset$PS_VUE_mean_pkt_size * summarized_dataset$PS_VUE_total_packets               
# QQ_data_occupation <- summarized_dataset$QQ_mean_pkt_size * summarized_dataset$QQ_total_packets                  
# QUIC_data_occupation <- summarized_dataset$QUIC_mean_pkt_size * summarized_dataset$QUIC_total_packets                 
# Radius_data_occupation <- summarized_dataset$Radius_mean_pkt_size * summarized_dataset$Radius_total_packets               
# RDP_data_occupation <- summarized_dataset$RDP_mean_pkt_size * summarized_dataset$RDP_total_packets                  
# RTMP_data_occupation <- summarized_dataset$RTMP_mean_pkt_size * summarized_dataset$RTMP_total_packets                 
# RTP_data_occupation <- summarized_dataset$RTP_mean_pkt_size * summarized_dataset$RTP_total_packets
# RTSP_data_occupation <- summarized_dataset$RTSP_mean_pkt_size * summarized_dataset$RTSP_total_packets
# RX_data_occupation <- summarized_dataset$RX_mean_pkt_size * summarized_dataset$RX_total_packets                  
# SAP_data_occupation <- summarized_dataset$SAP_mean_pkt_size * summarized_dataset$SAP_total_packets                 
# sFlow_data_occupation <- summarized_dataset$sFlow_mean_pkt_size * summarized_dataset$sFlow_total_packets
# Signal_data_occupation <- summarized_dataset$Signal_mean_pkt_size * summarized_dataset$Signal_total_packets
# Sina_Weibo_data_occupation <- summarized_dataset$`Sina(Weibo)_mean_pkt_size` * summarized_dataset$`Sina(Weibo)_total_packets`
# SIP_data_occupation <- summarized_dataset$SIP_mean_pkt_size * summarized_dataset$SIP_total_packets                  
# Skype_data_occupation <- summarized_dataset$Skype_mean_pkt_size * summarized_dataset$Skype_total_packets
# SkypeCall_data_occupation <- summarized_dataset$SkypeCall_mean_pkt_size * summarized_dataset$Skype_total_packets
# Slack_data_occupation <- summarized_dataset$Slack_mean_pkt_size * summarized_dataset$Slack_total_packets
# SMBv1_data_occupation <- summarized_dataset$SMBv1_mean_pkt_size * summarized_dataset$SMBv1_total_packets
# SMBv23_data_occupation <- summarized_dataset$SMBv23_mean_pkt_size * summarized_dataset$SMBv23_total_packets
# SMTP_data_occupation <- summarized_dataset$SMTP_mean_pkt_size * summarized_dataset$SMTP_total_packets                 
# SMTPS_data_occupation <- summarized_dataset$SMTPS_mean_pkt_size * summarized_dataset$SMTPS_total_packets
# Snapchat_data_occupation <- summarized_dataset$Snapchat_mean_pkt_size * summarized_dataset$Snapchat_total_packets
# SNMP_data_occupation <- summarized_dataset$SNMP_mean_pkt_size * summarized_dataset$SNMP_total_packets
# SOCKS_data_occupation <- summarized_dataset$SOCKS_mean_pkt_size * summarized_dataset$SOCKS_total_packets
# SOMEIP_data_occupation <- summarized_dataset$SOMEIP_mean_pkt_size * summarized_dataset$SOMEIP_total_packets
# SoundCloud_data_occupation <- summarized_dataset$SoundCloud_mean_pkt_size * summarized_dataset$SoundCloud_total_packets
# Spotify_data_occupation <- summarized_dataset$Spotify_mean_pkt_size * summarized_dataset$Spotify_total_packets
# SSDP_data_occupation <- summarized_dataset$SSDP_mean_pkt_size * summarized_dataset$SSDP_total_packets
# SSH_data_occupation <- summarized_dataset$SSH_mean_pkt_size * summarized_dataset$SSH_total_packets
# Starcraft_data_occupation <- summarized_dataset$Starcraft_mean_pkt_size * summarized_dataset$Starcraft_total_packets
# Steam_data_occupation <- summarized_dataset$Steam_mean_pkt_size * summarized_dataset$Steam_total_packets
# STUN_data_occupation <- summarized_dataset$STUN_mean_pkt_size * summarized_dataset$STUN_total_packets                  
# Syslog_data_occupation <- summarized_dataset$Syslog_mean_pkt_size * summarized_dataset$Syslog_total_packets       
# Targus_Dataspeed_data_occupation <- summarized_dataset$`Targus Dataspeed_mean_pkt_size` * summarized_dataset$`Targus Dataspeed_total_packets`
# TeamViewer_data_occupation <- summarized_dataset$TeamViewer_mean_pkt_size * summarized_dataset$TeamViewer_total_packets
# Telegram_data_occupation <- summarized_dataset$Telegram_mean_pkt_size * summarized_dataset$Telegram_total_packets             
# Teredo_data_occupation <- summarized_dataset$Teredo_mean_pkt_size * summarized_dataset$Teredo_total_packets               
# TikTok_data_occupation <- summarized_dataset$TikTok_mean_pkt_size * summarized_dataset$TikTok_total_packets               
# TLS_data_occupation <- summarized_dataset$TLS_mean_pkt_size * summarized_dataset$TLS_total_packets                  
# Tor_data_occupation <- summarized_dataset$Tor_mean_pkt_size * summarized_dataset$Tor_total_packets                 
# Tuenti_data_occupation <- summarized_dataset$Tuenti_mean_pkt_size * summarized_dataset$Tuenti_total_packets               
# Twitch_data_occupation <- summarized_dataset$Twitch_mean_pkt_size * summarized_dataset$Twitch_total_packets              
# Twitter_data_occupation <- summarized_dataset$Twitter_mean_pkt_size * summarized_dataset$Twitter_total_packets              
# UBNTAC2_data_occupation <- summarized_dataset$UBNTAC2_mean_pkt_size * summarized_dataset$UBNTAC2_total_packets              
# UbuntuONE_data_occupation <- summarized_dataset$UbuntuONE_mean_pkt_size * summarized_dataset$UbuntuONE_total_packets     
# Unencrypted_Jabber_data_occupation <- summarized_dataset$Unencrypted_Jabber_mean_pkt_size * summarized_dataset$Unencrypted_Jabber_total_packets   
# Unknown_data_occupation <- summarized_dataset$Unknown_mean_pkt_size * summarized_dataset$Unknown_total_packets              
# UPnP_data_occupation <- summarized_dataset$UPnP_mean_pkt_size * summarized_dataset$UPnP_total_packets                 
# Viber_data_occupation <- summarized_dataset$Viber_mean_pkt_size * summarized_dataset$Viber_total_packets                
# VNC_data_occupation <- summarized_dataset$VNC_mean_pkt_size * summarized_dataset$VNC_total_packets                 
# Waze_data_occupation <- summarized_dataset$Waze_mean_pkt_size * summarized_dataset$Waze_total_packets                 
# Webex_data_occupation <- summarized_dataset$Webex_mean_pkt_size * summarized_dataset$Webex_total_packets                
# WeChat_data_occupation <- summarized_dataset$WeChat_mean_pkt_size * summarized_dataset$WeChat_total_packets               
# WhatsApp_data_occupation <- summarized_dataset$WhatsApp_mean_pkt_size * summarized_dataset$WhatsApp_total_packets           
# WhatsAppCall_data_occupation <- summarized_dataset$WhatsAppCall_mean_pkt_size * summarized_dataset$WhatsAppCall_total_packets         
# WhatsAppFiles_data_occupation <- summarized_dataset$WhatsAppFiles_mean_pkt_size * summarized_dataset$WhatsAppFiles_total_packets
# Whois-DAS_data_occupation <- summarized_dataset$`Whois-DAS_mean_pkt_size` * summarized_dataset$`Whois-DAS_total_packets`            
# Wikipedia_data_occupation <- summarized_dataset$Wikipedia_mean_pkt_size * summarized_dataset$Wikipedia_total_packets         
# WindowsUpdate_data_occupation <- summarized_dataset$WindowsUpdate_mean_pkt_size * summarized_dataset$WindowsUpdate_total_packets
# Xbox_data_occupation <- summarized_dataset$Xbox_mean_pkt_size * summarized_dataset$Xbox_total_packets                 
# Yahoo_data_occupation <- summarized_dataset$Yahoo_mean_pkt_size * summarized_dataset$Yahoo_total_packets               
# YouTube_data_occupation <- summarized_dataset$YouTube_mean_pkt_size * summarized_dataset$YouTube_total_packets 
# Zoom_data_occupation <- summarized_dataset$Zoom_mean_pkt_size * summarized_dataset$Zoom_total_packets

# Join all the columns
final_dataset <- cbind(summarized_dataset[,c(1:2)])
final_dataset <- as.data.frame(final_dataset)
#final_dataset[, c("src_ip")] <- numeric_to_ip(final_dataset[, c("src_ip_numeric")])


# Joining Time Occupation columns
final_dataset <- cbind(final_dataset, `104_time_occupation`)
final_dataset <- cbind(final_dataset, AJP_time_occupation)
final_dataset <- cbind(final_dataset, Amazon_time_occupation)
final_dataset <- cbind(final_dataset, AmazonVideo_time_occupation)
final_dataset <- cbind(final_dataset, Apple_time_occupation )
final_dataset <- cbind(final_dataset, AppleiCloud_time_occupation)
final_dataset <- cbind(final_dataset, AppleiTunes_time_occupation)
final_dataset <- cbind(final_dataset, ApplePush_time_occupation)
final_dataset <- cbind(final_dataset, AppleStore_time_occupation)
final_dataset <- cbind(final_dataset, BGP_time_occupation)
final_dataset <- cbind(final_dataset, BitTorrent_time_occupation)
final_dataset <- cbind(final_dataset, BJNP_time_occupation)
final_dataset <- cbind(final_dataset, CiscoSkinny_time_occupation)
final_dataset <- cbind(final_dataset, CiscoVPN_time_occupation)
final_dataset <- cbind(final_dataset, Citrix_time_occupation)
final_dataset <- cbind(final_dataset, Cloudflare_time_occupation)
final_dataset <- cbind(final_dataset, CNN_time_occupation)
final_dataset <- cbind(final_dataset, DataSaver_time_occupation)
final_dataset <- cbind(final_dataset, Deezer_time_occupation)
final_dataset <- cbind(final_dataset, DHCP_time_occupation)
final_dataset <- cbind(final_dataset, Direct_Download_Link_time_occupation)
final_dataset <- cbind(final_dataset, DNP3_time_occupation)
final_dataset <- cbind(final_dataset, DNS_time_occupation)
final_dataset <- cbind(final_dataset, DNSoverHTTPS_time_occupation)
final_dataset <- cbind(final_dataset, Dropbox_time_occupation)
final_dataset <- cbind(final_dataset, eBay_time_occupation)
final_dataset <- cbind(final_dataset, eDonkey_time_occupation)
final_dataset <- cbind(final_dataset, Facebook_time_occupation)
final_dataset <- cbind(final_dataset, FTP_CONTROL_time_occupation)
final_dataset <- cbind(final_dataset, FTP_DATA_time_occupation)
final_dataset <- cbind(final_dataset, Github_time_occupation)
final_dataset <- cbind(final_dataset, GMail_time_occupation)
final_dataset <- cbind(final_dataset, Google_time_occupation)
final_dataset <- cbind(final_dataset, GoogleDocs_time_occupation)
final_dataset <- cbind(final_dataset, GoogleDrive_time_occupation)
final_dataset <- cbind(final_dataset, GoogleHangoutDuo_time_occupation)
final_dataset <- cbind(final_dataset, GoogleMaps_time_occupation)
final_dataset <- cbind(final_dataset, GooglePlus_time_occupation)
final_dataset <- cbind(final_dataset, GoogleServices_time_occupation)
final_dataset <- cbind(final_dataset, GTP_time_occupation)
final_dataset <- cbind(final_dataset, H323_time_occupation)
final_dataset <- cbind(final_dataset, HotspotShield_time_occupation)
final_dataset <- cbind(final_dataset, HTTP_time_occupation)
final_dataset <- cbind(final_dataset, HTTP_Proxy_time_occupation)
final_dataset <- cbind(final_dataset, IAX_time_occupation)
final_dataset <- cbind(final_dataset, ICMP_time_occupation)
final_dataset <- cbind(final_dataset, IMAPS_time_occupation)
final_dataset <- cbind(final_dataset, IMO_time_occupation)
final_dataset <- cbind(final_dataset, Instagram_time_occupation)
final_dataset <- cbind(final_dataset, IPsec_time_occupation)
final_dataset <- cbind(final_dataset, IRC_time_occupation)
final_dataset <- cbind(final_dataset, LDAP_time_occupation)
final_dataset <- cbind(final_dataset, LinkedIn_time_occupation)
final_dataset <- cbind(final_dataset, LotusNotes_time_occupation)
final_dataset <- cbind(final_dataset, MDNS_time_occupation)
final_dataset <- cbind(final_dataset, Messenger_time_occupation)
final_dataset <- cbind(final_dataset, Microsoft_time_occupation)
final_dataset <- cbind(final_dataset, Mining_time_occupation)
final_dataset <- cbind(final_dataset, MQTT_time_occupation)
final_dataset <- cbind(final_dataset, MS_OneDrive_time_occupation)
final_dataset <- cbind(final_dataset, MSN_time_occupation)
final_dataset <- cbind(final_dataset, `MsSQL-TDS_time_occupation`)
final_dataset <- cbind(final_dataset, MySQL_time_occupation)
final_dataset <- cbind(final_dataset, NestLogSink_time_occupation)
final_dataset <- cbind(final_dataset, NetBIOS_time_occupation)
final_dataset <- cbind(final_dataset, NetFlix_time_occupation)
final_dataset <- cbind(final_dataset, NFS_time_occupation)
final_dataset <- cbind(final_dataset, NTP_time_occupation)
final_dataset <- cbind(final_dataset, Office365_time_occupation)
final_dataset <- cbind(final_dataset, Ookla_time_occupation)
final_dataset <- cbind(final_dataset, OpenDNS_time_occupation)
final_dataset <- cbind(final_dataset, OpenVPN_time_occupation)
final_dataset <- cbind(final_dataset, Oracle_time_occupation)
final_dataset <- cbind(final_dataset, Pando_Media_Booster_time_occupation)
final_dataset <- cbind(final_dataset, Playstation_time_occupation)
final_dataset <- cbind(final_dataset, PlayStore_time_occupation)
final_dataset <- cbind(final_dataset, POP3_time_occupation)
final_dataset <- cbind(final_dataset, PostgreSQL_time_occupation)
final_dataset <- cbind(final_dataset, PS_VUE_time_occupation)
final_dataset <- cbind(final_dataset, QQ_time_occupation)
final_dataset <- cbind(final_dataset, QUIC_time_occupation)
final_dataset <- cbind(final_dataset, Radius_time_occupation)
final_dataset <- cbind(final_dataset, RDP_time_occupation)
final_dataset <- cbind(final_dataset, RTMP_time_occupation)
final_dataset <- cbind(final_dataset, RTP_time_occupation)
final_dataset <- cbind(final_dataset, RTSP_time_occupation)
final_dataset <- cbind(final_dataset, RX_time_occupation)
final_dataset <- cbind(final_dataset, SAP_time_occupation)
final_dataset <- cbind(final_dataset, sFlow_time_occupation)
final_dataset <- cbind(final_dataset, Signal_time_occupation)
final_dataset <- cbind(final_dataset, Sina_Weibo_time_occupation)
final_dataset <- cbind(final_dataset, SIP_time_occupation)
final_dataset <- cbind(final_dataset, Skype_time_occupation)
final_dataset <- cbind(final_dataset, SkypeCall_time_occupation)
final_dataset <- cbind(final_dataset, Slack_time_occupation)
final_dataset <- cbind(final_dataset, SMBv1_time_occupation)
final_dataset <- cbind(final_dataset, SMBv23_time_occupation)
final_dataset <- cbind(final_dataset, SMTP_time_occupation)
final_dataset <- cbind(final_dataset, SMTPS_time_occupation)
final_dataset <- cbind(final_dataset, Snapchat_time_occupation)
final_dataset <- cbind(final_dataset, SNMP_time_occupation)
final_dataset <- cbind(final_dataset, SOCKS_time_occupation)
final_dataset <- cbind(final_dataset, SOMEIP_time_occupation)
final_dataset <- cbind(final_dataset, SoundCloud_time_occupation)
final_dataset <- cbind(final_dataset, Spotify_time_occupation)
final_dataset <- cbind(final_dataset, SSDP_time_occupation)
final_dataset <- cbind(final_dataset, SSH_time_occupation)
final_dataset <- cbind(final_dataset, Starcraft_time_occupation)
final_dataset <- cbind(final_dataset, Steam_time_occupation)
final_dataset <- cbind(final_dataset, STUN_time_occupation)
final_dataset <- cbind(final_dataset, Syslog_time_occupation)
final_dataset <- cbind(final_dataset, Targus_Dataspeed_time_occupation)
final_dataset <- cbind(final_dataset, TeamViewer_time_occupation)
final_dataset <- cbind(final_dataset, Telegram_time_occupation)
final_dataset <- cbind(final_dataset, Teredo_time_occupation)
final_dataset <- cbind(final_dataset, TikTok_time_occupation)
final_dataset <- cbind(final_dataset, TLS_time_occupation)
final_dataset <- cbind(final_dataset, Tor_time_occupation)
final_dataset <- cbind(final_dataset, Tuenti_time_occupation)
final_dataset <- cbind(final_dataset, Twitch_time_occupation)
final_dataset <- cbind(final_dataset, Twitter_time_occupation)
final_dataset <- cbind(final_dataset, UBNTAC2_time_occupation)
final_dataset <- cbind(final_dataset, UbuntuONE_time_occupation)
final_dataset <- cbind(final_dataset, Unencrypted_Jabber_time_occupation)
final_dataset <- cbind(final_dataset, Unknown_time_occupation)
final_dataset <- cbind(final_dataset, UPnP_time_occupation)
final_dataset <- cbind(final_dataset, Viber_time_occupation)
final_dataset <- cbind(final_dataset, VNC_time_occupation)
final_dataset <- cbind(final_dataset, Waze_time_occupation)
final_dataset <- cbind(final_dataset, Webex_time_occupation)
final_dataset <- cbind(final_dataset, WeChat_time_occupation)
final_dataset <- cbind(final_dataset, WhatsApp_time_occupation)
final_dataset <- cbind(final_dataset, WhatsAppCall_time_occupation)
final_dataset <- cbind(final_dataset, WhatsAppFiles_time_occupation)
final_dataset <- cbind(final_dataset, `Whois-DAS_time_occupation`)
final_dataset <- cbind(final_dataset, Wikipedia_time_occupation)
final_dataset <- cbind(final_dataset, WindowsUpdate_time_occupation)
final_dataset <- cbind(final_dataset, Xbox_time_occupation)
final_dataset <- cbind(final_dataset, Yahoo_time_occupation)
final_dataset <- cbind(final_dataset, YouTube_time_occupation)
final_dataset <- cbind(final_dataset, Zoom_time_occupation)

# Joining Data Occupation columns
final_dataset <- cbind(final_dataset, summarized_dataset[,c(144:1694)])

# final_dataset <- cbind(final_dataset, AJP_data_occupation)
# final_dataset <- cbind(final_dataset, Amazon_data_occupation)
# final_dataset <- cbind(final_dataset, AmazonVideo_data_occupation)
# final_dataset <- cbind(final_dataset, Apple_data_occupation)
# final_dataset <- cbind(final_dataset, AppleiCloud_data_occupation)
# final_dataset <- cbind(final_dataset, AppleiTunes_data_occupation)
# final_dataset <- cbind(final_dataset, ApplePush_data_occupation)
# final_dataset <- cbind(final_dataset, AppleStore_data_occupation)
# final_dataset <- cbind(final_dataset, BGP_data_occupation)
# final_dataset <- cbind(final_dataset, BitTorrent_data_occupation)
# final_dataset <- cbind(final_dataset, BJNP_data_occupation)
# final_dataset <- cbind(final_dataset, CiscoSkinny_data_occupation)
# final_dataset <- cbind(final_dataset, CiscoVPN_data_occupation)
# final_dataset <- cbind(final_dataset, Citrix_data_occupation)
# final_dataset <- cbind(final_dataset, Cloudflare_data_occupation)
# final_dataset <- cbind(final_dataset, CNN_data_occupation)
# final_dataset <- cbind(final_dataset, DataSaver_data_occupation)
# final_dataset <- cbind(final_dataset, Deezer_data_occupation)
# final_dataset <- cbind(final_dataset, DHCP_data_occupation)
# final_dataset <- cbind(final_dataset, Direct_Download_Link_data_occupation)
# final_dataset <- cbind(final_dataset, DNP3_data_occupation)
# final_dataset <- cbind(final_dataset, DNS_data_occupation)
# final_dataset <- cbind(final_dataset, DNSoverHTTPS_data_occupation)
# final_dataset <- cbind(final_dataset, Dropbox_data_occupation)
# final_dataset <- cbind(final_dataset, eBay_data_occupation)
# final_dataset <- cbind(final_dataset, eDonkey_data_occupation)
# final_dataset <- cbind(final_dataset, Facebook_data_occupation)
# final_dataset <- cbind(final_dataset, FTP_CONTROL_data_occupation)
# final_dataset <- cbind(final_dataset, FTP_DATA_data_occupation)
# final_dataset <- cbind(final_dataset, Github_data_occupation)
# final_dataset <- cbind(final_dataset, GMail_data_occupation)
# final_dataset <- cbind(final_dataset, Google_data_occupation)
# final_dataset <- cbind(final_dataset, GoogleDocs_data_occupation)
# final_dataset <- cbind(final_dataset, GoogleDrive_data_occupation)
# final_dataset <- cbind(final_dataset, GoogleHangoutDuo_data_occupation)
# final_dataset <- cbind(final_dataset, GoogleMaps_data_occupation)
# final_dataset <- cbind(final_dataset, GooglePlus_data_occupation)
# final_dataset <- cbind(final_dataset, GoogleServices_data_occupation)
# final_dataset <- cbind(final_dataset, GTP_data_occupation)
# final_dataset <- cbind(final_dataset, H323_data_occupation)
# final_dataset <- cbind(final_dataset, HotspotShield_data_occupation)
# final_dataset <- cbind(final_dataset, HTTP_data_occupation)
# final_dataset <- cbind(final_dataset, HTTP_Proxy_data_occupation)
# final_dataset <- cbind(final_dataset, IAX_data_occupation)
# final_dataset <- cbind(final_dataset, ICMP_data_occupation)
# final_dataset <- cbind(final_dataset, IMAPS_data_occupation)
# final_dataset <- cbind(final_dataset, IMO_data_occupation)
# final_dataset <- cbind(final_dataset, Instagram_data_occupation)
# final_dataset <- cbind(final_dataset, IPsec_data_occupation)
# final_dataset <- cbind(final_dataset, IRC_data_occupation)
# final_dataset <- cbind(final_dataset, LDAP_data_occupation)
# final_dataset <- cbind(final_dataset, LinkedIn_data_occupation)
# final_dataset <- cbind(final_dataset, LotusNotes_data_occupation)
# final_dataset <- cbind(final_dataset, MDNS_data_occupation)
# final_dataset <- cbind(final_dataset, Messenger_data_occupation)
# final_dataset <- cbind(final_dataset, Microsoft_data_occupation)
# final_dataset <- cbind(final_dataset, Mining_data_occupation)
# final_dataset <- cbind(final_dataset, MQTT_data_occupation)
# final_dataset <- cbind(final_dataset, MS_OneDrive_data_occupation)
# final_dataset <- cbind(final_dataset, MSN_data_occupation)
# final_dataset <- cbind(final_dataset, MsSQL-TDS_data_occupation)
# final_dataset <- cbind(final_dataset, MySQL_data_occupation)
# final_dataset <- cbind(final_dataset, NestLogSink_data_occupation)
# final_dataset <- cbind(final_dataset, NetBIOS_data_occupation)
# final_dataset <- cbind(final_dataset, NetFlix_data_occupation)
# final_dataset <- cbind(final_dataset, NFS_data_occupation)
# final_dataset <- cbind(final_dataset, NTP_data_occupation)
# final_dataset <- cbind(final_dataset, Office365_data_occupation)
# final_dataset <- cbind(final_dataset, Ookla_data_occupation)
# final_dataset <- cbind(final_dataset, OpenDNS_data_occupation)
# final_dataset <- cbind(final_dataset, OpenVPN_data_occupation)
# final_dataset <- cbind(final_dataset, Oracle_data_occupation)
# final_dataset <- cbind(final_dataset, Pando_Media_Booster_data_occupation)
# final_dataset <- cbind(final_dataset, Playstation_data_occupation)
# final_dataset <- cbind(final_dataset, PlayStore_data_occupation)
# final_dataset <- cbind(final_dataset, POP3_data_occupation)
# final_dataset <- cbind(final_dataset, PostgreSQL_data_occupation)
# final_dataset <- cbind(final_dataset, PS_VUE_data_occupation)
# final_dataset <- cbind(final_dataset, QQ_data_occupation)
# final_dataset <- cbind(final_dataset, QUIC_data_occupation)
# final_dataset <- cbind(final_dataset, Radius_data_occupation)
# final_dataset <- cbind(final_dataset, RDP_data_occupation)
# final_dataset <- cbind(final_dataset, RTMP_data_occupation)
# final_dataset <- cbind(final_dataset, RTP_data_occupation)
# final_dataset <- cbind(final_dataset, RTSP_data_occupation)
# final_dataset <- cbind(final_dataset, RX_data_occupation)
# final_dataset <- cbind(final_dataset, SAP_data_occupation)
# final_dataset <- cbind(final_dataset, sFlow_data_occupation)
# final_dataset <- cbind(final_dataset, Signal_data_occupation)
# final_dataset <- cbind(final_dataset, Sina_Weibo_data_occupation)
# final_dataset <- cbind(final_dataset, SIP_data_occupation)
# final_dataset <- cbind(final_dataset, Skype_data_occupation)
# final_dataset <- cbind(final_dataset, SkypeCall_data_occupation)
# final_dataset <- cbind(final_dataset, Slack_data_occupation)
# final_dataset <- cbind(final_dataset, SMBv1_data_occupation)
# final_dataset <- cbind(final_dataset, SMBv23_data_occupation)
# final_dataset <- cbind(final_dataset, SMTP_data_occupation)
# final_dataset <- cbind(final_dataset, SMTPS_data_occupation)
# final_dataset <- cbind(final_dataset, Snapchat_data_occupation)
# final_dataset <- cbind(final_dataset, SNMP_data_occupation)
# final_dataset <- cbind(final_dataset, SOCKS_data_occupation)
# final_dataset <- cbind(final_dataset, SOMEIP_data_occupation)
# final_dataset <- cbind(final_dataset, SoundCloud_data_occupation)
# final_dataset <- cbind(final_dataset, Spotify_data_occupation)
# final_dataset <- cbind(final_dataset, SSDP_data_occupation)
# final_dataset <- cbind(final_dataset, SSH_data_occupation)
# final_dataset <- cbind(final_dataset, Starcraft_data_occupation)
# final_dataset <- cbind(final_dataset, Steam_data_occupation)
# final_dataset <- cbind(final_dataset, STUN_data_occupation)
# final_dataset <- cbind(final_dataset, Syslog_data_occupation)
# final_dataset <- cbind(final_dataset, Targus_Dataspeed_data_occupation)
# final_dataset <- cbind(final_dataset, TeamViewer_data_occupation)
# final_dataset <- cbind(final_dataset, Telegram_data_occupation)
# final_dataset <- cbind(final_dataset, Teredo_data_occupation)
# final_dataset <- cbind(final_dataset, TikTok_data_occupation)
# final_dataset <- cbind(final_dataset, TLS_data_occupation)
# final_dataset <- cbind(final_dataset, Tor_data_occupation)
# final_dataset <- cbind(final_dataset, Tuenti_data_occupation)
# final_dataset <- cbind(final_dataset, Twitch_data_occupation)
# final_dataset <- cbind(final_dataset, Twitter_data_occupation)
# final_dataset <- cbind(final_dataset, UBNTAC2_data_occupation)
# final_dataset <- cbind(final_dataset, UbuntuONE_data_occupation)
# final_dataset <- cbind(final_dataset, Unencrypted_Jabber_data_occupation)
# final_dataset <- cbind(final_dataset, Unknown_data_occupation)
# final_dataset <- cbind(final_dataset, UPnP_data_occupation)
# final_dataset <- cbind(final_dataset, Viber_data_occupation)
# final_dataset <- cbind(final_dataset, VNC_data_occupation)
# final_dataset <- cbind(final_dataset, Waze_data_occupation)
# final_dataset <- cbind(final_dataset, Webex_data_occupation)
# final_dataset <- cbind(final_dataset, WeChat_data_occupation)
# final_dataset <- cbind(final_dataset, WhatsApp_data_occupation)
# final_dataset <- cbind(final_dataset, WhatsAppCall_data_occupation)
# final_dataset <- cbind(final_dataset, WhatsAppFiles_data_occupation)
# final_dataset <- cbind(final_dataset, Whois-DAS_data_occupation)
# final_dataset <- cbind(final_dataset, Wikipedia_data_occupation)
# final_dataset <- cbind(final_dataset, WindowsUpdate_data_occupation)
# final_dataset <- cbind(final_dataset, Xbox_data_occupation)
# final_dataset <- cbind(final_dataset, Yahoo_data_occupation)
# final_dataset <- cbind(final_dataset, YouTube_data_occupation)
# final_dataset <- cbind(final_dataset, Zoom_data_occupation)

write.csv(final_dataset, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Unicauca_Users-Consumption-dataset-April_June_2019-AllAttributes-AllApps.csv", row.names = FALSE, quote = FALSE, sep=",")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------OBTAINING THE BEST NUMBER OF CLUSTERS------------------------------------------------------------------------------------------------------------------
rm(summarized_dataset)
test_data <- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Final Dataset - Users Consumption 2019 - OTT apps Only/Dataset with all attributes/Unicauca_Users-Consumption-dataset-April_June_2019-AllAttributes-OTTApps-WithoutNetworkIP.csv")

test_data$src_ip_numeric <- NULL
df_clusters <- scale(test_data)
rm(df_clusters)

# Elbow method . Hierarchical Clustering
fviz_nbclust(test_data, hcut, method = "wss", k.max = 20) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Hierarchical clustering - Final Dataset 2019")

# Elbow method - Kmeans
fviz_nbclust(test_data, kmeans, method = "wss", k.max = 20) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Kmeans - FInal Dataset 2019")

# Silhouette method - Kmeans
fviz_nbclust(test_data, kmeans, method = "silhouette", k.max = 20) +
  labs(subtitle = "Silhouette method - Kmeans - Final Dataset 2019")

# Silhouette method - Hierarchichal Clustering 
fviz_nbclust(test_data, hcut, method = "silhouette", k.max = 20)+
  labs(subtitle = "Silhouette method - Hierarchical Clustering - Final Dataset 2019")

# Gap statistic method - Kmeans
set.seed(123)
fviz_nbclust(test_data, kmeans, nstart = 25,  method = "gap_stat", k.max = 10, nboot = 50)+
  labs(subtitle = "Gap statistic method- Kmeans - Sample_FinalDataset - Kmax= 10 clusters")

#--------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------CLUSTERING WITH KMEANS-------------------------------------------------------------------
final_dataset$src_ip_numeric <- NULL
kmeans3 <- kmeans(final_dataset, centers = 3)
kmeans2 <- kmeans(final_dataset, centers = 2)
kmeans4 <- kmeans(final_dataset, centers = 4)
getOption("max.print")
options(max.print = 100000)
summary(final_dataset)

rm(kmeans2)
rm(kmeans3)
gc()

attributes(kmeans3)
kmeans3$size
kmeans3$centers
kmeans3$centers

library(cluster)
library(fpc)
class(final_dataset_df)
any(is.na(final_dataset_df))
any(is.infinite(final_dataset_df))

final_dataset_df <- as.data.frame(final_dataset)
plotcluster(final_dataset, kmeans3$cluster)
clusplot(final_dataset[,1:250], kmeans3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
clusplot(final_dataset[,1:250], kmeans2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#----------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------CLUSTERING WITH DBSCAN---------------------------------------------------------------------
# Compute DBSCAN using fpc package
library("fpc")
set.seed(123)
db <- fpc::dbscan(test_data[, 3:299], eps = 0.15, MinPts = 5)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())
#----------------------------------------------------------------------------------------------------------------------------------------------------------
  
