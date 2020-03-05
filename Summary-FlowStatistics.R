# This script is intended to summarize the consumption behavior of the users by creating a new dataset where each instance is a user.
# The idea is to obtain the Users' devices IP addresses in both network and decimal format and then count the number of flows per OTT application 
# and obtain the mean values of certain attributes in order to classify the users on a certain category
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# This is to clean the memory from stored data
rm(list = ls())
rm(dataset)
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
mean_octet_total_count.df <- as.data.frame(tapply(df$octetTotalCount, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_octet_total_count.df) <- 1:nrow(mean_octet_total_count.df)
# Replace NA with 0
mean_octet_total_count.df[is.na((mean_octet_total_count.df))] <- 0

head(mean_octet_total_count.df)


# Set the column names in their order - 99 applications - 22-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size", "AppleStore_mean_pkt_size",
                             "BitTorrent_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Cloudflare_mean_pkt_size", "DataSaver_mean_pkt_size", "DHCP_mean_pkt_size", "DNS_mean_pkt_size", "DNSoverHTTPS_mean_pkt_size",
                             "Dropbox_mean_pkt_size", "eBay_mean_pkt_size", "Facebook_mean_pkt_size", "Github_mean_pkt_size", "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size",
                             "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size", "H323_mean_pkt_size", "HotspotShield_mean_pkt_size",
                             "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size", "IMO_mean_pkt_size", "Instagram_mean_pkt_size", "IPsec_mean_pkt_size",
                             "IRC_mean_pkt_size", "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size",  "Mining_mean_pkt_size", "MQTT_mean_pkt_size",
                             "MS_OneDrive_mean_pkt_size", "MSN_mean_pkt_size", "MsSQL-TDS_mean_pkt_size", "NetBIOS_mean_pkt_size_mean_pkt_size", "NetFlix_mean_pkt_size", "NTP_mean_pkt_size", "Office365_mean_pkt_size",
                             "Ookla_mean_pkt_size", "OpenDNS_mean_pkt_size", "Oracle_mean_pkt_size", "Pando_Media_Booster_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size", "PS_VUE_mean_pkt_size",
                             "QQ_mean_pkt_size", "QUIC_mean_pkt_size", "RDP_mean_pkt_size", "RTMP_mean_pkt_size", "RTP_mean_pkt_size", "RX_mean_pkt_size", "Signal_mean_pkt_size",
                             "SIP_mean_pkt_size", "Skype_mean_pkt_size", "SkypeCall_mean_pkt_size", "Slack_mean_pkt_size", "SMBv23_mean_pkt_size", "SMTP_mean_pkt_size", "Snapchat_mean_pkt_size",
                             "SNMP_mean_pkt_size", "Spotify_mean_pkt_size", "SSDP_mean_pkt_size", "SSH_mean_pkt_size", "Starcraft_mean_pkt_size", "Steam_mean_pkt_size", "STUN_mean_pkt_size",
                             "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "Teredo_mean_pkt_size", "TikTok_mean_pkt_size", "TLS_mean_pkt_size", "Tor_mean_pkt_size",
                             "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unencrypted_Jabber_mean_pkt_size", "Unknown_mean_pkt_size", "VNC_mean_pkt_size", "Webex_mean_pkt_size", "WeChat_mean_pkt_size", 
                             "WhatsApp_mean_pkt_size", "WhatsAppCall_mean_pkt_size", "Whois-DAS_mean_pkt_size", "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")

# Set the column names in their order - 100 applications - 23-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("104_mean_pkt_size","AJP_mean_pkt_size", "Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size", "AppleStore_mean_pkt_size",
                             "BitTorrent_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Cloudflare_mean_pkt_size", "DataSaver_mean_pkt_size", "DHCP_mean_pkt_size", "Direct_Download_Link_mean_pkt_size","DNS_mean_pkt_size",
                             "Dropbox_mean_pkt_size", "eBay_mean_pkt_size","eDonkey_mean_pkt_size", "Facebook_mean_pkt_size", "FTP_DATA_mean_pkt_size", "Github_mean_pkt_size", "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size",
                             "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size", "H323_mean_pkt_size", "HotspotShield_mean_pkt_size",
                             "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size", "IMO_mean_pkt_size", "Instagram_mean_pkt_size", "IPsec_mean_pkt_size",
                             "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size",  "Mining_mean_pkt_size", "MQTT_mean_pkt_size",
                             "MS_OneDrive_mean_pkt_size", "MSN_mean_pkt_size", "MsSQL-TDS_mean_pkt_size", "NetBIOS_mean_pkt_size_mean_pkt_size", "NetFlix_mean_pkt_size", "NTP_mean_pkt_size", "Office365_mean_pkt_size",
                             "Ookla_mean_pkt_size", "OpenVPN_mean_pkt_size", "Oracle_mean_pkt_size", "Pando_Media_Booster_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size", "PS_VUE_mean_pkt_size",
                             "QQ_mean_pkt_size", "QUIC_mean_pkt_size", "Radius_mean_pkt_size","RDP_mean_pkt_size", "RTMP_mean_pkt_size", "RX_mean_pkt_size", "Signal_mean_pkt_size",
                             "SIP_mean_pkt_size", "Skype_mean_pkt_size", "SkypeCall_mean_pkt_size", "SMBv23_mean_pkt_size", "SMTP_mean_pkt_size", "Snapchat_mean_pkt_size",
                             "SNMP_mean_pkt_size", "SoundCloud_mean_pkt_size","Spotify_mean_pkt_size", "SSH_mean_pkt_size", "Starcraft_mean_pkt_size", "Steam_mean_pkt_size", "STUN_mean_pkt_size",
                             "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "Teredo_mean_pkt_size", "TLS_mean_pkt_size", "Twitch_mean_pkt_size",
                             "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unencrypted_Jabber_mean_pkt_size", "Unknown_mean_pkt_size", "UPnP_mean_pkt_size", "VNC_mean_pkt_size", "Webex_mean_pkt_size", "WeChat_mean_pkt_size", 
                             "WhatsApp_mean_pkt_size", "WhatsAppCall_mean_pkt_size", "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")

# Set the column names in their order - 99 applications - 26-04-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("AJP_mean_pkt_size", "Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size", "AppleStore_mean_pkt_size",        
                             "BitTorrent_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Citrix_mean_pkt_size", "Cloudflare_mean_pkt_size", "CNN_mean_pkt_size", "DataSaver_mean_pkt_size", "DHCP_mean_pkt_size", "DNP3_mean_pkt_size",              
                             "DNS_mean_pkt_size", "Dropbox_mean_pkt_size", "Facebook_mean_pkt_size", "Github_mean_pkt_size", "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size",       
                             "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size", "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size",             
                             "Instagram_mean_pkt_size", "IPsec_mean_pkt_size", "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size", "Mining_mean_pkt_size", "MQTT_mean_pkt_size",              
                             "MS_OneDrive_mean_pkt_size", "MSN_mean_pkt_size", "MsSQL-TDS_mean_pkt_size", "MySQL_mean_pkt_size", "NestLogSink_mean_pkt_size", "NetBIOS_mean_pkt_size", "NetFlix_mean_pkt_size", "NFS_mean_pkt_size",               
                             "NTP_mean_pkt_size", "Office365_mean_pkt_size", "Ookla_mean_pkt_size", "OpenDNS_mean_pkt_size", "Oracle_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size", "QQ_mean_pkt_size",                
                             "QUIC_mean_pkt_size", "Radius_mean_pkt_size", "RDP_mean_pkt_size", "RTMP_mean_pkt_size", "RTP_mean_pkt_size", "RX_mean_pkt_size", "sFlow_mean_pkt_size", "SIP_mean_pkt_size",               
                             "Skype_mean_pkt_size", "SkypeCall_mean_pkt_size", "Slack_mean_pkt_size", "SMBv23_mean_pkt_size", "SMTP_mean_pkt_size", "SMTPS_mean_pkt_size", "Snapchat_mean_pkt_size", "SNMP_mean_pkt_size",              
                             "SoundCloud_mean_pkt_size", "Spotify_mean_pkt_size", "SSH_mean_pkt_size", "Starcraft_mean_pkt_size", "STUN_mean_pkt_size", "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size",          
                             "Teredo_mean_pkt_size", "TikTok_mean_pkt_size", "TLS_mean_pkt_size", "Twitch_mean_pkt_size", "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unencrypted_Jabber_mean_pkt_size", "Unknown_mean_pkt_size",           
                             "Viber_mean_pkt_size", "VNC_mean_pkt_size", "Webex_mean_pkt_size", "WeChat_mean_pkt_size", "WhatsApp_mean_pkt_size", "WhatsAppCall_mean_pkt_size", "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size",     
                             "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")

# Set the column names in their order - 98 applications - 04-06-2020 - from 2pm until 4 pm
colnames(mean_octet_total_count.df) <- c("Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size", "AppleStore_mean_pkt_size",
                             "BitTorrent_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Citrix_mean_pkt_size", "Cloudflare_mean_pkt_size", "DataSaver_mean_pkt_size", "Deezer_mean_pkt_size", "DHCP_mean_pkt_size",
                             "DNP3_mean_pkt_size", "DNS_mean_pkt_size", "DNSoverHTTPS_mean_pkt_size", "Dropbox_mean_pkt_size", "Facebook_mean_pkt_size", "FTP_DATA_mean_pkt_size", "Github_mean_pkt_size",
                             "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GoogleServices_mean_pkt_size",
                             "GTP_mean_pkt_size", "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "IAX_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size", "IMO_mean_pkt_size",                
                             "Instagram_mean_pkt_size","IPsec_mean_pkt_size", "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "LotusNotes_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size",          
                             "MQTT_mean_pkt_size", "MS_OneDrive_mean_pkt_size", "MSN_mean_pkt_size", "MsSQL-TDS_mean_pkt_size", "NetBIOS_mean_pkt_size", "NetFlix_mean_pkt_size", "NTP_mean_pkt_size",                
                             "Office365_mean_pkt_size", "Ookla_mean_pkt_size", "OpenDNS_mean_pkt_size", "Oracle_mean_pkt_size", "Pando_Media_Booster_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size",          
                             "QQ_mean_pkt_size", "QUIC_mean_pkt_size", "RDP_mean_pkt_size", "RTMP_mean_pkt_size", "RTP_mean_pkt_size", "RTSP_mean_pkt_size", "RX_mean_pkt_size",      
                             "SAP_mean_pkt_size", "sFlow_mean_pkt_size", "SIP_mean_pkt_size", "Skype_mean_pkt_size", "SkypeCall_mean_pkt_size", "SMBv1_mean_pkt_size", "SMBv23_mean_pkt_size",             
                             "SMTP_mean_pkt_size", "Snapchat_mean_pkt_size", "SNMP_mean_pkt_size", "SOMEIP_mean_pkt_size", "Spotify_mean_pkt_size", "SSDP_mean_pkt_size", "SSH_mean_pkt_size",                
                             "Steam_mean_pkt_size", "STUN_mean_pkt_size", "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "Teredo_mean_pkt_size", "TLS_mean_pkt_size",                
                             "Twitch_mean_pkt_size", "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unencrypted_Jabber_mean_pkt_size", "Unknown_mean_pkt_size", "VNC_mean_pkt_size", "WhatsApp_mean_pkt_size",           
                             "WhatsAppCall_mean_pkt_size", "Whois-DAS_mean_pkt_size", "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")

# Set the column names in their order - 141 applications - Complete dataset
colnames(mean_octet_total_count.df) <- c("104_mean_pkt_size",                  "AJP_mean_pkt_size",                  "Amazon_mean_pkt_size",               "AmazonVideo_mean_pkt_size",          "Apple_mean_pkt_size",                "AppleiCloud_mean_pkt_size",          "AppleiTunes_mean_pkt_size",          "ApplePush_mean_pkt_size",           
                                     "AppleStore_mean_pkt_size",           "BGP_mean_pkt_size",                  "BitTorrent_mean_pkt_size",           "BJNP_mean_pkt_size",                 "CiscoSkinny_mean_pkt_size",          "CiscoVPN_mean_pkt_size",             "Citrix_mean_pkt_size",               "Cloudflare_mean_pkt_size",          
                                     "CNN_mean_pkt_size",                  "DataSaver_mean_pkt_size",            "Deezer_mean_pkt_size",               "DHCP_mean_pkt_size",                 "Direct_Download_Link_mean_pkt_size", "DNP3_mean_pkt_size",                 "DNS_mean_pkt_size",                  "DNSoverHTTPS_mean_pkt_size",        
                                     "Dropbox_mean_pkt_size",              "eBay_mean_pkt_size",                 "eDonkey_mean_pkt_size",              "Facebook_mean_pkt_size",             "FTP_CONTROL_mean_pkt_size",          "FTP_DATA_mean_pkt_size",             "Github_mean_pkt_size",               "GMail_mean_pkt_size",               
                                     "Google_mean_pkt_size",               "GoogleDocs_mean_pkt_size",           "GoogleDrive_mean_pkt_size",          "GoogleHangoutDuo_mean_pkt_size",     "GoogleMaps_mean_pkt_size",           "GooglePlus_mean_pkt_size",           "GoogleServices_mean_pkt_size",       "GTP_mean_pkt_size",                 
                                     "H323_mean_pkt_size",                 "HotspotShield_mean_pkt_size",        "HTTP_mean_pkt_size",                 "HTTP_Proxy_mean_pkt_size",           "IAX_mean_pkt_size",                  "ICMP_mean_pkt_size",                 "IMAPS_mean_pkt_size",                "IMO_mean_pkt_size",                 
                                     "Instagram_mean_pkt_size",            "IPsec_mean_pkt_size",                "IRC_mean_pkt_size",                  "LDAP_mean_pkt_size",                 "LinkedIn_mean_pkt_size",             "LotusNotes_mean_pkt_size",           "MDNS_mean_pkt_size",                 "Messenger_mean_pkt_size",           
                                     "Microsoft_mean_pkt_size",            "Mining_mean_pkt_size",               "MQTT_mean_pkt_size",                 "MS_OneDrive_mean_pkt_size",          "MSN_mean_pkt_size",                  "MsSQL-TDS_mean_pkt_size",            "MySQL_mean_pkt_size",                "NestLogSink_mean_pkt_size",         
                                     "NetBIOS_mean_pkt_size",              "NetFlix_mean_pkt_size",              "NFS_mean_pkt_size",                  "NTP_mean_pkt_size",                  "Office365_mean_pkt_size",            "Ookla_mean_pkt_size",                "OpenDNS_mean_pkt_size",              "OpenVPN_mean_pkt_size",             
                                     "Oracle_mean_pkt_size",               "Pando_Media_Booster_mean_pkt_size",  "Playstation_mean_pkt_size",          "PlayStore_mean_pkt_size",            "POP3_mean_pkt_size",                 "PostgreSQL_mean_pkt_size",           "PS_VUE_mean_pkt_size",               "QQ_mean_pkt_size",                  
                                     "QUIC_mean_pkt_size",                 "Radius_mean_pkt_size",               "RDP_mean_pkt_size",                  "RTMP_mean_pkt_size",                 "RTP_mean_pkt_size",                  "RTSP_mean_pkt_size",                 "RX_mean_pkt_size",                   "SAP_mean_pkt_size",                 
                                     "sFlow_mean_pkt_size",                "Signal_mean_pkt_size",               "Sina(Weibo)_mean_pkt_size",          "SIP_mean_pkt_size",                  "Skype_mean_pkt_size",                "SkypeCall_mean_pkt_size",            "Slack_mean_pkt_size",                "SMBv1_mean_pkt_size",               
                                     "SMBv23_mean_pkt_size",               "SMTP_mean_pkt_size",                 "SMTPS_mean_pkt_size",                "Snapchat_mean_pkt_size",             "SNMP_mean_pkt_size",                 "SOCKS_mean_pkt_size",                "SOMEIP_mean_pkt_size",               "SoundCloud_mean_pkt_size",          
                                     "Spotify_mean_pkt_size",              "SSDP_mean_pkt_size",                 "SSH_mean_pkt_size",                  "Starcraft_mean_pkt_size",            "Steam_mean_pkt_size",                "STUN_mean_pkt_size",                 "Syslog_mean_pkt_size",               "Targus Dataspeed_mean_pkt_size",    
                                     "TeamViewer_mean_pkt_size",           "Telegram_mean_pkt_size",             "Teredo_mean_pkt_size",               "TikTok_mean_pkt_size",               "TLS_mean_pkt_size",                  "Tor_mean_pkt_size",                  "Tuenti_mean_pkt_size",               "Twitch_mean_pkt_size",              
                                     "Twitter_mean_pkt_size",              "UBNTAC2_mean_pkt_size",              "UbuntuONE_mean_pkt_size",            "Unencrypted_Jabber_mean_pkt_size",   "Unknown_mean_pkt_size",              "UPnP_mean_pkt_size",                 "Viber_mean_pkt_size",                "VNC_mean_pkt_size",                 
                                     "Waze_mean_pkt_size",                 "Webex_mean_pkt_size",                "WeChat_mean_pkt_size",               "WhatsApp_mean_pkt_size",             "WhatsAppCall_mean_pkt_size",         "WhatsAppFiles_mean_pkt_size",        "Whois-DAS_mean_pkt_size",            "Wikipedia_mean_pkt_size",           
                                     "WindowsUpdate_mean_pkt_size",        "Xbox_mean_pkt_size",                 "Yahoo_mean_pkt_size",                "YouTube_mean_pkt_size",              "Zoom_mean_pkt_size")

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

#-----------------------------------------------------OBTAIN THE TOTAL NUMBER OF PACKETS SENT IN BOTH DIRECTIONS PER APPLICATION--------------------------------------------------------------------------
# Obtain a summary of the total number of packets sent in both directions per application label for each src ip
total_packets_per_app <- as.data.frame(tapply(df$pktTotalCount, list(df$src_ip, df$web_service), sum))
# Set the row indexes as numbers
rownames(total_packets_per_app) <- 1:nrow(total_packets_per_app)
# Replace NA with 0
total_packets_per_app[is.na((total_packets_per_app))] <- 0

# Set the column names in their order - 141 applications - Complete dataset
colnames(total_packets_per_app) <- c("104_total_packets",                  "AJP_total_packets",                  "Amazon_total_packets",               "AmazonVideo_total_packets",          "Apple_total_packets",                "AppleiCloud_total_packets",          "AppleiTunes_total_packets",          "ApplePush_total_packets",           
                                     "AppleStore_total_packets",           "BGP_total_packets",                  "BitTorrent_total_packets",           "BJNP_total_packets",                 "CiscoSkinny_total_packets",          "CiscoVPN_total_packets",             "Citrix_total_packets",               "Cloudflare_total_packets",          
                                     "CNN_total_packets",                  "DataSaver_total_packets",            "Deezer_total_packets",               "DHCP_total_packets",                 "Direct_Download_Link_total_packets", "DNP3_total_packets",                 "DNS_total_packets",                  "DNSoverHTTPS_total_packets",        
                                     "Dropbox_total_packets",              "eBay_total_packets",                 "eDonkey_total_packets",              "Facebook_total_packets",             "FTP_CONTROL_total_packets",          "FTP_DATA_total_packets",             "Github_total_packets",               "GMail_total_packets",               
                                     "Google_total_packets",               "GoogleDocs_total_packets",           "GoogleDrive_total_packets",          "GoogleHangoutDuo_total_packets",     "GoogleMaps_total_packets",           "GooglePlus_total_packets",           "GoogleServices_total_packets",       "GTP_total_packets",                 
                                     "H323_total_packets",                 "HotspotShield_total_packets",        "HTTP_total_packets",                 "HTTP_Proxy_total_packets",           "IAX_total_packets",                  "ICMP_total_packets",                 "IMAPS_total_packets",                "IMO_total_packets",                 
                                     "Instagram_total_packets",            "IPsec_total_packets",                "IRC_total_packets",                  "LDAP_total_packets",                 "LinkedIn_total_packets",             "LotusNotes_total_packets",           "MDNS_total_packets",                 "Messenger_total_packets",           
                                     "Microsoft_total_packets",            "Mining_total_packets",               "MQTT_total_packets",                 "MS_OneDrive_total_packets",          "MSN_total_packets",                  "MsSQL-TDS_total_packets",            "MySQL_total_packets",                "NestLogSink_total_packets",         
                                     "NetBIOS_total_packets",              "NetFlix_total_packets",              "NFS_total_packets",                  "NTP_total_packets",                  "Office365_total_packets",            "Ookla_total_packets",                "OpenDNS_total_packets",              "OpenVPN_total_packets",             
                                     "Oracle_total_packets",               "Pando_Media_Booster_total_packets",  "Playstation_total_packets",          "PlayStore_total_packets",            "POP3_total_packets",                 "PostgreSQL_total_packets",           "PS_VUE_total_packets",               "QQ_total_packets",                  
                                     "QUIC_total_packets",                 "Radius_total_packets",               "RDP_total_packets",                  "RTMP_total_packets",                 "RTP_total_packets",                  "RTSP_total_packets",                 "RX_total_packets",                   "SAP_total_packets",                 
                                     "sFlow_total_packets",                "Signal_total_packets",               "Sina(Weibo)_total_packets",          "SIP_total_packets",                  "Skype_total_packets",                "SkypeCall_total_packets",            "Slack_total_packets",                "SMBv1_total_packets",               
                                     "SMBv23_total_packets",               "SMTP_total_packets",                 "SMTPS_total_packets",                "Snapchat_total_packets",             "SNMP_total_packets",                 "SOCKS_total_packets",                "SOMEIP_total_packets",               "SoundCloud_total_packets",          
                                     "Spotify_total_packets",              "SSDP_total_packets",                 "SSH_total_packets",                  "Starcraft_total_packets",            "Steam_total_packets",                "STUN_total_packets",                 "Syslog_total_packets",               "Targus Dataspeed_total_packets",    
                                     "TeamViewer_total_packets",           "Telegram_total_packets",             "Teredo_total_packets",               "TikTok_total_packets",               "TLS_total_packets",                  "Tor_total_packets",                  "Tuenti_total_packets",               "Twitch_total_packets",              
                                     "Twitter_total_packets",              "UBNTAC2_total_packets",              "UbuntuONE_total_packets",            "Unencrypted_Jabber_total_packets",   "Unknown_total_packets",              "UPnP_total_packets",                 "Viber_total_packets",                "VNC_total_packets",                 
                                     "Waze_total_packets",                 "Webex_total_packets",                "WeChat_total_packets",               "WhatsApp_total_packets",             "WhatsAppCall_total_packets",         "WhatsAppFiles_total_packets",        "Whois-DAS_total_packets",            "Wikipedia_total_packets",           
                                     "WindowsUpdate_total_packets",        "Xbox_total_packets",                 "Yahoo_total_packets",                "YouTube_total_packets",              "Zoom_total_packets")

head(total_packets_per_app)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------JOIN COLUMNS AND GENERATE THE SUMMARIZED DATASET (TOTAL FLOWS; MEAN PKT SIZE; MEAN FLOW DURATION; TOTAL PACKETS) ------------------------------------------------------------------------------------
summarized_dataset <- cbind(flows_per_app, mean_octet_total_count.df)
summarized_dataset <- cbind(summarized_dataset, mean_flow_duration.df)
summarized_dataset <- cbind(summarized_dataset, total_packets_per_app)

# Convert src ip adresses from network format to decimal format
summarized_dataset[, c("src_ip_numeric")] <- unique(df[, c("src_ip_numeric")])

# Order the rows in ascending order based on the value of the src ip decimal number
summarized_dataset <- summarized_dataset[order(summarized_dataset[,c("src_ip_numeric")]),]

# Reorder the columns so the decimal form of the src ip is first
summarized_dataset <- subset(summarized_dataset, select=c(566,1:565))


write.csv(summarized_dataset, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Unicauca_Users-summarized-dataset-April_June_2019.csv", row.names = FALSE, quote = FALSE, sep=",")
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

summarized_dataset <- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Summarized Dataset 2019 - all Apps - Analysis/Unicauca_Users-Summarized-dataset-April_June_2019.csv")

# Store the user identificators in two columns
src_ip_numeric <- summarized_dataset$src_ip_numeric

# Obtain the occupation of the network in terms of time (seconds)
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
MsSQL-TDS_time_occupation <- summarized_dataset$`MsSQL-TDS_flows`* summarized_dataset$`MsSQL-TDS_mean_flow_duration`          
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
Sina_Weibo_time_occupation <- summarized_dataset$`Sina(Weibo)_flows`* summarized_dataset$`Sina(Weibo)_mean_flow_duration`
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
Targus_Dataspeed_time_occupation <- summarized_dataset$`Targus Dataspeed_flows` * summarized_dataset$`Targus Dataspeed_mean_flow_duration`
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
Whois-DAS_time_occupation <- summarized_dataset$`Whois-DAS_flows`* summarized_dataset$`Whois-DAS_mean_flow_duration`            
Wikipedia_time_occupation <- summarized_dataset$Wikipedia_flows * summarized_dataset$Wikipedia_mean_flow_duration         
WindowsUpdate_time_occupation <- summarized_dataset$WindowsUpdate_flows * summarized_dataset$WindowsUpdate_mean_flow_duration
Xbox_time_occupation <- summarized_dataset$Xbox_flows * summarized_dataset$Xbox_mean_flow_duration                 
Yahoo_time_occupation <- summarized_dataset$Yahoo_flows * summarized_dataset$Yahoo_mean_flow_duration               
YouTube_time_occupation <- summarized_dataset$YouTube_flows * summarized_dataset$YouTube_mean_flow_duration 
Zoom_time_occupation <- summarized_dataset$Zoom_flows * summarized_dataset$Zoom_mean_flow_duration

# Obtain the occupation of the network in terms of quantity of transferred data (bytes)
AJP_data_occupation <- summarized_dataset$AJP_mean_pkt_size * summarized_dataset$AJP_total_packets
Amazon_data_occupation <- summarized_dataset$Amazon_mean_pkt_size * summarized_dataset$Amazon_total_packets
AmazonVideo_data_occupation <- summarized_dataset$AmazonVideo_mean_pkt_size * summarized_dataset$AmazonVideo_total_packets          
Apple_data_occupation <- summarized_dataset$Apple_mean_pkt_size * summarized_dataset$Apple_total_packets                
AppleiCloud_data_occupation <- summarized_dataset$AppleiCloud_mean_pkt_size * summarized_dataset$AppleiCloud_total_packets          
AppleiTunes_data_occupation <- summarized_dataset$AppleiTunes_mean_pkt_size * summarized_dataset$AppleiTunes_total_packets          
ApplePush_data_occupation <- summarized_dataset$ApplePush_mean_pkt_size * summarized_dataset$ApplePush_total_packets           
AppleStore_data_occupation <- summarized_dataset$AppleStore_mean_pkt_size * summarized_dataset$AppleStore_total_packets           
BGP_data_occupation <- summarized_dataset$BGP_mean_pkt_size * summarized_dataset$BGP_total_packets                  
BitTorrent_data_occupation <- summarized_dataset$BitTorrent_mean_pkt_size * summarized_dataset$BitTorrent_total_packets           
BJNP_data_occupation <- summarized_dataset$BJNP_mean_pkt_size * summarized_dataset$BJNP_total_packets                 
CiscoSkinny_data_occupation <- summarized_dataset$CiscoSkinny_mean_pkt_size * summarized_dataset$CiscoSkinny_total_packets          
CiscoVPN_data_occupation <- summarized_dataset$CiscoVPN_mean_pkt_size * summarized_dataset$CiscoVPN_total_packets             
Citrix_data_occupation <- summarized_dataset$Citrix_mean_pkt_size * summarized_dataset$Citrix_total_packets               
Cloudflare_data_occupation <- summarized_dataset$Cloudflare_mean_pkt_size * summarized_dataset$Cloudflare_total_packets          
CNN_data_occupation <- summarized_dataset$CNN_mean_pkt_size * summarized_dataset$CNN_total_packets                  
DataSaver_data_occupation <- summarized_dataset$DataSaver_mean_pkt_size * summarized_dataset$DataSaver_total_packets            
Deezer_data_occupation <- summarized_dataset$Deezer_mean_pkt_size * summarized_dataset$Deezer_total_packets             
DHCP_data_occupation <- summarized_dataset$DHCP_mean_pkt_size * summarized_dataset$DHCP_total_packets                 
Direct_Download_Link_data_occupation <- summarized_dataset$Direct_Download_Link_mean_pkt_size * summarized_dataset$Direct_Download_Link_total_packets
DNP3_data_occupation <- summarized_dataset$DNP3_mean_pkt_size * summarized_dataset$DNP3_total_packets
DNS_data_occupation <- summarized_dataset$DNS_mean_pkt_size * summarized_dataset$DNS_total_packets
DNSoverHTTPS_data_occupation <- summarized_dataset$DNSoverHTTPS_mean_pkt_size * summarized_dataset$DNSoverHTTPS_total_packets
Dropbox_data_occupation <- summarized_dataset$Dropbox_mean_pkt_size * summarized_dataset$Dropbox_total_packets
eBay_data_occupation <- summarized_dataset$eBay_mean_pkt_size * summarized_dataset$eBay_total_packets
eDonkey_data_occupation <- summarized_dataset$eDonkey_mean_pkt_size * summarized_dataset$eDonkey_total_packets
Facebook_data_occupation <- summarized_dataset$Facebook_mean_pkt_size * summarized_dataset$Facebook_total_packets            
FTP_CONTROL_total_packet <- summarized_dataset$FTP_CONTROL_mean_pkt_size * summarized_dataset$FTP_CONTROL_total_packets         
FTP_DATA_total_packet <- summarized_dataset$FTP_DATA_mean_pkt_size * summarized_dataset$FTP_DATA_total_packets            
Github_data_occupation <- summarized_dataset$Github_mean_pkt_size * summarized_dataset$Github_total_packets            
GMail_data_occupation <- summarized_dataset$GMail_mean_pkt_size * summarized_dataset$GMail_total_packets              
Google_data_occupation <- summarized_dataset$Google_mean_pkt_size * summarized_dataset$Google_total_packets              
GoogleDocs_data_occupation <- summarized_dataset$GoogleDocs_mean_pkt_size * summarized_dataset$GoogleDocs_total_packets           
GoogleDrive_data_occupation <- summarized_dataset$GoogleDrive_mean_pkt_size * summarized_dataset$GoogleDrive_total_packets          
GoogleHangoutDuo_data_occupation <- summarized_dataset$GoogleHangoutDuo_mean_pkt_size * summarized_dataset$GoogleHangoutDuo_total_packets     
GoogleMaps_data_occupation <- summarized_dataset$GoogleMaps_mean_pkt_size * summarized_dataset$GoogleMaps_total_packets          
GooglePlus_data_occupation <- summarized_dataset$GooglePlus_mean_pkt_size * summarized_dataset$GooglePlus_total_packets          
GoogleServices_data_occupation <- summarized_dataset$GoogleServices_mean_pkt_size * summarized_dataset$GoogleServices_total_packets      
GTP_data_occupation <- summarized_dataset$GTP_mean_pkt_size * summarized_dataset$GTP_total_packets               
H323_data_occupation <- summarized_dataset$H323_mean_pkt_size * summarized_dataset$H323_total_packets              
HotspotShield_data_occupation <- summarized_dataset$HotspotShield_mean_pkt_size * summarized_dataset$HotspotShield_total_packets       
HTTP_data_occupation <- summarized_dataset$HTTP_mean_pkt_size * summarized_dataset$HTTP_total_packets                 
HTTP_Proxy_data_occupation <- summarized_dataset$HTTP_Proxy_mean_pkt_size * summarized_dataset$HTTP_Proxy_total_packets          
IAX_data_occupation <- summarized_dataset$IAX_mean_pkt_size * summarized_dataset$IAX_total_packets                  
ICMP_data_occupation <- summarized_dataset$ICMP_mean_pkt_size * summarized_dataset$ICMP_total_packets               
IMAPS_data_occupation <- summarized_dataset$IMAPS_mean_pkt_size * summarized_dataset$IMAPS_total_packets               
IMO_data_occupation <- summarized_dataset$IMO_mean_pkt_size * summarized_dataset$IMO_total_packets                 
Instagram_data_occupation <- summarized_dataset$Instagram_mean_pkt_size * summarized_dataset$Instagram_total_packets            
IPsec_data_occupation <- summarized_dataset$IPsec_mean_pkt_size * summarized_dataset$IPsec_total_packets                
IRC_data_occupation <- summarized_dataset$IRC_mean_pkt_size * summarized_dataset$IRC_total_packets                  
LDAP_data_occupation <- summarized_dataset$LDAP_mean_pkt_size * summarized_dataset$LDAP_total_packets                
LinkedIn_data_occupation <- summarized_dataset$LinkedIn_mean_pkt_size * summarized_dataset$LinkedIn_total_packets           
LotusNotes_data_occupation <- summarized_dataset$LotusNotes_mean_pkt_size * summarized_dataset$LotusNotes_total_packets           
MDNS_data_occupation <- summarized_dataset$MDNS_mean_pkt_size * summarized_dataset$MDNS_total_packets                 
Messenger_data_occupation <- summarized_dataset$Messenger_mean_pkt_size * summarized_dataset$Messenger_total_packets          
Microsoft_data_occupation <- summarized_dataset$Microsoft_mean_pkt_size * summarized_dataset$Microsoft_total_packets            
Mining_data_occupation <- summarized_dataset$Mining_mean_pkt_size * summarized_dataset$Mining_total_packets              
MQTT_data_occupation <- summarized_dataset$MQTT_mean_pkt_size * summarized_dataset$MQTT_total_packets                
MS_OneDrive_data_occupation <- summarized_dataset$MS_OneDrive_mean_pkt_size * summarized_dataset$MS_OneDrive_total_packets        
MSN_data_occupation <- summarized_dataset$MSN_mean_pkt_size * summarized_dataset$MSN_total_packets             
MsSQL-TDS_data_occupation <- summarized_dataset$`MsSQL-TDS_mean_pkt_size` * summarized_dataset$`MsSQL-TDS_total_packets`          
MySQL_data_occupation <- summarized_dataset$MySQL_mean_pkt_size * summarized_dataset$MySQL_total_packets              
NestLogSink_data_occupation <- summarized_dataset$NestLogSink_mean_pkt_size * summarized_dataset$NestLogSink_total_packets        
NetBIOS_data_occupation <- summarized_dataset$NetBIOS_mean_pkt_size * summarized_dataset$NetBIOS_total_packets             
NetFlix_data_occupation <- summarized_dataset$NetFlix_mean_pkt_size * summarized_dataset$NetFlix_total_packets            
NFS_data_occupation <- summarized_dataset$NFS_mean_pkt_size * summarized_dataset$NFS_total_packets                
NTP_data_occupation <- summarized_dataset$NTP_mean_pkt_size * summarized_dataset$NTP_total_packets
Office365_data_occupation <- summarized_dataset$Office365_mean_pkt_size * summarized_dataset$Office365_total_packets          
Ookla_data_occupation <- summarized_dataset$Ookla_mean_pkt_size * summarized_dataset$Ookla_total_packets               
OpenDNS_data_occupation <- summarized_dataset$OpenDNS_mean_pkt_size * summarized_dataset$OpenDNS_total_packets             
OpenVPN_data_occupation <- summarized_dataset$OpenVPN_mean_pkt_size * summarized_dataset$OpenVPN_total_packets             
Oracle_data_occupation <- summarized_dataset$Oracle_mean_pkt_size * summarized_dataset$Oracle_total_packets    
Pando_Media_Booster_data_occupation <- summarized_dataset$Pando_Media_Booster_mean_pkt_size * summarized_dataset$Pando_Media_Booster_total_packets 
Playstation_data_occupation <- summarized_dataset$Playstation_mean_pkt_size * summarized_dataset$Playstation_total_packets          
PlayStore_data_occupation <- summarized_dataset$PlayStore_mean_pkt_size * summarized_dataset$PlayStore_total_packets            
POP3_data_occupation <- summarized_dataset$POP3_mean_pkt_size * summarized_dataset$POP3_total_packets             
PostgreSQL_data_occupation <- summarized_dataset$PostgreSQL_mean_pkt_size * summarized_dataset$PostgreSQL_total_packets           
PS_VUE_data_occupation <- summarized_dataset$PS_VUE_mean_pkt_size * summarized_dataset$PS_VUE_total_packets               
QQ_data_occupation <- summarized_dataset$QQ_mean_pkt_size * summarized_dataset$QQ_total_packets                  
QUIC_data_occupation <- summarized_dataset$QUIC_mean_pkt_size * summarized_dataset$QUIC_total_packets                 
Radius_data_occupation <- summarized_dataset$Radius_mean_pkt_size * summarized_dataset$Radius_total_packets               
RDP_data_occupation <- summarized_dataset$RDP_mean_pkt_size * summarized_dataset$RDP_total_packets                  
RTMP_data_occupation <- summarized_dataset$RTMP_mean_pkt_size * summarized_dataset$RTMP_total_packets                 
RTP_data_occupation <- summarized_dataset$RTP_mean_pkt_size * summarized_dataset$RTP_total_packets
RTSP_data_occupation <- summarized_dataset$RTSP_mean_pkt_size * summarized_dataset$RTSP_total_packets
RX_data_occupation <- summarized_dataset$RX_mean_pkt_size * summarized_dataset$RX_total_packets                  
SAP_data_occupation <- summarized_dataset$SAP_mean_pkt_size * summarized_dataset$SAP_total_packets                 
sFlow_data_occupation <- summarized_dataset$sFlow_mean_pkt_size * summarized_dataset$sFlow_total_packets
Signal_data_occupation <- summarized_dataset$Signal_mean_pkt_size * summarized_dataset$Signal_total_packets
Sina_Weibo_data_occupation <- summarized_dataset$`Sina(Weibo)_mean_pkt_size` * summarized_dataset$`Sina(Weibo)_total_packets`
SIP_data_occupation <- summarized_dataset$SIP_mean_pkt_size * summarized_dataset$SIP_total_packets                  
Skype_data_occupation <- summarized_dataset$Skype_mean_pkt_size * summarized_dataset$Skype_total_packets
SkypeCall_data_occupation <- summarized_dataset$SkypeCall_mean_pkt_size * summarized_dataset$Skype_total_packets
Slack_data_occupation <- summarized_dataset$Slack_mean_pkt_size * summarized_dataset$Slack_total_packets
SMBv1_data_occupation <- summarized_dataset$SMBv1_mean_pkt_size * summarized_dataset$SMBv1_total_packets
SMBv23_data_occupation <- summarized_dataset$SMBv23_mean_pkt_size * summarized_dataset$SMBv23_total_packets
SMTP_data_occupation <- summarized_dataset$SMTP_mean_pkt_size * summarized_dataset$SMTP_total_packets                 
SMTPS_data_occupation <- summarized_dataset$SMTPS_mean_pkt_size * summarized_dataset$SMTPS_total_packets
Snapchat_data_occupation <- summarized_dataset$Snapchat_mean_pkt_size * summarized_dataset$Snapchat_total_packets
SNMP_data_occupation <- summarized_dataset$SNMP_mean_pkt_size * summarized_dataset$SNMP_total_packets
SOCKS_data_occupation <- summarized_dataset$SOCKS_mean_pkt_size * summarized_dataset$SOCKS_total_packets
SOMEIP_data_occupation <- summarized_dataset$SOMEIP_mean_pkt_size * summarized_dataset$SOMEIP_total_packets
SoundCloud_data_occupation <- summarized_dataset$SoundCloud_mean_pkt_size * summarized_dataset$SoundCloud_total_packets
Spotify_data_occupation <- summarized_dataset$Spotify_mean_pkt_size * summarized_dataset$Spotify_total_packets
SSDP_data_occupation <- summarized_dataset$SSDP_mean_pkt_size * summarized_dataset$SSDP_total_packets
SSH_data_occupation <- summarized_dataset$SSH_mean_pkt_size * summarized_dataset$SSH_total_packets
Starcraft_data_occupation <- summarized_dataset$Starcraft_mean_pkt_size * summarized_dataset$Starcraft_total_packets
Steam_data_occupation <- summarized_dataset$Steam_mean_pkt_size * summarized_dataset$Steam_total_packets
STUN_data_occupation <- summarized_dataset$STUN_mean_pkt_size * summarized_dataset$STUN_total_packets                  
Syslog_data_occupation <- summarized_dataset$Syslog_mean_pkt_size * summarized_dataset$Syslog_total_packets       
Targus_Dataspeed_data_occupation <- summarized_dataset$`Targus Dataspeed_mean_pkt_size` * summarized_dataset$`Targus Dataspeed_total_packets`
TeamViewer_data_occupation <- summarized_dataset$TeamViewer_mean_pkt_size * summarized_dataset$TeamViewer_total_packets
Telegram_data_occupation <- summarized_dataset$Telegram_mean_pkt_size * summarized_dataset$Telegram_total_packets             
Teredo_data_occupation <- summarized_dataset$Teredo_mean_pkt_size * summarized_dataset$Teredo_total_packets               
TikTok_data_occupation <- summarized_dataset$TikTok_mean_pkt_size * summarized_dataset$TikTok_total_packets               
TLS_data_occupation <- summarized_dataset$TLS_mean_pkt_size * summarized_dataset$TLS_total_packets                  
Tor_data_occupation <- summarized_dataset$Tor_mean_pkt_size * summarized_dataset$Tor_total_packets                 
Tuenti_data_occupation <- summarized_dataset$Tuenti_mean_pkt_size * summarized_dataset$Tuenti_total_packets               
Twitch_data_occupation <- summarized_dataset$Twitch_mean_pkt_size * summarized_dataset$Twitch_total_packets              
Twitter_data_occupation <- summarized_dataset$Twitter_mean_pkt_size * summarized_dataset$Twitter_total_packets              
UBNTAC2_data_occupation <- summarized_dataset$UBNTAC2_mean_pkt_size * summarized_dataset$UBNTAC2_total_packets              
UbuntuONE_data_occupation <- summarized_dataset$UbuntuONE_mean_pkt_size * summarized_dataset$UbuntuONE_total_packets     
Unencrypted_Jabber_data_occupation <- summarized_dataset$Unencrypted_Jabber_mean_pkt_size * summarized_dataset$Unencrypted_Jabber_total_packets   
Unknown_data_occupation <- summarized_dataset$Unknown_mean_pkt_size * summarized_dataset$Unknown_total_packets              
UPnP_data_occupation <- summarized_dataset$UPnP_mean_pkt_size * summarized_dataset$UPnP_total_packets                 
Viber_data_occupation <- summarized_dataset$Viber_mean_pkt_size * summarized_dataset$Viber_total_packets                
VNC_data_occupation <- summarized_dataset$VNC_mean_pkt_size * summarized_dataset$VNC_total_packets                 
Waze_data_occupation <- summarized_dataset$Waze_mean_pkt_size * summarized_dataset$Waze_total_packets                 
Webex_data_occupation <- summarized_dataset$Webex_mean_pkt_size * summarized_dataset$Webex_total_packets                
WeChat_data_occupation <- summarized_dataset$WeChat_mean_pkt_size * summarized_dataset$WeChat_total_packets               
WhatsApp_data_occupation <- summarized_dataset$WhatsApp_mean_pkt_size * summarized_dataset$WhatsApp_total_packets           
WhatsAppCall_data_occupation <- summarized_dataset$WhatsAppCall_mean_pkt_size * summarized_dataset$WhatsAppCall_total_packets         
WhatsAppFiles_data_occupation <- summarized_dataset$WhatsAppFiles_mean_pkt_size * summarized_dataset$WhatsAppFiles_total_packets
Whois-DAS_data_occupation <- summarized_dataset$`Whois-DAS_mean_pkt_size` * summarized_dataset$`Whois-DAS_total_packets`            
Wikipedia_data_occupation <- summarized_dataset$Wikipedia_mean_pkt_size * summarized_dataset$Wikipedia_total_packets         
WindowsUpdate_data_occupation <- summarized_dataset$WindowsUpdate_mean_pkt_size * summarized_dataset$WindowsUpdate_total_packets
Xbox_data_occupation <- summarized_dataset$Xbox_mean_pkt_size * summarized_dataset$Xbox_total_packets                 
Yahoo_data_occupation <- summarized_dataset$Yahoo_mean_pkt_size * summarized_dataset$Yahoo_total_packets               
YouTube_data_occupation <- summarized_dataset$YouTube_mean_pkt_size * summarized_dataset$YouTube_total_packets 
Zoom_data_occupation <- summarized_dataset$Zoom_mean_pkt_size * summarized_dataset$Zoom_total_packets

# Join all the columns
rm(final_dataset)
final_dataset <- cbind(src_ip_numeric)
final_dataset <- as.data.frame(final_dataset)
final_dataset[, c("src_ip")] <- numeric_to_ip(final_dataset[, c("src_ip_numeric")])


# Joining Time Occupation columns
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
final_dataset <- cbind(final_dataset, MsSQL-TDS_time_occupation)
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
final_dataset <- cbind(final_dataset, Whois-DAS_time_occupation)
final_dataset <- cbind(final_dataset, Wikipedia_time_occupation)
final_dataset <- cbind(final_dataset, WindowsUpdate_time_occupation)
final_dataset <- cbind(final_dataset, Xbox_time_occupation)
final_dataset <- cbind(final_dataset, Yahoo_time_occupation)
final_dataset <- cbind(final_dataset, YouTube_time_occupation)
final_dataset <- cbind(final_dataset, Zoom_time_occupation)

# Joining Data Occupation columns
final_dataset <- cbind(final_dataset, AJP_data_occupation)
final_dataset <- cbind(final_dataset, Amazon_data_occupation)
final_dataset <- cbind(final_dataset, AmazonVideo_data_occupation)
final_dataset <- cbind(final_dataset, Apple_data_occupation)
final_dataset <- cbind(final_dataset, AppleiCloud_data_occupation)
final_dataset <- cbind(final_dataset, AppleiTunes_data_occupation)
final_dataset <- cbind(final_dataset, ApplePush_data_occupation)
final_dataset <- cbind(final_dataset, AppleStore_data_occupation)
final_dataset <- cbind(final_dataset, BGP_data_occupation)
final_dataset <- cbind(final_dataset, BitTorrent_data_occupation)
final_dataset <- cbind(final_dataset, BJNP_data_occupation)
final_dataset <- cbind(final_dataset, CiscoSkinny_data_occupation)
final_dataset <- cbind(final_dataset, CiscoVPN_data_occupation)
final_dataset <- cbind(final_dataset, Citrix_data_occupation)
final_dataset <- cbind(final_dataset, Cloudflare_data_occupation)
final_dataset <- cbind(final_dataset, CNN_data_occupation)
final_dataset <- cbind(final_dataset, DataSaver_data_occupation)
final_dataset <- cbind(final_dataset, Deezer_data_occupation)
final_dataset <- cbind(final_dataset, DHCP_data_occupation)
final_dataset <- cbind(final_dataset, Direct_Download_Link_data_occupation)
final_dataset <- cbind(final_dataset, DNP3_data_occupation)
final_dataset <- cbind(final_dataset, DNS_data_occupation)
final_dataset <- cbind(final_dataset, DNSoverHTTPS_data_occupation)
final_dataset <- cbind(final_dataset, Dropbox_data_occupation)
final_dataset <- cbind(final_dataset, eBay_data_occupation)
final_dataset <- cbind(final_dataset, eDonkey_data_occupation)
final_dataset <- cbind(final_dataset, Facebook_data_occupation)
final_dataset <- cbind(final_dataset, FTP_CONTROL_data_occupation)
final_dataset <- cbind(final_dataset, FTP_DATA_data_occupation)
final_dataset <- cbind(final_dataset, Github_data_occupation)
final_dataset <- cbind(final_dataset, GMail_data_occupation)
final_dataset <- cbind(final_dataset, Google_data_occupation)
final_dataset <- cbind(final_dataset, GoogleDocs_data_occupation)
final_dataset <- cbind(final_dataset, GoogleDrive_data_occupation)
final_dataset <- cbind(final_dataset, GoogleHangoutDuo_data_occupation)
final_dataset <- cbind(final_dataset, GoogleMaps_data_occupation)
final_dataset <- cbind(final_dataset, GooglePlus_data_occupation)
final_dataset <- cbind(final_dataset, GoogleServices_data_occupation)
final_dataset <- cbind(final_dataset, GTP_data_occupation)
final_dataset <- cbind(final_dataset, H323_data_occupation)
final_dataset <- cbind(final_dataset, HotspotShield_data_occupation)
final_dataset <- cbind(final_dataset, HTTP_data_occupation)
final_dataset <- cbind(final_dataset, HTTP_Proxy_data_occupation)
final_dataset <- cbind(final_dataset, IAX_data_occupation)
final_dataset <- cbind(final_dataset, ICMP_data_occupation)
final_dataset <- cbind(final_dataset, IMAPS_data_occupation)
final_dataset <- cbind(final_dataset, IMO_data_occupation)
final_dataset <- cbind(final_dataset, Instagram_data_occupation)
final_dataset <- cbind(final_dataset, IPsec_data_occupation)
final_dataset <- cbind(final_dataset, IRC_data_occupation)
final_dataset <- cbind(final_dataset, LDAP_data_occupation)
final_dataset <- cbind(final_dataset, LinkedIn_data_occupation)
final_dataset <- cbind(final_dataset, LotusNotes_data_occupation)
final_dataset <- cbind(final_dataset, MDNS_data_occupation)
final_dataset <- cbind(final_dataset, Messenger_data_occupation)
final_dataset <- cbind(final_dataset, Microsoft_data_occupation)
final_dataset <- cbind(final_dataset, Mining_data_occupation)
final_dataset <- cbind(final_dataset, MQTT_data_occupation)
final_dataset <- cbind(final_dataset, MS_OneDrive_data_occupation)
final_dataset <- cbind(final_dataset, MSN_data_occupation)
final_dataset <- cbind(final_dataset, MsSQL-TDS_data_occupation)
final_dataset <- cbind(final_dataset, MySQL_data_occupation)
final_dataset <- cbind(final_dataset, NestLogSink_data_occupation)
final_dataset <- cbind(final_dataset, NetBIOS_data_occupation)
final_dataset <- cbind(final_dataset, NetFlix_data_occupation)
final_dataset <- cbind(final_dataset, NFS_data_occupation)
final_dataset <- cbind(final_dataset, NTP_data_occupation)
final_dataset <- cbind(final_dataset, Office365_data_occupation)
final_dataset <- cbind(final_dataset, Ookla_data_occupation)
final_dataset <- cbind(final_dataset, OpenDNS_data_occupation)
final_dataset <- cbind(final_dataset, OpenVPN_data_occupation)
final_dataset <- cbind(final_dataset, Oracle_data_occupation)
final_dataset <- cbind(final_dataset, Pando_Media_Booster_data_occupation)
final_dataset <- cbind(final_dataset, Playstation_data_occupation)
final_dataset <- cbind(final_dataset, PlayStore_data_occupation)
final_dataset <- cbind(final_dataset, POP3_data_occupation)
final_dataset <- cbind(final_dataset, PostgreSQL_data_occupation)
final_dataset <- cbind(final_dataset, PS_VUE_data_occupation)
final_dataset <- cbind(final_dataset, QQ_data_occupation)
final_dataset <- cbind(final_dataset, QUIC_data_occupation)
final_dataset <- cbind(final_dataset, Radius_data_occupation)
final_dataset <- cbind(final_dataset, RDP_data_occupation)
final_dataset <- cbind(final_dataset, RTMP_data_occupation)
final_dataset <- cbind(final_dataset, RTP_data_occupation)
final_dataset <- cbind(final_dataset, RTSP_data_occupation)
final_dataset <- cbind(final_dataset, RX_data_occupation)
final_dataset <- cbind(final_dataset, SAP_data_occupation)
final_dataset <- cbind(final_dataset, sFlow_data_occupation)
final_dataset <- cbind(final_dataset, Signal_data_occupation)
final_dataset <- cbind(final_dataset, Sina_Weibo_data_occupation)
final_dataset <- cbind(final_dataset, SIP_data_occupation)
final_dataset <- cbind(final_dataset, Skype_data_occupation)
final_dataset <- cbind(final_dataset, SkypeCall_data_occupation)
final_dataset <- cbind(final_dataset, Slack_data_occupation)
final_dataset <- cbind(final_dataset, SMBv1_data_occupation)
final_dataset <- cbind(final_dataset, SMBv23_data_occupation)
final_dataset <- cbind(final_dataset, SMTP_data_occupation)
final_dataset <- cbind(final_dataset, SMTPS_data_occupation)
final_dataset <- cbind(final_dataset, Snapchat_data_occupation)
final_dataset <- cbind(final_dataset, SNMP_data_occupation)
final_dataset <- cbind(final_dataset, SOCKS_data_occupation)
final_dataset <- cbind(final_dataset, SOMEIP_data_occupation)
final_dataset <- cbind(final_dataset, SoundCloud_data_occupation)
final_dataset <- cbind(final_dataset, Spotify_data_occupation)
final_dataset <- cbind(final_dataset, SSDP_data_occupation)
final_dataset <- cbind(final_dataset, SSH_data_occupation)
final_dataset <- cbind(final_dataset, Starcraft_data_occupation)
final_dataset <- cbind(final_dataset, Steam_data_occupation)
final_dataset <- cbind(final_dataset, STUN_data_occupation)
final_dataset <- cbind(final_dataset, Syslog_data_occupation)
final_dataset <- cbind(final_dataset, Targus_Dataspeed_data_occupation)
final_dataset <- cbind(final_dataset, TeamViewer_data_occupation)
final_dataset <- cbind(final_dataset, Telegram_data_occupation)
final_dataset <- cbind(final_dataset, Teredo_data_occupation)
final_dataset <- cbind(final_dataset, TikTok_data_occupation)
final_dataset <- cbind(final_dataset, TLS_data_occupation)
final_dataset <- cbind(final_dataset, Tor_data_occupation)
final_dataset <- cbind(final_dataset, Tuenti_data_occupation)
final_dataset <- cbind(final_dataset, Twitch_data_occupation)
final_dataset <- cbind(final_dataset, Twitter_data_occupation)
final_dataset <- cbind(final_dataset, UBNTAC2_data_occupation)
final_dataset <- cbind(final_dataset, UbuntuONE_data_occupation)
final_dataset <- cbind(final_dataset, Unencrypted_Jabber_data_occupation)
final_dataset <- cbind(final_dataset, Unknown_data_occupation)
final_dataset <- cbind(final_dataset, UPnP_data_occupation)
final_dataset <- cbind(final_dataset, Viber_data_occupation)
final_dataset <- cbind(final_dataset, VNC_data_occupation)
final_dataset <- cbind(final_dataset, Waze_data_occupation)
final_dataset <- cbind(final_dataset, Webex_data_occupation)
final_dataset <- cbind(final_dataset, WeChat_data_occupation)
final_dataset <- cbind(final_dataset, WhatsApp_data_occupation)
final_dataset <- cbind(final_dataset, WhatsAppCall_data_occupation)
final_dataset <- cbind(final_dataset, WhatsAppFiles_data_occupation)
final_dataset <- cbind(final_dataset, Whois-DAS_data_occupation)
final_dataset <- cbind(final_dataset, Wikipedia_data_occupation)
final_dataset <- cbind(final_dataset, WindowsUpdate_data_occupation)
final_dataset <- cbind(final_dataset, Xbox_data_occupation)
final_dataset <- cbind(final_dataset, Yahoo_data_occupation)
final_dataset <- cbind(final_dataset, YouTube_data_occupation)
final_dataset <- cbind(final_dataset, Zoom_data_occupation)

write.csv(final_dataset, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Unicauca_Users-Consumption-dataset-April_June_2019.csv", row.names = FALSE, quote = FALSE, sep=",")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------OBTAINING THE BEST NUMBER OF CLUSTERS------------------------------------------------------------------------------------------------------------------

test_data <- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Final Dataset - Users Consumption 2019 - OTT apps Only/Unicauca_Users-Consumption-dataset-April_June_2019-OTT_Apps_Only.csv")

final_dataset$src_ip <- NULL
test_data$src_ip <- NULL
test_data$src_ip_numeric <- NULL
df_clusters <- scale(test_data)
rm(df_clusters_noNA)

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
  
