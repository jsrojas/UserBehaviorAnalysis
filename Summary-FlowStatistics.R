# This script is intended to summarize the consumption behavior of the users by creating a new dataset where each instance is a user.
# The idea is to obtain the Users' devices IP addresses in both network and decimal format and then count the number of flows per OTT application 
# and obtain the mean values of certain attributes in order to classify the users on a certain category
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# This is to clean the memory from stored data
rm(list = ls())
rm(kmeans2)
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
dataset<- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/22-04-2019-labeled-2pm-to-4_30pm.csv")

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

write.csv(df, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/22-04-2019-2pm-to-4_30pm-SepparatedLabels.csv", row.names = FALSE, quote = FALSE, sep=",")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------REPLACE NAMES OF APPLICATION LABELS -----------------------------------------------------------------------------------------------------
# Show the different levels or labels of the column
levels(df$web_service)
# Show the number of applications found in the dataset
length(levels(df$web_service))

# Add a new level or label to the ones that the dataframe already has so it can be used as replacement
# levels(df$application_name) <- c(levels(df$application_name), "104", "AJP","Amazon","AmazonVideo","Apple","AppleiCloud","AppleiTunes","ApplePush", "AppleStore","BitTorrent","CiscoSkinny", "CiscoVPN","Citrix",
#                                  "Cloudflare","CNN","CSGO","DataSaver","DHCP","DNS","DNSoverHTTPS","Dropbox","eBay", "Facebook", "FTP_CONTROL","FTP_DATA","Github", "GMail","Google","GoogleDocs", "GoogleDrive",
#                                  "GoogleHangoutDuo", "GoogleMaps", "GooglePlus", "GoogleServices","HTTP","HTTP_Proxy","IAX", "ICMP","IMAPS" ,"IMO","IPsec","Instagram","LDAP", "LinkedIn", "Messenger", "Microsoft",
#                                  "MQTT","MS_OneDrive","MsSQL-TDS","NetBIOS", "NetFlix","NTP", "Office365", "Ookla","OpenDNS", "Oracle", "Playstation", "PlayStore","PostgreSQL", "PS_VUE", "QQ","QUIC","Radius", 
#                                  "RDP","RTMP","RX","SAP","sFlow","Signal", "Sina(Weibo)", "SIP", "Skype", "Slack","SMBv1","SMBv23","SMTP","Snapchat","SNMP","SOCKS", "Spotify","SSDP","SSH","Starcraft", "Steam", 
#                                  "STUN","Syslog","Targus_Dataspeed","TeamViewer","Telegram", "Teredo","TikTok","TLS", "Twitter", "UbuntuONE","Unencrypted_Jabber","Unknown", "Viber", "VNC","Webex", "WeChat", 
#                                  "WhatsApp", "WhatsAppFiles", "Wikipedia", "WindowsUpdate", "Xbox", "Yahoo", "YouTube")
# 
# Replace the label names with the ones just added
# df$application_name[df$application_name=="DNS.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="DNS.AmazonVideo"] <- "AmazonVideo"
# df$application_name[df$application_name=="DNS.Apple"] <- "Apple"
# df$application_name[df$application_name=="DNS.AppleiCloud"] <- "AppleiCloud"
# df$application_name[df$application_name=="DNS.AppleiTunes"] <- "AppleiTunes"
# df$application_name[df$application_name=="DNS.ApplePush"] <- "ApplePush"
# df$application_name[df$application_name=="DNS.AppleStore"] <- "AppleStore"
# df$application_name[df$application_name=="DNS.CNN"] <- "CNN"
# df$application_name[df$application_name=="DNS.DataSaver"] <- "DataSaver"
# df$application_name[df$application_name=="DNS.DNSoverHTTPS"] <- "DNSoverHTTPS"
# df$application_name[df$application_name=="DNS.Dropbox"] <- "Dropbox"
# df$application_name[df$application_name=="DNS.eBay"] <- "eBay"
# df$application_name[df$application_name=="DNS.Facebook"] <- "Facebook"
# df$application_name[df$application_name=="DNS.Github"] <- "Github"
# df$application_name[df$application_name=="DNS.GMail"] <- "GMail"
# df$application_name[df$application_name=="DNS.Google"] <- "Google"
# df$application_name[df$application_name=="DNS.GoogleDocs"] <- "GoogleDocs"
# df$application_name[df$application_name=="DNS.GoogleDrive"] <- "GoogleDrive"
# df$application_name[df$application_name=="DNS.GoogleMaps"] <- "GoogleMaps"
# df$application_name[df$application_name=="DNS.GooglePlus"] <- "GooglePlus"
# df$application_name[df$application_name=="DNS.GoogleServices"] <- "GoogleServices"
# df$application_name[df$application_name=="DNS.Instagram"] <- "Instagram"
# df$application_name[df$application_name=="DNS.LinkedIn"] <- "LinkedIn"
# df$application_name[df$application_name=="DNS.Messenger"] <- "Messenger"
# df$application_name[df$application_name=="DNS.Microsoft"] <- "Microsoft"
# df$application_name[df$application_name=="DNS.MS_OneDrive"] <- "MS_OneDrive"
# df$application_name[df$application_name=="DNS.MSN"] <- "Messenger"
# df$application_name[df$application_name=="DNS.NetFlix"] <- "NetFlix"
# df$application_name[df$application_name=="DNS.Office365"] <- "Office365"
# df$application_name[df$application_name=="DNS.OpenDNS"] <- "OpenDNS"
# df$application_name[df$application_name=="DNS.Playstation"] <- "Playstation"
# df$application_name[df$application_name=="DNS.PlayStore"] <- "PlayStore"
# df$application_name[df$application_name=="DNS.QQ"] <- "QQ"
# df$application_name[df$application_name=="DNS.Sina(Weibo)"] <- "Sina(Weibo)"
# df$application_name[df$application_name=="DNS.Skype"] <- "Skype"
# df$application_name[df$application_name=="DNS.Slack"] <- "Slack"
# df$application_name[df$application_name=="DNS.Spotify"] <- "Spotify"
# df$application_name[df$application_name=="DNS.Steam"] <- "Steam"
# df$application_name[df$application_name=="DNS.Telegram"] <- "Telegram"
# df$application_name[df$application_name=="DNS.TikTok"] <- "TikTok"
# df$application_name[df$application_name=="DNS.Twitter"] <- "Twitter"
# df$application_name[df$application_name=="DNS.UbuntuONE"] <- "UbuntuONE"
# df$application_name[df$application_name=="DNS.Webex"] <- "Webex"
# df$application_name[df$application_name=="DNS.WhatsApp"] <- "WhatsApp"
# df$application_name[df$application_name=="DNS.WhatsAppFiles"] <- "WhatsAppFiles"
# df$application_name[df$application_name=="DNS.Wikipedia"] <- "Wikipedia"
# df$application_name[df$application_name=="DNS.WindowsUpdate"] <- "WindowsUpdate"
# df$application_name[df$application_name=="DNS.Xbox"] <- "Xbox"
# df$application_name[df$application_name=="DNS.Yahoo"] <- "Yahoo"
# df$application_name[df$application_name=="DNS.YouTube"] <- "YouTube"
# df$application_name[df$application_name=="HTTP.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="HTTP.AmazonVideo"] <- "AmazonVideo"
# df$application_name[df$application_name=="HTTP.Apple"] <- "Apple"
# df$application_name[df$application_name=="HTTP.AppleiCloud"] <- "AppleiCloud"
# df$application_name[df$application_name=="HTTP.ApplePush"] <- "ApplePush"
# df$application_name[df$application_name=="HTTP.AppleStore"] <- "AppleStore"
# df$application_name[df$application_name=="HTTP.Cloudflare"] <- "Cloudflare"
# df$application_name[df$application_name=="HTTP.CNN"] <- "CNN"
# df$application_name[df$application_name=="HTTP.DataSaver"] <- "DataSaver"
# df$application_name[df$application_name=="HTTP.Dropbox"] <- "Dropbox"
# df$application_name[df$application_name=="HTTP.Facebook"] <- "Facebook"
# df$application_name[df$application_name=="HTTP.GMail"] <- "GMail"
# df$application_name[df$application_name=="HTTP.Google"] <- "Google"
# df$application_name[df$application_name=="HTTP.GoogleDocs"] <- "GoogleDocs"
# df$application_name[df$application_name=="HTTP.GoogleDrive"] <- "GoogleDrive"
# df$application_name[df$application_name=="HTTP.GoogleMaps"] <- "GoogleMaps"
# df$application_name[df$application_name=="HTTP.GoogleServices"] <- "GoogleServices"
# df$application_name[df$application_name=="HTTP.HTTP"] <- "HTTP"
# df$application_name[df$application_name=="HTTP.Instagram"] <- "Instagram"
# df$application_name[df$application_name=="HTTP.LinkedIn"] <- "LinkedIn"
# df$application_name[df$application_name=="HTTP.Microsoft"] <- "Microsoft"
# df$application_name[df$application_name=="HTTP.MS_OneDrive"] <- "MS_OneDrive"
# df$application_name[df$application_name=="HTTP.MSN"] <- "Messenger"
# df$application_name[df$application_name=="HTTP.NetFlix"] <- "NetFlix"
# df$application_name[df$application_name=="HTTP.Office365"] <- "Office365"
# df$application_name[df$application_name=="HTTP.Ookla"] <- "Ookla"
# df$application_name[df$application_name=="HTTP.PS_VUE"] <- "PS_VUE"
# df$application_name[df$application_name=="HTTP.QQ"] <- "QQ"
# df$application_name[df$application_name=="HTTP.Skype"] <- "Skype"
# df$application_name[df$application_name=="HTTP.Spotify"] <- "Spotify"
# df$application_name[df$application_name=="HTTP.Steam"] <- "Steam"
# df$application_name[df$application_name=="HTTP.Telegram"] <- "Telegram"
# df$application_name[df$application_name=="HTTP.TikTok"] <- "TikTok"
# df$application_name[df$application_name=="HTTP.Twitter"] <- "Twitter"
# df$application_name[df$application_name=="HTTP.UbuntuONE"] <- "UbuntuONE"
# df$application_name[df$application_name=="HTTP.WeChat"] <- "WeChat"
# df$application_name[df$application_name=="HTTP.WhatsApp"] <- "WhatsApp"
# df$application_name[df$application_name=="HTTP.WindowsUpdate"] <- "WindowsUpdate"
# df$application_name[df$application_name=="HTTP.Xbox"] <- "Xbox"
# df$application_name[df$application_name=="HTTP.Yahoo"] <- "Yahoo"
# df$application_name[df$application_name=="HTTP.YouTube"] <- "YouTube"
# df$application_name[df$application_name=="IMAPS.Apple"] <- "Apple"
# df$application_name[df$application_name=="IMAPS.GMail"] <- "GMail"
# df$application_name[df$application_name=="IMAPS.Google"] <- "Google"
# df$application_name[df$application_name=="NetBIOS.Google"] <- "Google"
# df$application_name[df$application_name=="POPS.GMail"] <- "GMail"
# df$application_name[df$application_name=="POPS.Google"] <- "Google"
# df$application_name[df$application_name=="PostgreSQL.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="QUIC.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="QUIC.Cloudflare"] <- "Cloudflare"
# df$application_name[df$application_name=="QUIC.DataSaver"] <- "DataSaver"
# df$application_name[df$application_name=="QUIC.Google"] <- "Google"
# df$application_name[df$application_name=="QUIC.GoogleDocs"] <- "GoogleDocs"
# df$application_name[df$application_name=="QUIC.GoogleDrive"] <- "GoogleDrive"
# df$application_name[df$application_name=="QUIC.GoogleMaps"] <- "GoogleMaps"
# df$application_name[df$application_name=="QUIC.GooglePlus"] <- "GooglePlus"
# df$application_name[df$application_name=="QUIC.GoogleServices"] <- "GoogleServices"
# df$application_name[df$application_name=="QUIC.PlayStore"] <- "PlayStore"
# df$application_name[df$application_name=="QUIC.YouTube"] <- "YouTube"
# df$application_name[df$application_name=="RDP.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="RDP.Skype"] <- "Skype"
# df$application_name[df$application_name=="Skype.SkypeCall"] <- "Skype"
# df$application_name[df$application_name=="SMBv23.Google"] <- "Google"
# df$application_name[df$application_name=="SMTPS.GMail"] <- "GMail"
# df$application_name[df$application_name=="SMTPS.Google"] <- "Google"
# df$application_name[df$application_name=="SOCKS.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="STUN.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="STUN.GoogleHangoutDuo"] <- "GoogleHangoutDuo"
# df$application_name[df$application_name=="STUN.Signal"] <- "Signal"
# df$application_name[df$application_name=="TLS.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="TLS.AmazonVideo"] <- "AmazonVideo"
# df$application_name[df$application_name=="TLS.Apple"] <- "Apple"
# df$application_name[df$application_name=="TLS.AppleiCloud"] <- "AppleiCloud"
# df$application_name[df$application_name=="TLS.AppleiTunes"] <- "AppleiTunes"
# df$application_name[df$application_name=="TLS.ApplePush"] <- "ApplePush"
# df$application_name[df$application_name=="TLS.AppleStore"] <- "AppleStore"
# df$application_name[df$application_name=="TLS.Cloudflare"] <- "Cloudflare"
# df$application_name[df$application_name=="TLS.CNN"] <- "CNN"
# df$application_name[df$application_name=="TLS.DataSaver"] <- "DataSaver"
# df$application_name[df$application_name=="TLS.DNSoverHTTPS"] <- "DNSoverHTTPS"
# df$application_name[df$application_name=="TLS.Dropbox"] <- "Dropbox"
# df$application_name[df$application_name=="TLS.eBay"] <- "eBay"
# df$application_name[df$application_name=="TLS.Facebook"] <- "Facebook"
# df$application_name[df$application_name=="TLS.Github"] <- "Github"
# df$application_name[df$application_name=="TLS.GMail"] <- "GMail"
# df$application_name[df$application_name=="TLS.Google"] <- "Google"
# df$application_name[df$application_name=="TLS.GoogleDocs"] <- "GoogleDocs"
# df$application_name[df$application_name=="TLS.GoogleDrive"] <- "GoogleDrive"
# df$application_name[df$application_name=="TLS.GoogleMaps"] <- "GoogleMaps"
# df$application_name[df$application_name=="TLS.GooglePlus"] <- "GooglePlus"
# df$application_name[df$application_name=="TLS.GoogleServices"] <- "GoogleServices"
# df$application_name[df$application_name=="TLS.IMAPS"] <- "IMAPS"
# df$application_name[df$application_name=="TLS.Instagram"] <- "Instagram"
# df$application_name[df$application_name=="TLS.LinkedIn"] <- "LinkedIn"
# df$application_name[df$application_name=="TLS.Messenger"] <- "Messenger"
# df$application_name[df$application_name=="TLS.Microsoft"] <- "Microsoft"
# df$application_name[df$application_name=="TLS.MS_OneDrive"] <- "MS_OneDrive"
# df$application_name[df$application_name=="TLS.MSN"] <- "Messenger"
# df$application_name[df$application_name=="TLS.NetFlix"] <- "NetFlix"
# df$application_name[df$application_name=="TLS.Office365"] <- "Office365"
# df$application_name[df$application_name=="TLS.PlayStore"] <- "PlayStore"
# df$application_name[df$application_name=="TLS.QQ"] <- "QQ"
# df$application_name[df$application_name=="TLS.Skype"] <- "Skype"
# df$application_name[df$application_name=="TLS.Slack"] <- "Slack"
# df$application_name[df$application_name=="TLS.Snapchat"] <- "Snapchat"
# df$application_name[df$application_name=="TLS.Spotify"] <- "Spotify"
# df$application_name[df$application_name=="TLS.Starcraft"] <- "Starcraft"
# df$application_name[df$application_name=="TLS.Steam"] <- "Steam"
# df$application_name[df$application_name=="TLS.Telegram"] <- "Telegram"
# df$application_name[df$application_name=="TLS.TikTok"] <- "TikTok"
# df$application_name[df$application_name=="TLS.Twitter"] <- "Twitter"
# df$application_name[df$application_name=="TLS.UbuntuONE"] <- "UbuntuONE"
# df$application_name[df$application_name=="TLS.Webex"] <- "Webex"
# df$application_name[df$application_name=="TLS.WhatsApp"] <- "WhatsApp"
# df$application_name[df$application_name=="TLS.Wikipedia"] <- "Wikipedia"
# df$application_name[df$application_name=="TLS.WindowsUpdate"] <- "WindowsUpdate"
# df$application_name[df$application_name=="TLS.Xbox"] <- "Xbox"
# df$application_name[df$application_name=="TLS.Yahoo"] <- "Yahoo"
# df$application_name[df$application_name=="TLS.YouTube"] <- "YouTube"
# df$application_name[df$application_name=="Unknown.104"] <- "104"
# df$application_name[df$application_name=="Unknown.AJP"] <- "AJP"
# df$application_name[df$application_name=="Unknown.Amazon"] <- "Amazon"
# df$application_name[df$application_name=="Unknown.ApplePush"] <- "ApplePush"
# df$application_name[df$application_name=="Unknown.BitTorrent"] <- "BitTorrent"
# df$application_name[df$application_name=="Unknown.CiscoSkinny"] <- "CiscoSkinny"
# df$application_name[df$application_name=="Unknown.CiscoVPN"] <- "CiscoVPN"
# df$application_name[df$application_name=="Unknown.Citrix"] <- "Citrix"
# df$application_name[df$application_name=="Unknown.Cloudflare"] <- "Cloudflare"
# df$application_name[df$application_name=="Unknown.CSGO"] <- "CSGO"
# df$application_name[df$application_name=="Unknown.DHCP"] <- "DHCP"
# df$application_name[df$application_name=="Unknown.DNS"] <- "DNS"
# df$application_name[df$application_name=="Unknown.Facebook"] <- "Facebook"
# df$application_name[df$application_name=="Unknown.FTP_CONTROL"] <- "FTP_CONTROL"
# df$application_name[df$application_name=="Unknown.FTP_DATA"] <- "FTP_DATA"
# df$application_name[df$application_name=="Unknown.Google"] <- "Google"
# df$application_name[df$application_name=="Unknown.GoogleHangoutDuo"] <- "GoogleHangoutDuo"
# df$application_name[df$application_name=="Unknown.HTTP"] <- "HTTP"
# df$application_name[df$application_name=="Unknown.HTTP_Proxy"] <- "HTTP_Proxy"
# df$application_name[df$application_name=="Unknown.IAX"] <- "IAX"
# df$application_name[df$application_name=="Unknown.ICMP"] <- "ICMP"
# df$application_name[df$application_name=="Unknown.IMAPS"] <- "IMAPS"
# df$application_name[df$application_name=="Unknown.IMO"] <- "IMO"
# df$application_name[df$application_name=="Unknown.IPsec"] <- "IPsec"
# df$application_name[df$application_name=="Unknown.LDAP"] <- "LDAP"
# df$application_name[df$application_name=="Unknown.MQTT"] <- "MQTT"
# df$application_name[df$application_name=="Unknown.MsSQL-TDS"] <- "MsSQL-TDS"
# df$application_name[df$application_name=="Unknown.NetBIOS"] <- "NetBIOS"
# df$application_name[df$application_name=="Unknown.NTP"] <- "NTP"
# df$application_name[df$application_name=="Unknown.Oracle"] <- "Oracle"
# df$application_name[df$application_name=="Unknown.PostgreSQL"] <- "PostgreSQL"
# df$application_name[df$application_name=="Unknown.QUIC"] <- "QUIC"
# df$application_name[df$application_name=="Unknown.Radius"] <- "Radius"
# df$application_name[df$application_name=="Unknown.RDP"] <- "RDP"
# df$application_name[df$application_name=="Unknown.RTMP"] <- "RTMP"
# df$application_name[df$application_name=="Unknown.RX"] <- "RX"
# df$application_name[df$application_name=="Unknown.SAP"] <- "SAP"
# df$application_name[df$application_name=="Unknown.sFlow"] <- "sFlow"
# df$application_name[df$application_name=="Unknown.SIP"] <- "SIP"
# df$application_name[df$application_name=="Unknown.SMBv1"] <- "SMBv1"
# df$application_name[df$application_name=="Unknown.SMBv23"] <- "SMBv23"
# df$application_name[df$application_name=="Unknown.SMTP"] <- "SMTP"
# df$application_name[df$application_name=="Unknown.SNMP"] <- "SNMP"
# df$application_name[df$application_name=="Unknown.SOCKS"] <- "SOCKS"
# df$application_name[df$application_name=="Unknown.Spotify"] <- "Spotify"
# df$application_name[df$application_name=="Unknown.SSDP"] <- "SSDP"
# df$application_name[df$application_name=="Unknown.SSH"] <- "SSH"
# df$application_name[df$application_name=="Unknown.Steam"] <- "Steam"
# df$application_name[df$application_name=="Unknown.STUN"] <- "STUN"
# df$application_name[df$application_name=="Unknown.Syslog"] <- "Syslog"
# df$application_name[df$application_name=="Unknown.Targus Dataspeed"] <- "Targus_Dataspeed"
# df$application_name[df$application_name=="Unknown.TeamViewer"] <- "TeamViewer"
# df$application_name[df$application_name=="Unknown.Telegram"] <- "Telegram"
# df$application_name[df$application_name=="Unknown.Teredo"] <- "Teredo"
# df$application_name[df$application_name=="Unknown.TLS"] <- "TLS"
# df$application_name[df$application_name=="Unknown.Unencrypted_Jabber"] <- "GoogleServices"
# df$application_name[df$application_name=="Unknown.Unknown"] <- "Unknown"
# df$application_name[df$application_name=="Unknown.Viber"] <- "Viber"
# df$application_name[df$application_name=="Unknown.VNC"] <- "VNC"
# df$application_name[df$application_name=="Unknown.WhatsApp"] <- "WhatsApp"

# Remove rows from non-OTT applications from the dataset
df <- subset(df, application_name!="104")
df <- subset(df, application_name!="AJP")
df <- subset(df, application_name!="CiscoSkinny")
df <- subset(df, application_name!="CiscoVPN")
df <- subset(df, application_name!="Citrix")
df <- subset(df, application_name!="Cloudflare")
df <- subset(df, application_name!="DHCP")
df <- subset(df, application_name!="DNS")
df <- subset(df, application_name!="DNSoverHTTPS")
df <- subset(df, application_name!="FTP_CONTROL")
df <- subset(df, application_name!="FTP_DATA")
df <- subset(df, application_name!="IAX")
df <- subset(df, application_name!="ICMP")
df <- subset(df, application_name!="IMAPS")
df <- subset(df, application_name!="IPsec")
df <- subset(df, application_name!="LDAP")
df <- subset(df, application_name!="Microsoft")
df <- subset(df, application_name!="MQTT")
df <- subset(df, application_name!="MsSQL-TDS")
df <- subset(df, application_name!="NetBIOS")
df <- subset(df, application_name!="NTP")
df <- subset(df, application_name!="Office365")
df <- subset(df, application_name!="Ookla")
df <- subset(df, application_name!="OpenDNS")
df <- subset(df, application_name!="Oracle")
df <- subset(df, application_name!="PostgreSQL")
df <- subset(df, application_name!="PS_VUE")
df <- subset(df, application_name!="QQ")
df <- subset(df, application_name!="QUIC")
df <- subset(df, application_name!="Radius")
df <- subset(df, application_name!="RDP")
df <- subset(df, application_name!="RTMP")
df <- subset(df, application_name!="RX")
df <- subset(df, application_name!="SAP")
df <- subset(df, application_name!="sFlow")
df <- subset(df, application_name!="Signal")
df <- subset(df, application_name!="Sina(Weibo)")
df <- subset(df, application_name!="SIP")
df <- subset(df, application_name!="SMBv1")
df <- subset(df, application_name!="SMBv23")
df <- subset(df, application_name!="SMTP")
df <- subset(df, application_name!="SNMP")
df <- subset(df, application_name!="SOCKS")
df <- subset(df, application_name!="SSDP")
df <- subset(df, application_name!="SSH")
df <- subset(df, application_name!="STUN")
df <- subset(df, application_name!="Syslog")
df <- subset(df, application_name!="Targus_Dataspeed")
df <- subset(df, application_name!="Teredo")
df <- subset(df, application_name!="TLS")
df <- subset(df, application_name!="UbuntuONE")
df <- subset(df, application_name!="Unencrypted_Jabber")
df <- subset(df, application_name!="Unknown")
df <- subset(df, application_name!="VNC")
df <- subset(df, application_name!="Webex")
df <- subset(df, application_name!="WindowsUpdate")

# Check the different levels or tags of a column (YouTube, Facebook, etc)
levels(df$web_service)

# Remove unused labels from the levels of the application_name column
# so it does not generate more columns in the following steps
df$application_name <- factor(df$application_name)

summary(df$application_name)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------COUNT THE NUMBER OF FLOWS PER OTT APPLICATION------------------------------------------------------------
# Obtain a summary of how many instances are per application label for each src ip
library(data.table)
#flows_per_app <- dcast(setDT(df), src_ip~application_name, length)
flows_per_app <- dcast(setDT(df), src_ip~web_service, length)
head(flows_per_app)

# Set the column names in their order - 105 applications (CSGO and WeChat not appear)
colnames(flows_per_app) <- c("src_ip", 
                             "104_flows", "AJP_flows", "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows",
                             "AppleStore_flows", "BitTorrent_flows", "CiscoSkinny_flows", "CiscoVPN_flows", "Citrix_flows", "Cloudflare_flows", "CNN_flows", "DataSaver_flows",
                             "DHCP_flows", "DNS_flows", "DNSoverHTTPS_flows", "Dropbox_flows", "eBay_flows", "Facebook_flows", "FTP_DATA_flows", "Github_flows",          
                             "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows",  
                             "HTTP_flows", "HTTP_Proxy_flows", "IAX_flows", "ICMP_flows", "IMAPS_flows", "IMO_flows", "Instagram_flows", "IPsec_flows",       
                             "LDAP_flows", "LinkedIn_flows", "Messenger_flows", "Microsoft_flows", "MQTT_flows", "MS_OneDrive_flows", "MSN", "MsSQL-TDS_flows", 
                             "NetBIOS_flows", "NetFlix_flows", "NTP_flows", "Office365_flows", "Ookla_flows", "OpenDNS_flows", "Oracle_flows", "Playstation_flows", 
                             "PlayStore_flows", "PostgreSQL_flows", "PS_VUE_flows", "QQ_flows", "QUIC_flows", "Radius_flows", "RDP_flows", "RTMP_flows", 
                             "RX_flows", "sFlow_flows", "Signal_flows", "Sina(Weibo)_flows", "SIP_flows", "Skype_flows","SkypeCall", "Slack_flows", 
                             "SMBv1_flows", "SMBv23_flows", "SMTP_flows", "Snapchat_flows", "SNMP_flows", "SOCKS_flows", "Spotify_flows", "SSDP_flows", 
                             "SSH_flows", "Starcraft_flows", "Steam_flows", "STUN_flows", "Syslog_flows", "TeamViewer_flows", "Telegram_flows", "Teredo_flows", 
                             "TikTok_flows", "TLS_flows", "Twitter_flows", "UbuntuONE_flows", "Unencrypted_Jabber","Unknown_flows", "Viber_flows", "VNC_flows", 
                             "Webex_flows", "WhatsApp_flows", "WhatsAppFiles_flows", "Wikipedia_flows", "WindowsUpdate_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")


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

# # Set the column names in their order -  105 applications (without WeChat and CSGO)
# colnames(flows_per_app) <- c("src_ip", 
#                              "104_flows", "AJP_flows", "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows",
#                              "AppleStore_flows", "BitTorrent_flows", "CiscoSkinny_flows", "CiscoVPN_flows", "Citrix_flows", "Cloudflare_flows", "CNN_flows", "DataSaver_flows",
#                              "DHCP_flows", "DNS_flows", "DNSoverHTTPS_flows", "Dropbox_flows", "eBay_flows", "Facebook_flows", "FTP_DATA_flows", "Github_flows",          
#                              "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows",  
#                              "HTTP_flows", "HTTP_Proxy_flows", "IAX_flows", "ICMP_flows", "IMAPS_flows", "IMO_flows", "IPsec_flows", "Instagram_flows",       
#                              "LDAP_flows", "LinkedIn_flows", "Messenger_flows", "Microsoft_flows", "MQTT_flows", "MS_OneDrive_flows", "MsSQL-TDS_flows", "NetBIOS_flows",         
#                              "NetFlix_flows", "NTP_flows", "Office365_flows", "Ookla_flows", "OpenDNS_flows", "Oracle_flows", "Playstation_flows", "PlayStore_flows",       
#                              "PostgreSQL_flows", "PS_VUE_flows", "QQ_flows", "QUIC_flows", "Radius_flows", "RDP_flows", "RTMP_flows", "RX_flows",              
#                              "sFlow_flows", "Signal_flows", "Sina(Weibo)_flows", "SIP_flows", "Skype_flows", "Slack_flows", "SMBv1_flows", "SMBv23_flows",          
#                              "SMTP_flows", "Snapchat_flows", "SNMP_flows", "SOCKS_flows", "Spotify_flows", "SSDP_flows", "SSH_flows", "Starcraft_flows",       
#                              "Steam_flows", "STUN_flows", "Syslog_flows", "TeamViewer_flows", "Telegram_flows", "Teredo_flows", "TikTok_flows", "TLS_flows",             
#                              "Twitter_flows", "UbuntuONE_flows", "Unknown_flows", "Viber_flows", "VNC_flows", "Webex_flows", "WhatsApp_flows", "WhatsAppFiles_flows",   
#                              "Wikipedia_flows", "WindowsUpdate_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")
# 
# # Set the column names in their order - 49 OTT Applications only (without CSGO and WeChat)
# colnames(flows_per_app) <- c("src_ip", 
#                              "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows",
#                              "AppleStore_flows", "BitTorrent_flows", "CNN_flows", "DataSaver_flows",
#                              "Dropbox_flows", "eBay_flows", "Facebook_flows", "Github_flows",          
#                              "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows",  
#                              "HTTP_flows", "HTTP_Proxy_flows", "IMO_flows", "Instagram_flows",       
#                              "LinkedIn_flows", "Messenger_flows", "MS_OneDrive_flows",         
#                              "NetFlix_flows", "Playstation_flows", "PlayStore_flows",       
#                              "Skype_flows", "Slack_flows", "Snapchat_flows", "Spotify_flows", "Starcraft_flows",       
#                              "Steam_flows", "TeamViewer_flows", "Telegram_flows", "TikTok_flows",             
#                              "Twitter_flows", "Viber_flows", "WhatsApp_flows", "WhatsAppFiles_flows",   
#                              "Wikipedia_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")
# 
# # Set the column names in their order - 51 OTT Applications only (with CSGO and WeChat)
# colnames(flows_per_app) <- c("src_ip", 
#                              "Amazon_flows", "AmazonVideo_flows", "Apple_flows", "AppleiCloud_flows", "AppleiTunes_flows", "ApplePush_flows",
#                              "AppleStore_flows", "BitTorrent_flows", "CNN_flows","CSGO_flows", "DataSaver_flows",
#                              "Dropbox_flows", "eBay_flows", "Facebook_flows", "Github_flows",          
#                              "GMail_flows", "Google_flows", "GoogleDocs_flows", "GoogleDrive_flows", "GoogleHangoutDuo_flows", "GoogleMaps_flows", "GooglePlus_flows", "GoogleServices_flows",  
#                              "HTTP_flows", "HTTP_Proxy_flows", "IMO_flows", "Instagram_flows",       
#                              "LinkedIn_flows", "Messenger_flows", "MS_OneDrive_flows",         
#                              "NetFlix_flows", "Playstation_flows", "PlayStore_flows",       
#                              "Skype_flows", "Slack_flows", "Snapchat_flows", "Spotify_flows", "Starcraft_flows",       
#                              "Steam_flows", "TeamViewer_flows", "Telegram_flows", "TikTok_flows",             
#                              "Twitter_flows", "Viber_flows", "WeChat_flows", "WhatsApp_flows", "WhatsAppFiles_flows",   
#                              "Wikipedia_flows", "Xbox_flows", "Yahoo_flows", "YouTube_flows")

# Count the number of flows per application - useful for graphs
df %>% count(web_service)

#-----------------------------------------------------OBTAIN THE MEAN OCTET TOTAL COUNT--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP address and application name
mean_octet_total_count.df <- as.data.frame(tapply(df$octetTotalCount, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_octet_total_count.df) <- 1:nrow(mean_octet_total_count.df)
# Replace NA with 0
mean_octet_total_count.df[is.na((mean_octet_total_count.df))] <- 0

# Set the column names in their order - 105 applications (CSGO and WeChat not appear)
colnames(mean_octet_total_count.df) <- c("104_mean_pkt_size", "AJP_mean_pkt_size", "Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size",
                             "AppleStore_mean_pkt_size", "BitTorrent_mean_pkt_size", "CiscoSkinny_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Citrix_mean_pkt_size", "Cloudflare_mean_pkt_size", "CNN_mean_pkt_size", "DataSaver_mean_pkt_size",
                             "DHCP_mean_pkt_size", "DNS_mean_pkt_size", "DNSoverHTTPS_mean_pkt_size", "Dropbox_mean_pkt_size", "eBay_mean_pkt_size", "Facebook_mean_pkt_size", "FTP_DATA_mean_pkt_size", "Github_mean_pkt_size",          
                             "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size",  
                             "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "IAX_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size", "IMO_mean_pkt_size", "Instagram_mean_pkt_size", "IPsec_mean_pkt_size",       
                             "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size", "MQTT_mean_pkt_size", "MS_OneDrive_mean_pkt_size", "MSN", "MsSQL-TDS_mean_pkt_size", 
                             "NetBIOS_mean_pkt_size", "NetFlix_mean_pkt_size", "NTP_mean_pkt_size", "Office365_mean_pkt_size", "Ookla_mean_pkt_size", "OpenDNS_mean_pkt_size", "Oracle_mean_pkt_size", "Playstation_mean_pkt_size", 
                             "PlayStore_mean_pkt_size", "PostgreSQL_mean_pkt_size", "PS_VUE_mean_pkt_size", "QQ_mean_pkt_size", "QUIC_mean_pkt_size", "Radius_mean_pkt_size", "RDP_mean_pkt_size", "RTMP_mean_pkt_size", 
                             "RX_mean_pkt_size", "sFlow_mean_pkt_size", "Signal_mean_pkt_size", "Sina(Weibo)_mean_pkt_size", "SIP_mean_pkt_size", "Skype_mean_pkt_size","SkypeCall", "Slack_mean_pkt_size", 
                             "SMBv1_mean_pkt_size", "SMBv23_mean_pkt_size", "SMTP_mean_pkt_size", "Snapchat_mean_pkt_size", "SNMP_mean_pkt_size", "SOCKS_mean_pkt_size", "Spotify_mean_pkt_size", "SSDP_mean_pkt_size", 
                             "SSH_mean_pkt_size", "Starcraft_mean_pkt_size", "Steam_mean_pkt_size", "STUN_mean_pkt_size", "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "Teredo_mean_pkt_size", 
                             "TikTok_mean_pkt_size", "TLS_mean_pkt_size", "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unencrypted_Jabber","Unknown_mean_pkt_size", "Viber_mean_pkt_size", "VNC_mean_pkt_size", 
                             "Webex_mean_pkt_size", "WhatsApp_mean_pkt_size", "WhatsAppFiles_mean_pkt_size", "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")

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

# # Set the column names in their order - 105 Applications (without CSGO and WeChat)
# colnames(mean_octet_total_count.df) <- c("104_mean_pkt_size", "AJP_mean_pkt_size", "Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size",
#                                          "AppleStore_mean_pkt_size", "BitTorrent_mean_pkt_size", "CiscoSkinny_mean_pkt_size", "CiscoVPN_mean_pkt_size", "Citrix_mean_pkt_size", "Cloudflare_mean_pkt_size", "CNN_mean_pkt_size", "DataSaver_mean_pkt_size",
#                                          "DHCP_mean_pkt_size", "DNS_mean_pkt_size", "DNSoverHTTPS_mean_pkt_size", "Dropbox_mean_pkt_size", "eBay_mean_pkt_size", "Facebook_mean_pkt_size", "FTP_DATA_mean_pkt_size", "Github_mean_pkt_size",          
#                                          "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size",  
#                                          "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "IAX_mean_pkt_size", "ICMP_mean_pkt_size", "IMAPS_mean_pkt_size", "IMO_mean_pkt_size", "IPsec_mean_pkt_size", "Instagram_mean_pkt_size",       
#                                          "LDAP_mean_pkt_size", "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "Microsoft_mean_pkt_size", "MQTT_mean_pkt_size", "MS_OneDrive_mean_pkt_size", "MsSQL-TDS_mean_pkt_size", "NetBIOS_mean_pkt_size",         
#                                          "NetFlix_mean_pkt_size", "NTP_mean_pkt_size", "Office365_mean_pkt_size", "Ookla_mean_pkt_size", "OpenDNS_mean_pkt_size", "Oracle_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size",       
#                                          "PostgreSQL_mean_pkt_size", "PS_VUE_mean_pkt_size", "QQ_mean_pkt_size", "QUIC_mean_pkt_size", "Radius_mean_pkt_size", "RDP_mean_pkt_size", "RTMP_mean_pkt_size", "RX_mean_pkt_size",              
#                                          "sFlow_mean_pkt_size", "Signal_mean_pkt_size", "Sina(Weibo)_mean_pkt_size", "SIP_mean_pkt_size", "Skype_mean_pkt_size", "Slack_mean_pkt_size", "SMBv1_mean_pkt_size", "SMBv23_mean_pkt_size",          
#                                          "SMTP_mean_pkt_size", "Snapchat_mean_pkt_size", "SNMP_mean_pkt_size", "SOCKS_mean_pkt_size", "Spotify_mean_pkt_size", "SSDP_mean_pkt_size", "SSH_mean_pkt_size", "Starcraft_mean_pkt_size",       
#                                          "Steam_mean_pkt_size", "STUN_mean_pkt_size", "Syslog_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "Teredo_mean_pkt_size", "TikTok_mean_pkt_size", "TLS_mean_pkt_size",             
#                                          "Twitter_mean_pkt_size", "UbuntuONE_mean_pkt_size", "Unknown_mean_pkt_size", "Viber_mean_pkt_size", "VNC_mean_pkt_size", "Webex_mean_pkt_size", "WhatsApp_mean_pkt_size", "WhatsAppFiles_mean_pkt_size",   
#                                          "Wikipedia_mean_pkt_size", "WindowsUpdate_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")
# 
# # Set the column names in their order - 49 OTT Applications only (without CSGO and WeChat)
# colnames(mean_octet_total_count.df) <- c("Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size",
#                              "AppleStore_mean_pkt_size", "BitTorrent_mean_pkt_size", "CNN_mean_pkt_size", "DataSaver_mean_pkt_size",
#                              "Dropbox_mean_pkt_size", "eBay_mean_pkt_size", "Facebook_mean_pkt_size", "Github_mean_pkt_size",          
#                              "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size",  
#                              "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "IMO_mean_pkt_size", "Instagram_mean_pkt_size",       
#                              "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "MS_OneDrive_mean_pkt_size",         
#                              "NetFlix_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size",       
#                              "Skype_mean_pkt_size", "Slack_mean_pkt_size", "Snapchat_mean_pkt_size", "Spotify_mean_pkt_size", "Starcraft_mean_pkt_size",       
#                              "Steam_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "TikTok_mean_pkt_size",             
#                              "Twitter_mean_pkt_size", "Viber_mean_pkt_size", "WhatsApp_mean_pkt_size", "WhatsAppFiles_mean_pkt_size",   
#                              "Wikipedia_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")
# 
# # Set the column names in their order - 51 OTT Applications only (with CSGO and WeChat)
# colnames(mean_octet_total_count.df) <- c("Amazon_mean_pkt_size", "AmazonVideo_mean_pkt_size", "Apple_mean_pkt_size", "AppleiCloud_mean_pkt_size", "AppleiTunes_mean_pkt_size", "ApplePush_mean_pkt_size",
#                              "AppleStore_mean_pkt_size", "BitTorrent_mean_pkt_size", "CNN_mean_pkt_size","CSGO_mean_pkt_size", "DataSaver_mean_pkt_size",
#                              "Dropbox_mean_pkt_size", "eBay_mean_pkt_size", "Facebook_mean_pkt_size", "Github_mean_pkt_size",          
#                              "GMail_mean_pkt_size", "Google_mean_pkt_size", "GoogleDocs_mean_pkt_size", "GoogleDrive_mean_pkt_size", "GoogleHangoutDuo_mean_pkt_size", "GoogleMaps_mean_pkt_size", "GooglePlus_mean_pkt_size", "GoogleServices_mean_pkt_size",  
#                              "HTTP_mean_pkt_size", "HTTP_Proxy_mean_pkt_size", "IMO_mean_pkt_size", "Instagram_mean_pkt_size",       
#                              "LinkedIn_mean_pkt_size", "Messenger_mean_pkt_size", "MS_OneDrive_mean_pkt_size",         
#                              "NetFlix_mean_pkt_size", "Playstation_mean_pkt_size", "PlayStore_mean_pkt_size",       
#                              "Skype_mean_pkt_size", "Slack_mean_pkt_size", "Snapchat_mean_pkt_size", "Spotify_mean_pkt_size", "Starcraft_mean_pkt_size",       
#                              "Steam_mean_pkt_size", "TeamViewer_mean_pkt_size", "Telegram_mean_pkt_size", "TikTok_mean_pkt_size",             
#                              "Twitter_mean_pkt_size", "Viber_mean_pkt_size","WeChat_mean_pkt_size", "WhatsApp_mean_pkt_size", "WhatsAppFiles_mean_pkt_size",   
#                              "Wikipedia_mean_pkt_size", "Xbox_mean_pkt_size", "Yahoo_mean_pkt_size", "YouTube_mean_pkt_size")



#-------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------OBTAIN THE MEAN FLOW DURATION PER APPLICATION--------------------------------------------------------------------------
# Obtain the average octet total count based on source IP decimal address and application name
mean_flow_duration.df <- as.data.frame(tapply(df$flowDuration, list(df$src_ip, df$web_service), mean))
# Set the row indexes as numbers
rownames(mean_flow_duration.df) <- 1:nrow(mean_flow_duration.df)
# Replace NA with 0
mean_flow_duration.df[is.na((mean_flow_duration.df))] <- 0

# Set the column names in their order - All applications
colnames(mean_flow_duration.df) <- c("104_mean_flow_duration", "AJP_mean_flow_duration", "Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration",
                                     "AppleStore_mean_flow_duration", "BitTorrent_mean_flow_duration", "CiscoSkinny_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Citrix_mean_flow_duration", "Cloudflare_mean_flow_duration", "CNN_mean_flow_duration", "DataSaver_mean_flow_duration",
                                     "DHCP_mean_flow_duration", "DNS_mean_flow_duration", "DNSoverHTTPS_mean_flow_duration", "Dropbox_mean_flow_duration", "eBay_mean_flow_duration", "Facebook_mean_flow_duration", "FTP_DATA_mean_flow_duration", "Github_mean_flow_duration",          
                                     "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration",  
                                     "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "IAX_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration", "IMO_mean_flow_duration", "Instagram_mean_flow_duration", "IPsec_mean_flow_duration",       
                                     "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration", "MQTT_mean_flow_duration", "MS_OneDrive_mean_flow_duration", "MSN", "MsSQL-TDS_mean_flow_duration", 
                                     "NetBIOS_mean_flow_duration", "NetFlix_mean_flow_duration", "NTP_mean_flow_duration", "Office365_mean_flow_duration", "Ookla_mean_flow_duration", "OpenDNS_mean_flow_duration", "Oracle_mean_flow_duration", "Playstation_mean_flow_duration", 
                                     "PlayStore_mean_flow_duration", "PostgreSQL_mean_flow_duration", "PS_VUE_mean_flow_duration", "QQ_mean_flow_duration", "QUIC_mean_flow_duration", "Radius_mean_flow_duration", "RDP_mean_flow_duration", "RTMP_mean_flow_duration", 
                                     "RX_mean_flow_duration", "sFlow_mean_flow_duration", "Signal_mean_flow_duration", "Sina(Weibo)_mean_flow_duration", "SIP_mean_flow_duration", "Skype_mean_flow_duration","SkypeCall", "Slack_mean_flow_duration", 
                                     "SMBv1_mean_flow_duration", "SMBv23_mean_flow_duration", "SMTP_mean_flow_duration", "Snapchat_mean_flow_duration", "SNMP_mean_flow_duration", "SOCKS_mean_flow_duration", "Spotify_mean_flow_duration", "SSDP_mean_flow_duration", 
                                     "SSH_mean_flow_duration", "Starcraft_mean_flow_duration", "Steam_mean_flow_duration", "STUN_mean_flow_duration", "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "Teredo_mean_flow_duration", 
                                     "TikTok_mean_flow_duration", "TLS_mean_flow_duration", "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unencrypted_Jabber","Unknown_mean_flow_duration", "Viber_mean_flow_duration", "VNC_mean_flow_duration", 
                                     "Webex_mean_flow_duration", "WhatsApp_mean_flow_duration", "WhatsAppFiles_mean_flow_duration", "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")

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

# # Set the column names in their order - 105 applications (without CSGO and WeChat)
# colnames(mean_flow_duration.df) <- c("104_mean_flow_duration", "AJP_mean_flow_duration", "Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration",
#                                      "AppleStore_mean_flow_duration", "BitTorrent_mean_flow_duration", "CiscoSkinny_mean_flow_duration", "CiscoVPN_mean_flow_duration", "Citrix_mean_flow_duration", "Cloudflare_mean_flow_duration", "CNN_mean_flow_duration", "DataSaver_mean_flow_duration",
#                                      "DHCP_mean_flow_duration", "DNS_mean_flow_duration", "DNSoverHTTPS_mean_flow_duration", "Dropbox_mean_flow_duration", "eBay_mean_flow_duration", "Facebook_mean_flow_duration", "FTP_DATA_mean_flow_duration", "Github_mean_flow_duration",          
#                                      "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration",  
#                                      "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "IAX_mean_flow_duration", "ICMP_mean_flow_duration", "IMAPS_mean_flow_duration", "IMO_mean_flow_duration", "IPsec_mean_flow_duration", "Instagram_mean_flow_duration",       
#                                      "LDAP_mean_flow_duration", "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "Microsoft_mean_flow_duration", "MQTT_mean_flow_duration", "MS_OneDrive_mean_flow_duration", "MsSQL-TDS_mean_flow_duration", "NetBIOS_mean_flow_duration",         
#                                      "NetFlix_mean_flow_duration", "NTP_mean_flow_duration", "Office365_mean_flow_duration", "Ookla_mean_flow_duration", "OpenDNS_mean_flow_duration", "Oracle_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration",       
#                                      "PostgreSQL_mean_flow_duration", "PS_VUE_mean_flow_duration", "QQ_mean_flow_duration", "QUIC_mean_flow_duration", "Radius_mean_flow_duration", "RDP_mean_flow_duration", "RTMP_mean_flow_duration", "RX_mean_flow_duration",              
#                                      "sFlow_mean_flow_duration", "Signal_mean_flow_duration", "Sina(Weibo)_mean_flow_duration", "SIP_mean_flow_duration", "Skype_mean_flow_duration", "Slack_mean_flow_duration", "SMBv1_mean_flow_duration", "SMBv23_mean_flow_duration",          
#                                      "SMTP_mean_flow_duration", "Snapchat_mean_flow_duration", "SNMP_mean_flow_duration", "SOCKS_mean_flow_duration", "Spotify_mean_flow_duration", "SSDP_mean_flow_duration", "SSH_mean_flow_duration", "Starcraft_mean_flow_duration",       
#                                      "Steam_mean_flow_duration", "STUN_mean_flow_duration", "Syslog_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "Teredo_mean_flow_duration", "TikTok_mean_flow_duration", "TLS_mean_flow_duration",             
#                                      "Twitter_mean_flow_duration", "UbuntuONE_mean_flow_duration", "Unknown_mean_flow_duration", "Viber_mean_flow_duration", "VNC_mean_flow_duration", "Webex_mean_flow_duration", "WhatsApp_mean_flow_duration", "WhatsAppFiles_mean_flow_duration",   
#                                      "Wikipedia_mean_flow_duration", "WindowsUpdate_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")
# 
# # Set the column names in their order - OTT Applications only (without CSGO and WeChat)
# colnames(mean_flow_duration.df) <- c("Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration",
#                              "AppleStore_mean_flow_duration", "BitTorrent_mean_flow_duration", "CNN_mean_flow_duration", "DataSaver_mean_flow_duration",
#                              "Dropbox_mean_flow_duration", "eBay_mean_flow_duration", "Facebook_mean_flow_duration", "Github_mean_flow_duration",          
#                              "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration",  
#                              "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "IMO_mean_flow_duration", "Instagram_mean_flow_duration",       
#                              "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "MS_OneDrive_mean_flow_duration",         
#                              "NetFlix_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration",       
#                              "Skype_mean_flow_duration", "Slack_mean_flow_duration", "Snapchat_mean_flow_duration", "Spotify_mean_flow_duration", "Starcraft_mean_flow_duration",       
#                              "Steam_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "TikTok_mean_flow_duration",             
#                              "Twitter_mean_flow_duration", "Viber_mean_flow_duration", "WhatsApp_mean_flow_duration", "WhatsAppFiles_mean_flow_duration",   
#                              "Wikipedia_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")
# 
# # Set the column names in their order - OTT Applications only (with CSGO and WeChat)
# colnames(mean_flow_duration.df) <- c("Amazon_mean_flow_duration", "AmazonVideo_mean_flow_duration", "Apple_mean_flow_duration", "AppleiCloud_mean_flow_duration", "AppleiTunes_mean_flow_duration", "ApplePush_mean_flow_duration",
#                                      "AppleStore_mean_flow_duration", "BitTorrent_mean_flow_duration", "CNN_mean_flow_duration", "CSGO_mean_flow_duration","DataSaver_mean_flow_duration",
#                                      "Dropbox_mean_flow_duration", "eBay_mean_flow_duration", "Facebook_mean_flow_duration", "Github_mean_flow_duration",          
#                                      "GMail_mean_flow_duration", "Google_mean_flow_duration", "GoogleDocs_mean_flow_duration", "GoogleDrive_mean_flow_duration", "GoogleHangoutDuo_mean_flow_duration", "GoogleMaps_mean_flow_duration", "GooglePlus_mean_flow_duration", "GoogleServices_mean_flow_duration",  
#                                      "HTTP_mean_flow_duration", "HTTP_Proxy_mean_flow_duration", "IMO_mean_flow_duration", "Instagram_mean_flow_duration",       
#                                      "LinkedIn_mean_flow_duration", "Messenger_mean_flow_duration", "MS_OneDrive_mean_flow_duration",         
#                                      "NetFlix_mean_flow_duration", "Playstation_mean_flow_duration", "PlayStore_mean_flow_duration",       
#                                      "Skype_mean_flow_duration", "Slack_mean_flow_duration", "Snapchat_mean_flow_duration", "Spotify_mean_flow_duration", "Starcraft_mean_flow_duration",       
#                                      "Steam_mean_flow_duration", "TeamViewer_mean_flow_duration", "Telegram_mean_flow_duration", "TikTok_mean_flow_duration",             
#                                      "Twitter_mean_flow_duration", "Viber_mean_flow_duration", "WeChat_mean_flow_duration", "WhatsApp_mean_flow_duration", "WhatsAppFiles_mean_flow_duration",   
#                                      "Wikipedia_mean_flow_duration", "Xbox_mean_flow_duration", "Yahoo_mean_flow_duration", "YouTube_mean_flow_duration")
#-------------------------------------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------JOIN COLUMNS AND GENERATE THE FINAL DATASET ------------------------------------------------------------------------------------
final_dataset <- cbind(flows_per_app, mean_octet_total_count.df)
final_dataset <- cbind(final_dataset, mean_flow_duration.df)

# Convert src ip adresses from network format to decimal format
final_dataset[, c("src_ip_numeric")] <- unique(df[, c("src_ip_numeric")])

# Order the rows in ascending order based on the value of the src ip decimal number
final_dataset <- final_dataset[order(final_dataset[,c("src_ip_numeric")]),]

# Reorder the columns so the decimal form of the src ip is first - with 107 apps
final_dataset <- subset(final_dataset, select=c(299,1:298))

# Reorder the columns so the decimal form of the src ip is first - with 49 apps
# final_dataset <- subset(final_dataset, select=c(149,1:148))


write.csv(final_dataset, "/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Users-behavior-dataset-22-04-2019-2pm-to-4_30pm.csv", row.names = FALSE, quote = FALSE, sep=",")
#---------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------OBTAINING THE BEST NUMBER OF CLUSTERS--------------------------------------------
rm(dataset)
rm(df)
rm(flows_per_app)
rm(labels)
rm(mean_octet_total_count.df)
rm(mean_flow_duration.df)
gc()

test_data <- read.csv("/home/jsrojas/Juan/Unicauca/Doctorado/PhD Internship/Labeled Dataset with FlowLabeler/Comparison of different days in the same time interval/22-04-2019-2pm_4_30pm/Users-behavior-dataset-22-04-2019-2pm-to-4_30pm.csv")

final_dataset$src_ip <- NULL
test_data$src_ip <- NULL
df_clusters <- scale(test_data)
rm(df_clusters_noNA)

# Elbow method . Hierarchical Clustering
fviz_nbclust(test_data, hcut, method = "wss", k.max = 20) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Hierarchical clustering - 22-04-2019-2pm-to-4_30pm_OTT-Apps-Only")

# Elbow method - Kmeans
fviz_nbclust(test_data, kmeans, method = "wss", k.max = 20) +
  #geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method - Kmeans - 22-04-2019-2pm-to-4_30pm_OTT-Apps-Only")

# Silhouette method - Kmeans
fviz_nbclust(test_data, kmeans, method = "silhouette", k.max = 20) +
  labs(subtitle = "Silhouette method - Kmeans - 22-04-2019-2pm-to-4_30pm_OTT-Apps-Only")

# Silhouette method - Hierarchichal Clustering 
fviz_nbclust(test_data, hcut, method = "silhouette", k.max = 20)+
  labs(subtitle = "Silhouette method - Hierarchical Clustering - 22-04-2019-2pm-to-4_30pm_OTT-Apps-Only")

# Gap statistic method - Kmeans
set.seed(123)
fviz_nbclust(final_dataset, kmeans, nstart = 25,  method = "gap_stat", k.max = 10, nboot = 50)+
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

