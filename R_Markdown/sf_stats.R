
#Do I have access to input from a similar path?
source('~/Desktop/Signal_Redshift_Conn.R')
#Write your source file

#Packages
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages('rsconnect')
#install.packages('styler')

library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(lubridate)
library(rlang)

#sf <- read.csv("/Users/tonye.cole/Desktop/sf_cases_support.csv", stringsAsFactors=FALSE)

qry <- "

SELECT
	*
FROM
	analytics.current_sf_cases
WHERE
	created_at >= CURRENT_DATE - interval '3 months'
	AND(case_owner_email = 'ewa.gierlowska@signal-ai.com'
		OR case_owner_email = 'stephani.pineda@signal-ai.com'
		OR case_owner_email = 'alexandra.dulama@signal-ai.com'
		OR case_owner_email = 'casey.jenson@signal-ai.com'
		OR case_owner_email = 'michael.taiwo@signal-ai.com'
		OR case_owner_email = 'callum.boyce@signal-ai.com'
		OR case_owner_email = 'tonye.cole@signal-ai.com'
		OR case_owner_email = 'tina.zeng@signal-ai.com'
		OR case_owner_email = 'mabel.oreilly@signal-ai.com'
		OR case_owner_email = 'chris.bonet@signal-ai.com'
		OR case_owner_email = 'chrystal.wong@signal-ai.com'
		OR case_owner_email = 'ben.jones@signal-ai.com')
ORDER BY
	created_at DESC

" 
sf <- dbGetQuery(pconn_r, qry)
#Is this DBT?


d <- sf %>%
  select(origin, minutes_open, issue_type, issue_detail, is_escalated, escalation_details, created_at, updated_at, case_owner_email) %>%
  mutate(
    created_at_date = as.POSIXct(created_at, format = "%d/%m/%Y %H:%M"),
    week_number = lubridate::week(created_at_date)
  )

issue_type_filter <- (sf %>% count(issue_type, sort = TRUE) %>% pull(issue_type))[1:20]
# RECALL THAT MINUTES OPEN IS BROKEN at this point (data will be fixed by 2022-03-10)
d <- d %>% filter(issue_type %in% issue_type_filter, issue_type != "", minutes_open < 5) %>% as.tbl()

#Trying to convert dates to buckets seperated by weeks & months
  #filter(created_at >= as.POSIXct('2022-02-01'))
#All sf_cases have is_escalated as false. Do we know if it's ever been escalated? 
#Is minutes_open actually minutes or is it in hours? 

#convert created at into buckets

p <- ggplot(data = d, aes(x = case_owner_email, y = minutes_open)) + geom_boxplot()

ggplot(data = d %>% 
         filter(issue_type %in% c('Account Edits', 'Dashboards', 'Source check', 'User Management')), aes(x = case_owner_email, y = minutes_open)) + 
  geom_boxplot() + facet_wrap(~ issue_type, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

