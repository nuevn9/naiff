

#Naif albugami 
Complaints_data=read.csv("C:/Users/PC/Desktop/Comcast_data.csv")
View(Complaints_data)
head(Complaints_data)

summary(Complaints_data)
str(Complaints_data)

library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)

head(Complaints_data)
names(Complaints_data)<- stri_replace_all(regex = "\\.",replacement = "",str =names(Complaints_data))
names
head(Complaints_data)

na_vector <- is.na(Complaints_data)
na_vector
length(na_vector[na_vector==T])


Complaints_data$Date<- dmy(Complaints_data$Date)
head(Complaints_data)
monthly_count=arrange(summarise(group_by(Complaints_data,month=as.integer(month(Date))),Count=n()),month)
daily_count=summarise(group_by(Complaints_data,Date),Count=n())
monthly_count
daily_count
ggplot(data = monthly_count,aes(month,Count,label = Count))+geom_line()+geom_text()+scale_x_continuous(breaks = monthly_count$month)+labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")
ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+geom_line()+theme(axis.text.x = element_text(angle = 75))+scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")


network_tickets=contains(Complaints_data$CustomerComplaint,match='network',ignore.case = T)
internet_tickets=contains(Complaints_data$CustomerComplaint,match ='internet',ignore.case = T)
bill_tickets=contains(Complaints_data$CustomerComplaint,match='bill',ignore.case = T)
email_tickets=contains(Complaints_data$CustomerComplaint,match="email",ignore.case = T)
charge_tickets=contains(Complaints_data$CustomerComplaint,match='charge',ignore.case = T)
Complaints_data$ComplaintType[internet_tickets]='Internet'
Complaints_data$ComplaintType[bill_tickets]='Billing'
Complaints_data$ComplaintType[email_tickets]='Email'
Complaints_data$ComplaintType[charge_tickets]='Charges'
Complaints_data$ComplaintType[network_tickets]='Network'
Complaints_data$ComplaintType[- c(network_tickets,internet_tickets,bill_tickets,email_tickets,charge_tickets)]="Others"
View(Complaints_data)
table(Complaints_data$ComplaintType)


open_complaints=(Complaints_data$Status=='Open'|Complaints_data$Status=='Pending')
closed_complaints=(Complaints_data$Status=='Closed'|Complaints_data$Status=='Solved')
Complaints_data$ComplaintStatus[open_complaints]="Open"
Complaints_data$ComplaintStatus[closed_complaints]='Closed'


chart_data=summarize(group_by(Complaints_data,State,ComplaintStatus),Count=n())
chart_data
chart_data=as.data.frame(chart_data)
chart_data
ggplot(chart_data ,mapping = aes(State,Count))+geom_col(aes(fill = ComplaintStatus),width = 0.95)+theme(axis.text.x = element_text(angle = 90))+labs(title = "Ticket Status Stacked Bar Chart ",x = "States",y = "No of Tickets",fill= "Status")


max(chart_data$Count)
arrange(select(chart_data,State,Count),desc(Count))

arrange(filter(chart_data,ComplaintStatus=="Open"),desc(Count))


resolved=summarise(filter(Complaints_data,ComplaintStatus=='Closed'),count=n())
resolved
resolved_internet=summarise(filter(Complaints_data,ComplaintStatus=='Closed',ReceivedVia=='Internet'),count=n())
resolved_internet
resolved_CustomerCare=summarise(filter(Complaints_data,ComplaintStatus=='Closed',ReceivedVia=='Customer Care Call'),count=n())
resolved_CustomerCare
percentage_internet=(resolved_internet/resolved)*100
percentage_internet
percentage_CustomerCare=(resolved_CustomerCare/resolved)*100
percentage_CustomerCare
table_df=table(Complaints_data$ReceivedVia,Complaints_data$ComplaintStatus)
table_df
bar=ggplot(Complaints_data,aes(ComplaintStatus,fill=ReceivedVia))+geom_bar()
bar
resolved_df=select(filter(Complaints_data,ComplaintStatus=='Closed'),ComplaintStatus, ReceivedVia)
pie<- ggplot(resolved_df, aes(x="", y= ComplaintStatus,fill=ReceivedVia)) +geom_bar(width = 1, stat = "identity") +coord_polar("y")
pie
