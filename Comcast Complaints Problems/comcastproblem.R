# libraries to be used for this project

library(dplyr) # mutate/arrange/group by
library(lubridate) # dmy
library(esquisse) # chart making
library(ggplot2) # ggplot

# import comcast data
Comcast.Telecom.Complaints.data<- read.csv(file.choose())

# convert char to date and also uniformly distribute the data
# check data type for na/missing values
Comcast.Telecom.Complaints.data$Date<- dmy(Comcast.Telecom.Complaints.data$Date)
head(Comcast.Telecom.Complaints.data)
sum(is.na(Comcast.Telecom.Complaints.data))
summary(Comcast.Telecom.Complaints.data)

# count number of complaints per month
df = Comcast.Telecom.Complaints.data %>% mutate(Month = month(Date))
temp1 = df %>% arrange(Month,by_group=T)
monthly_count = temp1 %>% count(Month)

# function to create chart
esquisse::esquisser()

# chart for monthly comcast complaints
ggplot(monthly_count) +
  aes(x = Month, y = n) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  scale_x_continuous(breaks = monthly_count$Month)+
  labs(x = "Months", y = "Number of Complaints", title = "Monthly complaints about Comcast Service") +
  theme_minimal()


# count number of complaints per day
temp2 = Comcast.Telecom.Complaints.data %>% arrange(Date,by_group=T)
daily_count = temp2 %>% count(Date)

# chart for daily comcast complaints
ggplot(daily_count) +
  aes(x = Date, y = n) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Days", y = "Number of complaints", title = "Daily trend of complaints about Comcast Service") +
  theme_minimal()

# create a new column called ComplaintType to distinguish the types of complaints made by a customer
# assign a variable that stores the row numbers containing that type of complaint
df1 = Comcast.Telecom.Complaints.data %>% mutate(ComplainType="Others")
internet = contains(Comcast.Telecom.Complaints.data$Customer.Complaint, match = "internet",ignore.case=TRUE)
bill = contains(Comcast.Telecom.Complaints.data$Customer.Complaint, match = "bill",ignore.case=TRUE)
service = contains(Comcast.Telecom.Complaints.data$Customer.Complaint, match = "service",ignore.case=TRUE)
price = contains(Comcast.Telecom.Complaints.data$Customer.Complaint, match = "price",ignore.case=TRUE)
network = contains(Comcast.Telecom.Complaints.data$Customer.Complaint, match = "network",ignore.case=TRUE)
 
# fill the column with respective complaint type
df1$ComplainType[internet] = "Internet"
df1$ComplainType[bill] = "Bill"
df1$ComplainType[service] = "Service"
df1$ComplainType[price] = "Price"
df1$ComplainType[network] = "Network"

# tabulate the frequency of the complaints
table(df1$ComplainType)

# create a new column called StatusType to determine whether the complaint issued had been resolved or not
df2 = Comcast.Telecom.Complaints.data %>% mutate(StatusType="Closed")

# assign row numbers into the variable
open = contains(Comcast.Telecom.Complaints.data$Status, match="open",ignore.case=T)
pending = contains(Comcast.Telecom.Complaints.data$Status, match="pending",ignore.case=T)

# fill in the new column with respective status
df2$StatusType[c(open,pending)]="Open"

# count the resolved cases versus the unresolved
count_states <- df %>% select(State,StatusType) %>% arrange(State,by_group=T)
temp <- count_states %>% count(State,StatusType)

# a bar chart showing for every state the number of open and 
# closed cases in a stacked manner
ggplot(temp) +
 aes(x = State, fill = StatusType, weight = n) +
 geom_bar() +
 scale_fill_hue() +
 labs(x = "States", y = "Number of cases", title = "Closed and open cases per states") +
 coord_flip() +
 theme_minimal()

# determination of the state with highest unresolved cased : Georgia - 80
df2 = temp %>% filter(StatusType=="Open")
df2 = df2 %>% mutate(Open = n)
df2[df2$n == max(df2$n),c(1,3)]

# summarise data based on the cases solved/unsolved and recieved via
resolved_data <- group_by(df,Received.Via,StatusType)
Cartegory_resolved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)))

# a pie chart illustrating data based on the cases solved/unsolved and recieved via
ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = StatusType))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

