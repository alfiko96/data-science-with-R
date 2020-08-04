# import web data
library(readxl)
webdata <- read_xlsx(file.choose())

# 1. The team wants to analyze each variable of the data collected 
# through data summarization to get a basic understanding of the 
# dataset and to prepare for further analysis.
str(webdata)
summary(webdata)

# 2. As mentioned earlier, a unique page view represents the number of 
# sessions during which that page was viewed one or more times. A visit 
# counts all instances, no matter how many times the same visitor may 
# have been to your site. So the team needs to know whether the unique 
# page view value depends on visits.

cor(webdata$Uniquepageviews,webdata$Visits)
# 0.8144457 (The number of visits has a positive impact on the unique page views)
# -1 < cor(x,y) < 1

# 3. Find out the probable factors from the dataset, which could affect 
# exits. Exit Page Analysis is usually required to get an idea about why 
# a user leaves the website for a session and moves on to another one. 
# Please keep in mind that exits should not be confused with bounces.

aov?
  
model<-aov(Exits~.,data = webdata)
summary(model)

# 4. Every site wants to increase the time on page for a visitor. This 
# increases the chances of the visitor understanding the site content 
# better and hence there are more chances of a transaction taking place. 
# Find the variables which possibly have an effect on the time on page.

model_two <- aov(Timeinpage~.,data=webdata)
summary(model_two)

# 5. A high bounce rate is a cause of alarm for websites which depend on 
# visitor engagement. Help the team in determining the factors that are 
# impacting the bounce.

webdata$Bounces=webdata$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = webdata,family = "binomial")
summary(rmm)
