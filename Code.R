library(readxl)

data<-read_excel(file.choose(),na="NA")
head(data)

names(data)
table(data$Bounces)
table(data$Exits)
table(data$Continent)
table(data$Sourcegroup)
head(table(data$Timeinpage))
table(data$Uniquepageviews)
table(data$Visits)
summary(data)




#As mentioned earlier, a unique page view represents the number of sessions during which that page
#was viewed one or more times. A visit counts all instances, no matter how many times the same visitor
#may have been to your site. So the team needs to know whether the unique page view value depends on visits.

# Ho: mu(uniquePageView) = mu(visits)
# ha = mu(uniquePageView) != mu(visits)

uniquePage<-data$Uniquepageviews
visit1<-data$Visits
t.test(uniquePage,visit1)
# since , p <0.05 . Therefore, unique page view doesn't depend on visits.





#Find out the probable factors from the dataset, which could affect the exits. Exit Page Analysis is usually 
#required to get an idea about why a user leaves the website for a session and moves on to another one. 
#Please keep in mind that exits should not be confused with bounces.


fit<-lm(data$Exits ~ data$Bounces+data$Continent+data$Sourcegroup+data$Timeinpage+data$Uniquepageviews+data$Visits, data=data)
summary(fit)

#Now since some rows of this categorical variable - Source group are significant and
#some are not- to confirm the overall dependence,we run anova:
  
a<-aov(data$Exits ~ as.factor(data$Sourcegroup),data=data)
summary(a)

a1<-aov(data$Exits ~ as.factor(data$Continent),data=data)
summary(a1)

# In case of Source group , p<0.05. So, we can conclude that Source groups are having impact on exits.
# But in case of continents , p>0.05 . So, continent doesn't have effect on exits.



#Every site wants to increase the time on page for a visitor. This increases the chances of the visitor
#understanding the site content better and hence there are more chances of a transaction taking place.
#Find the variables which possibly have an effect on the time on page.

fit<-lm(data$Timeinpage ~ data$Bounces+data$Exits+data$Uniquepageviews+data$Visits, data=data)
summary(fit)

a<-aov(data$Timeinpage ~ as.factor(data$Continent),data=data)
summary(a)

a<-aov(data$Timeinpage ~ as.factor(data$Sourcegroup),data=data)
summary(a)

#A high bounce rate is a cause of alarm for websites which depend on visitor engagement. 
#Help the team in determining the factors that are impacting the bounce.

fit<-lm(data$Bounces ~ data$Timeinpage+data$Exits+data$Uniquepageviews+data$Visits, data=data)
summary(fit)
 

a<-aov(data$Bounces ~ as.factor(data$Continent),data=data)
summary(a)
a<-aov(data$Bounces ~ as.factor(data$Sourcegroup),data=data)
summary(a)


