## analysis of Diabetes data for prediction
library(dplyr)
library(ggplot2)

### import and analyse
diabetecDF=read.csv("datasetdiabetic.csv", sep = ",", na.strings =c("",NA) , head=TRUE)
View(diabetecDF)
summary(diabetecDF)
glimpse(diabetecDF)

# data cleaning
any(is.na(diabetecDF))
df1=na.omit(diabetecDF) #remove all blank rows
#check the condition 0
df1%>%
  filter(glu==0)%>%count()
df1%>%
  filter(bp==0)%>%count()
df1%>%
  filter(skin==0)%>%count()
df1%>%
  filter(bmi==0)%>%count()
df1%>%
  filter(age==0)%>%count()

#replace 1 with positive
df2=df1%>%
  mutate(df1, type=
           ifelse(type==1,'positive','negetive'))
head(df2)

#count the number of text values
type1=df2%>%group_by(type)%>%count(type)
print(type1)

#correlation
h=select(df2, -(type))
#heatmap func
View(cor(h))
heatmap(cor(h),scale="column")

## ggplot heatmap
library(reshape2)
da<-cor(h[sapply(h,is.numeric)])
da1<-melt(da)
head(da)
ggplot(da1, aes(x = Var1,
                  y = Var2,
                  fill = value))+geom_tile()

#visualization of data
#graph1 - Type of Diabetes
ggplot(type1)+
  geom_col(mapping=aes(x=type, y=n, fill=type))+
  labs(title="Type of Diabetes", x="Test Result", y="No. of Patients")

#graph2 - Age vs Diabetes
ggplot(df2)+
  geom_boxplot(mapping=aes(x=type,y=age,fill=type))+
  labs(title="Age vs Diabetes", x="Age", y="No. of Patients")
  
#graph3 - glu vs type
ggplot(df2)+
  geom_point(mapping = aes(x=glu,y=age,col=type))+
  labs(title="Scatter Plot of Age vs Glucose", x="Glucose Count", y="Age")
