anth <- read.csv("G2_anthropometry.csv")
View(anth)
#Data cleaning

#1) Re-code the gender feature to Male and Female

anth$Sex[anth$gender=="F" ]="Female"
anth$Sex[anth$gender=="cm"]="Male"

#2) show all the rows that have NA

anth[! complete.cases(anth) , ]

#3)remove the text(cm)in height feature to convert it to numeric to use in the analysis

anth$height<-gsub(" cm" ,"" , anth$height)
anth$height<-as.numeric(anth$height)
str(anth)
summary(anth)

#4)Replace each NA in foot_length according to the mean of
#foot_length to all males for males and foot_length to all females for females

shapiro.test(anth$foot_length)

m_mean<-mean(anth[anth$Sex=="Male" , 'foot_length' ], na.rm=T)
m_mean
anth[is.na(anth$foot_length) & anth$Sex=="Male"  , 'foot_length']=m_mean

f_mean<-mean(anth[anth$Sex=="Female" , 'foot_length' ], na.rm=T)
anth[is.na(anth$foot_length) & anth$Sex=="Female"  , 'foot_length']=f_mean

#5)Re_code age variable 
anth$ageRange[anth$age <= 5]="0 _ 5"
anth$ageRange[anth$age > 5 & anth$age <= 10 ]="6 _ 10"
anth$ageRange[anth$age >10 ]="11 _ .."
anth

#6)Re_code the height feature using if else

x<-mean(anth$height)

anth$height_of_kid<-as.factor(ifelse(anth$height< x & anth$age >10,"Abnormal kid" 
                                   ,"Normal kid"))


#7)Re_code of code

anth$height_of_kid2[anth$height_of_kid=="Normal kid"]='0'
anth$height_of_kid2[anth$height_of_kid=="Abnormal kid"]='1'
anth$height_of_kid2<-as.factor(anth$height_of_kid2)
anth

#8)sort the data set ascending according to 2 variables

sorted<-anth[order(anth$age ,anth$height) , ]
anth

#9)Get only the first 30 rows
h<-head(anth ,20)
anth

#10)Get only the last 50 rows
t<-tail(anth ,40)
anth

#______________________________________________ now the data is ready to visualization

library(ggplot2)

#11)display the effect of the height on foot_length (co_relation) using scatter plot,name the figure


fig1<-ggplot(anth, aes(x=foot_length  , y= height))
fig1 + geom_point() + ggtitle("The co_relation between the Height and Foot_length")

#12)display the effect of height on foot_length colored by the groups of age range using scatterplot

fig2<-ggplot(anth , aes(foot_length , height))
fig2 + geom_point(aes(color=ageRange)) +stat_smooth(se=FALSE)  


#13)Show the distribution of footlength using histogram,name the figure and rename the x,y

fig3<-ggplot(anth , aes(foot_length))
fig3 + geom_histogram(binwidth = 8)
fig3 + geom_histogram(fill = "blue")+ ggtitle("Child's foot length distribution")+labs(x="Foot Length" , y="Frequency")

#14)Show the distribution of Height using histogram ,name the figure

fig4<-ggplot(anth , aes(height))
fig4 + geom_histogram(binwidth = 8)
fig4 + geom_histogram(fill = "red")+ ggtitle("Child's Height distribution") 

#15)summarize the heightcat2 0,1 to Sex and ageRange groups using Bar chart

fig5<-ggplot(anth, aes(x=height_of_kid2 ,fill= Sex))
fig5 +geom_bar()+labs(y=" Heightcat count" ,title="Height category rate")
fig5 +geom_bar() +theme_light()+facet_wrap(~ageRange)



