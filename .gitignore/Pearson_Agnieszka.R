dane <- read.csv("data/data2018.csv", head=TRUE, sep=";")
library(ggplot2)

summary(dane)

ggplot(dane, aes(x = country)) +
  geom_bar()


library(tidyverse)
in_course_summary<-dane %>% group_by(in_course) %>% summarise(N = n(), avg_score_total = mean(avg_score, na.rm=TRUE), complete_total = mean(completion, na.rm=TRUE), inv_total = mean(inv_rate, na.rm=TRUE))
unit_summary<-dane %>% group_by(unit) %>% summarise(N = n(), avg_score_total = mean(avg_score, na.rm=TRUE), complete_total = mean(completion, na.rm=TRUE), inv_total = mean(inv_rate, na.rm=TRUE))

ggplot(unit_summary, aes(x = unit, y=avg_score_total)) +
  geom_point()
ggplot(unit_summary, aes(x = unit, y=complete_total)) +
  geom_point()
ggplot(unit_summary, aes(x = unit, y=inv_total)) +
  geom_point()

high_av <- dane %>%
  mutate(high = avg_score > 0.8, na.rm=TRUE) 

high_com <- dane %>%
  mutate(highcom = completion > 0.8, na.rm=TRUE) 

high_order<- dane %>%
  mutate(highord = inv_rate==0, na.rm=TRUE) 


high_av %>%
  ggplot(aes(x = as.factor(in_course), fill= high)) + 
  geom_bar(position = "fill") +
  xlab("Teacher or without") + ylab("Proporcion")

high_av %>%
  ggplot(aes(x = as.factor(unit), fill= high)) + 
  geom_bar(position = "fill") +
  xlab("Unit") + ylab("Proporcion")



high_com %>%
  ggplot(aes(x = as.factor(in_course), fill= highcom)) + 
  geom_bar(position = "fill") +
  xlab("Teacher or without") + ylab("Proporcion")
 
high_com %>%
  ggplot(aes(x = as.factor(unit), fill= highcom)) + 
  geom_bar(position = "fill") +
  xlab("Unit") + ylab("Proporcion")

high_order %>%
  ggplot(aes(x = as.factor(in_course), fill= highord)) + 
  geom_bar(position = "fill") +
  xlab("Teacher or without") + ylab("Proporcion")

high_order %>%
  ggplot(aes(x = as.factor(unit), fill= highord)) + 
  geom_bar(position = "fill") +
  xlab("Unit") + ylab("Proporcion")




mod1 <- lm(avg_score ~ completion, data = dane)
summary(mod1) ni zależy

mod1 <- lm(avg_score ~ inv_rate, data = dane)
summary(mod1)

dane2<- dane[complete.cases(dane),]

cor(dane2[,c(5,6,7)])



TO NIE
mod12 <- lm(volume ~ hightemp + cloudcover + weekday, data = RailTrail)
mod13 <- lm(avg_score ~ . , data = dane)
summary(mod13)

mod2 <- lm( avg_score~unit, data = dane)
summary(mod2)

dane %>%
  ggplot(aes(x=unit, y=avg_score)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE)


