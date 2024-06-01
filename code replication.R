library(tidyverse)
library(ggeffects)
library(plm)
library(texreg)
library(lme4) 
library(eeptools)
library(plyr)
library(Rmisc)
library(dplyr)
library(doBy)
library(skimr)
library(sjPlot)

data=read.csv2("DATASET", sep=";", quote="\"") 
data<-as_tibble(data)
data<-data %>%
  filter(PropNA<0.50)
data$group <- factor(data$group, levels = c("A", "B")) 
data$group <- relevel(data$group, ref="A")
data$time <- as.factor(data$time) 
data$ban.n<- 0
data$ban.n[data$ban >=0.5] <- 1
DataTypes<-data 
DataTypes$association.n<-as.numeric(data$association) 
DataTypes$type<-DataTypes$donated*DataTypes$association.n
DataTypes$type<-as.factor(DataTypes$type)
levels(DataTypes$type) <-  c("neutral", "anti", "pro")

#            Above code part is part of Authors original replication package(14-27)           #

DataTypes$gender <- DataTypes$gender.f 
DataTypes$gender <- as.factor(DataTypes$gender)
DataTypes$edu <- recode(DataTypes$education,
                        "Abitur" = "High School",
                        "Weiterführende Schule nicht beendet" = "Primary",
                        "Bachelorabschluss" = "Bachelors",
                        "Berufliche Qualifikation" = "Vocational Qualification",
                        "Masterabschluss" = "Masters")
DataTypes$edu <- as.factor(DataTypes$edu)
DataTypes$image <- as.factor(DataTypes$image)
DataTypes$East <- as.factor(DataTypes$East)
W <- DataTypes %>%
  filter(East == "West")
E <- DataTypes %>% 
  filter(East == "East")

E2 <- E[, c("ID","score", "time", "type", "gender", "age", "group","edu","ban.n","image")]
E2 <- na.omit(E2)
E2$time <- as.factor(E2$time)  # Convert time to numeric
E2$type <- as.factor(E2$type)  # Convert type to factor
E2$gender <- as.factor(E2$gender)  # Convert gender.f to factor
E2$age <- as.factor(E2$age)  # Convert age to factor
E2$group <- as.factor(E2$group)  # Convert group to factor
#just making the EAST germany dataset#
ECOMB <-  E2 %>%
  select(image, group, gender, type, age, edu,time) %>%
  distinct()
W2 <- W[, c("ID","score", "time", "type", "gender", "age", "group", "edu", "ban.n","image")]
W2 <- na.omit(W2)
W2$time <- as.factor(W2$time)  # Convert time to numeric
W2$type <- as.factor(W2$type)  # Convert type to factor
W2$gender <- as.factor(W2$gender)  # Convert gender.f to factor
W2$age <- as.factor(W2$age)  # Convert age to factor
W2$group <- as.factor(W2$group)  # Convert group to factor
#just making the WEST germany dataset#
WCOMB <- W2 %>%
  select(image, group, gender, type, age, edu,time) %>%
  distinct()

#TIME#
eavgt <- aggregate(score ~ time + group+ gender+type, data = E2, FUN = mean)
eavgt$avgt <- eavgt$score
eavgt$score <- NULL
ECOMB <- merge(ECOMB, eavgt, by = c("time","gender","group", "type"), all.x = TRUE)

wavgt <- aggregate(score ~ time + group+ gender+type, data = W2, FUN = mean)
wavgt$avgt <- wavgt$score
wavgt$score <- NULL
WCOMB <- merge(WCOMB, wavgt, by = c("time","gender","group","type"), all.x = TRUE)

ggplot() +
  geom_point(data = WCOMB, aes(x = time, y = avgt, color =  "west", shape = gender)) +
  geom_point(data = ECOMB, aes(x = time, y = avgt, color = "east", shape = gender)) +
  geom_line(data = WCOMB, aes(x = time, y = avgt, color = "west", group = interaction(gender, type, group)), linetype = "solid") +
  geom_line(data = ECOMB, aes(x = time, y = avgt, color = "east", group = interaction(gender, type, group)), linetype = "solid") +
  labs(
    title = "Graph 1: Average Hate Score by Group, Gender, Time & Type",
    x = "Time",
    y = "Hate Score"
  ) +
  scale_color_manual(values = c("west" = "blue", "east" = "red")) +
  theme_minimal()+
  facet_grid(group~type)

#IMAGE#
eavgi <- aggregate(score ~ image + group+ gender+type, data = E2, FUN = mean)
eavgi$avgi <- eavgi$score
eavgi$score <- NULL
ECOMB <- merge(ECOMB, eavgi, by = c("image","gender","group", "type"), all.x = TRUE)

wavgi <- aggregate(score ~ image + group+ gender+type, data = W2, FUN = mean)
wavgi$avgi <- wavgi$score
wavgi$score <- NULL
WCOMB <- merge(WCOMB, wavgi, by = c("image","gender","group","type"), all.x = TRUE)

ggplot() +
  geom_point(data = WCOMB, aes(x = image, y = avgi, color =  "west", shape = gender)) +
  geom_point(data = ECOMB, aes(x = image, y = avgi, color = "east", shape = gender)) +
  geom_line(data = WCOMB, aes(x = image, y = avgi, color = "west", group = interaction(gender, type, group)), linetype = "solid") +
  geom_line(data = ECOMB, aes(x = image, y = avgi, color = "east", group = interaction(gender, type, group)), linetype = "solid") +
  labs(
    title = "Graph 2: Average Hate Score by Group, Gender, Image & Type",
    x = "Image",
    y = "Hate Score"
  ) +
  scale_color_manual(values = c("west" = "blue", "east" = "red")) +
  theme_minimal()+
  facet_grid(group~type)

Ban.exposed.increase <- lmer(ban.n ~ time*group+ (1|ID),data,REML=F)
model <- glm(ban.n ~ time * time * group, data = DataTypes, family = binomial)

AIC(model, Ban.exposed.increase)
BIC(model,Ban.exposed.increase)

bimg2 <- lmer(ban.n ~ image*group*time+(1|ID), data = DataTypes, REML = F)

mod2 <- glm(ban.n ~ image*group+time + (1|ID), data = DataTypes, family = binomial)
bimg <- lmer(ban.n ~ image*group+time+(1|ID), data = DataTypes, REML = F)
AIC(mod2, bimg)
BIC(mod2, bimg)
AIC(bimg, bimg2)
BIC(bimg, bimg2)

ggplot(E2, aes(x = time, y = score, color = group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(edu ~ type + age) +
  labs(title = "Faceted Plot: Score by Time, Group, and Other Predictors",
       x = "Time",
       y = "Score") +
  theme_minimal()

ggplot(W2, aes(x = time, y = score, color = group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_grid(edu ~ type + age) +
  labs(title = "Faceted Plot: Score by Time, Group, and Other Predictors",
       x = "Time",
       y = "Score") +
  theme_minimal()

DataTypes$pred <- predict(Ban.exposed.increase)
DataTypes %>%
  filter(!is.na(East)) %>%
  ggplot(aes(x = time, y = pred, colour = type)) +
  geom_point() +
  facet_grid(group ~ East) +
  labs(title = "Predicted Values by Time, Colored by Type, Faceted by Group and East",
       x = "Time",
       y = "Predicted Value") 

tab_model(bimg, title = "Table 1", dv.labels = "Dependent Variable: ban.n")

DataTypes$ipred <- predict(bimg)
DataTypes %>%
  filter(!is.na(East))%>%
  ggplot(aes(x = image, y = ipred, colour = type))+
  geom_point()+
  facet_grid(group~East)+
  labs(title = "Graph 3: Predicted Values by Image, Colored by Type, Faceted By Group & Location",
       x = "Time",
       y = "Predicted Value")