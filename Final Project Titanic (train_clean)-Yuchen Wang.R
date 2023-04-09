#packages and the file
packages <- c("readxl", "stargazer", "microbenchmark") 
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i])
    }
    library(packages[i], character.only = TRUE)
}
train_clean <- read_excel(file.choose(train_clean))
train_clean <- as.data.frame(train_clean)
str(train_clean)

#summary of the data
colnames(train_clean)
View(head(train_clean, n=5))
View(head(train_clean, n = 5))
dim(train_clean)
nrow(train_clean)
ncol(train_clean)
summary(train_clean)

#stargazer
install.packages("stargazer")
library(stargazer)
stargazer(train_clean, type = "html", summary = TRUE, title = "Descriptive statistics", digits = 2, out = "summary.html")
summary(train_clean)

#compare variables “survived” and “age”
View(train_clean[c("Survived", "Age")])
identical(train_clean[c("Survived", "Age")], train_clean[, c("Survived", "Age")])

#respectively demonstrate the survived passengers with ages in different classes
#3rd class = 119 of 491
View(train_clean[train_clean$Survived > 0 & train_clean$Pclass == 3, c("Pclass", "Survived")]) 
View(train_clean[train_clean$Pclass == 3, c("Pclass", "Survived")])
#2nd class = 87 of 707
View(train_clean[train_clean$Survived > 0 & train_clean$Pclass == 2, c("Age", "Pclass", "Survived")]) 
View(train_clean[train_clean$Pclass == 2, c("Age", "Pclass", "Survived")]) 
#1st class = 136 of 216
View(train_clean[train_clean$Survived > 0 & train_clean$Pclass == 1, c("Age", "Pclass", "Survived")]) 
View(train_clean[train_clean$Pclass == 1, c("Age", "Pclass", "Survived")])

#Survived male passengers = 109 of 577
View(train_clean[train_clean$Survived > 0 & train_clean$Sex == "male", c("Name","Sex", "Survived")]) 
View(train_clean[!train_clean$Sex == "male", c("Name","Sex", "Survived")])

#Survived female passengers = 233 of 314
View(train_clean[train_clean$Survived > 0 & train_clean$Sex == "female", c("Name","Sex", "Survived")]) 
View(train_clean[!train_clean$Sex == "female", c("Name","Sex", "Survived")])

#Female in 3rd class (144)
View(train_clean[train_clean$Pclass == 3 & train_clean$Sex == "female", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "female", c("Name", "Pclass", "Sex")])

#Male in 3rd class (347)
View(train_clean[train_clean$Pclass == 3 & train_clean$Sex == "male", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "male", c("Name", "Pclass", "Sex")])

#Female in 2nd class (76)
View(train_clean[train_clean$Pclass == 2 & train_clean$Sex == "female", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "female", c("Name", "Pclass", "Sex")])

#Male in 2nd class (108)
View(train_clean[train_clean$Pclass == 2 & train_clean$Sex == "male", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "male", c("Name", "Pclass", "Sex")])

#Female in 1st class (94)
View(train_clean[train_clean$Pclass == 1 & train_clean$Sex == "female", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "female", c("Name", "Pclass", "Sex")])

#Male in 1st class (122)
View(train_clean[train_clean$Pclass == 1 & train_clean$Sex == "male", c("Name","Pclass", "Sex")]) 
View(train_clean[!train_clean$Sex == "male", c("Name", "Pclass", "Sex")])

#data visualization
library(ggplot2)
str(train_clean)

#Convert the continuous variable "Survived" into categorical
pb <- train_clean 
pb$SurvivedClass <- cut(pb$Survived, breaks = c(-Inf, 0.5, Inf), labels = c("No","Yes"))
pb
#Line chart showing the distributions of passengers' age and fare in different sexes
bp <- ggplot(train_clean, aes(x=Age, y=Fare, group=Sex)) + geom_line(aes(color=Sex)) + scale_fill_discrete(name="Sex")
bp
#Line chart showing whether passengers at different age and with different fare survived 
pd <- ggplot(train_clean, aes(x=Age, y=Fare, group=pb$SurvivedClass)) + geom_line(aes(color=pb$SurvivedClass)) + scale_fill_discrete(name="Survived")
pd
#Scatter plot showing the distributions of passengers' age and fare in different sexes
qb <- ggplot(train_clean, aes(x=Age, y=Fare, group=Sex)) + geom_point(aes(color=Sex))
qb
#Scatter plot showing whether passengers at different age and with different fare survived 
qd <- ggplot(train_clean, aes(x=Age, y=Fare, group=pb$SurvivedClass)) + geom_point(aes(color=pb$SurvivedClass))
qd
