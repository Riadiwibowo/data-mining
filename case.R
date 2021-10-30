#read .csv files
food <- read.csv("favorite_food_data.csv",
                 fileEncoding = "UTF-8-BOM")
student <- read.csv("student_data.csv",
                 fileEncoding = "UTF-8-BOM")

#filter for missing value in data set
food <- food[food$Student.Name != "",]
student <- student[student$Student != "",]
View(food)
View(student)

#merge the two filtered data set into 1 data set
data_food <- merge(food, student, 
                   by.x="Student.Name", by.y="Student")
View(data_food)


#number 1. visulization for favorite food in percentage
data_1 <- data_food$Favorite.Food
data_1 <- table(data_food$Favorite.Food)
labels = paste(floor(data_1/sum(data_1)*100), "%")
lbls <- paste(labels, names(data_1),sep=" ")
pie(data_1,
    main = "Food Type Appeal",
    col = rainbow(7),
    label = lbls
    )

#number 2.visualization for number of students by grade and education year

data_2 <- paste(student$Education.Grade, student$Education.Year)
data_2 <- table(data_2)
barplot(data_2,
        main = "Number of Students By Grade",
        xlab = "student grade",
        ylab = "student count",
        col = rainbow(12))


#number 3.visualization for Primary Student average height based on education year, and student age less than <= 12
student <- read.csv("student_data.csv",
                    fileEncoding = "UTF-8-BOM")
#filter data
data_3 <- student
data_3<- data_3[data_3$Education.Grade == "SD",]
data_3 <- data_3[data_3$Age <= 12,]
agr_data<- aggregate(data_3$Height ,list(data_3$Education.Year),FUN = mean)

plot(agr_data,
     type = "b",
     main = "Primary School Average Height",
     xlab = "Education Year", 
     ylab ="grades",
     col="orange")


#preprocess
#remove all data that we dont need
preprocess_data <- data_food[data_food$Student.Name != "",]
preprocess_data <- preprocess_data[preprocess_data$Education.Grade != "SD",]
preprocess_data <- preprocess_data[!duplicated(preprocess_data),]
View(preprocess_data)

#data transformation
apriori_data <- split(preprocess_data$Favorite.Food, preprocess_data$Student.Name)

#data mining
library(arules)
freq_itemset <- apriori(apriori_data, parameter = 
                          list(
                            support = 0.25,
                            target = "frequent itemsets"
                          ))
inspect(freq_itemset)


#rule induction
assoc_rule <- ruleInduction(freq_itemset, confidence = 0.6)
inspect(assoc_rule)