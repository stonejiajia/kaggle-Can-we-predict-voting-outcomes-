setwd("/Users/stone20091652/GitHub/Data Science/kaggle Can we predict voting outcomes")

train = read.csv("train2016.csv")
str(train)

table(train$Gender, train$Party)

                                        #Male

1613/(1613+1712)

1275/(1275+855)

newdata = subset(train, train$Income == "under $25,000")

str(newdata)

table(newdata$Gender, newdata$Party)

166/(166+126)

234/(234+229)

table(train$HouseholdStatus)

newdata2 = subset(train, train$HouseholdStatus == "Single (w/kids)")
table(newdata2$Gender, newdata2$Party)

####################################################################################

Female = subset(train, train$Gender == "Female")

Male = subset(train, train$Gender == "Male")

table(Female$Party, Female$Q116197 == "A.M.")

table(Male$Party)

663/(663+192)

602/(602+253)


Q96024 = subset(train, train$Q96024 == "Yes")
table(Q96024$Party)

Q98059 = subset(train,train$Q98059 == "Yes")
table(Q98059$Party)

1684/(1683+1454)

Q98078 = subset(train, train$Q98078 == "Yes")
table(Q98078$Party)
766/(766+602)

Q98197 = subset(train, train$Q98197 == "Yes")
table(Q98197$Party)

755/(755+550)

