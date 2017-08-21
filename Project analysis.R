
df <- read.table("C:/Study Material/Marketing Research/Project/data.csv" , 
                 header = TRUE, sep = ",", fill = TRUE)
summary(df)
df <- apply(df,2, as.numeric)
df1 <- as.data.frame(df)
df1

#################### Service provider and Happiness
h1 <- df1[,c("Q29_1","Q11")]   


h1att <- h1[h1$Q11 == 0,"Q29_1"]
h1cricket <- h1[h1$Q11 == 4,"Q29_1"]
h1verizon <- h1[h1$Q11 == 3,"Q29_1"]
h1tmobile <- h1[h1$Q11 == 2,"Q29_1"]
h1sprint <- h1[h1$Q11 == 1,"Q29_1"]

mean(h1cricket)
mean(h1verizon)
mean(h1tmobile)
mean(h1sprint)
#t1 <- table(h1$Q29_1,h1$Q11)
#t1
####### att&t and criket
t.test(h1att,h1cricket)
####### att&t and sprint
t.test(h1att,h1sprint)
####### att and verizon 
t.test(h1att,h1verizon)
####### att and t mobile
t.test(h1att,h1tmobile)


h1ano <- aov(Q29_1 ~ Q11, data=h1)
summary(h1ano)

#############################################################################
####h2 service provider and frustation

h2 <- as.data.frame(df1[,c("Q29_5","Q11")])   
#h2 <- apply(h02,2, as.numeric)

h2att <- h2[h2$Q11 == 0,"Q29_5"]
h2cricket <- h2[h2$Q11 == 4,"Q29_5"]
h2verizon <- h2[h2$Q11 == 3,"Q29_5"]
h2tmobile <- h2[h2$Q11 == 2,"Q29_5"]
h2sprint <- h2[h2$Q11 == 1,"Q29_5"]

h2tmobile
mean(h2cricket,na.rm = TRUE)
mean(h2verizon)
mean(h2tmobile)
mean(h2sprint)
#t1 <- table(h1$Q29_1,h1$Q11)
#t1
####### att&t and criket
t.test(h2att,h2cricket)
####### att&t and sprint
t.test(h2att,h2sprint)
####### att and verizon 
t.test(h2att,h2verizon)
####### att and t mobile
t.test(h2att,h2tmobile)


h2ano <- aov(Q29_5 ~ Q11, data=h2)
summary(h2ano)


###############################################################
##### h3 Age group and freedom restriction
h3 <- as.data.frame(df1[,c("Q28_4","Q9")]) 
summary(h3)
h3_18 <- h3[h3$Q9 == 0,"Q28_4"]
h3_25 <- h3[h3$Q9 == 1,"Q28_4"]
h3_35 <- h3[h3$Q9 == 2,"Q28_4"]

t.test(h3_18,h3_25)
t.test(h3_18,h3_35)
t.test(h3_25,h3_35)

h3ano <- aov(Q28_4 ~ Q9, data=h3)
summary(h3ano)

#################################################################
###### h4 Age group and connected and disconnected
################################################################################
h4 <- as.data.frame(df1[,c("Q28_3","Q9")]) 
summary(h4)
h4_18 <- h4[h4$Q9 == 0,"Q28_3"]
h4_25 <- h4[h4$Q9 == 1,"Q28_3"]
h4_35 <- h4[h4$Q9 == 2,"Q28_3"]

h4_25

t.test(h3_18,h3_25)
t.test(h3_18,h3_35)
t.test(h3_25,h3_35)

h4ano <- aov(Q28_4 ~ Q9, data=h3)
summary(h4ano)


#######################################################
######## h5 income and monthly bill

h5 <- as.data.frame(df1[,c("Q13","Q6")])
h5
h5$Q6a <- ifelse(h5$Q6 <= 1, '25,000 or less','more than 25,000 ' )
h5$Q13a <- ifelse(h5$Q13 == 0,'Less than 50','More than 50')


tb5 <-table(h5$Q13a,h5$Q6a)
tb5
#tb5_1 <- tb5[c('0','2'),]
#tb5_1
chisq.test(tb5)

#h1$Q6a <- ifelse(h1$Q6 == '$20,000 to $34,999', 'A', ifelse(h1$Q6 == '$35,000 to $49,999', 'A',ifelse(h1$Q6 == '$50,000 to $74,999','A',"B")))
##t1_1 <- table(h1$Q29_1,h1$Q6a)
#t1_1
#chisq.test(t1_1)
#############################################################
##### service provider and monthly bill 

h6 <- as.data.frame(df1[,c("Q11","Q13")])
h6
h6$Q13a <- ifelse(h6$Q13 == 0,'Less than 50','More than 50')
tb6 <-table(h6$Q13a,h6$Q11)
tb6
tb6_1 <- tb6[,c('0','1')]
tb6_1
chisq.test(tb6_1)


##################################################################
#######Helpful harmful, service provider

h7 <- as.data.frame(df1[,c("Q28_2","Q11")])   
#h2 <- apply(h02,2, as.numeric)

h7att <- h7[h2$Q11 == 0,"Q28_2"]
h7cricket <- h7[h2$Q11 == 4,"Q28_2"]
h7verizon <- h7[h2$Q11 == 3,"Q28_2"]
h7tmobile <- h7[h2$Q11 == 2,"Q28_2"]
h7sprint <- h7[h2$Q11 == 1,"Q28_2"]

h7tmobile

####### att&t and criket
t.test(h2att,h2cricket)
####### att&t and sprint
t.test(h2att,h2sprint)
####### att and verizon 
t.test(h2att,h2verizon)
####### att and t mobile
t.test(h2att,h2tmobile)


h7ano <- aov(Q28_2 ~ Q11, data=h)
summary(h7ano)
######################################################################
############# Age and social media
h8 <- as.data.frame(df1[,c("Q28_3","Q9")]) 
summary(h8)
#h4_18 <- h4[h4$Q9 == 0,"Q28_3"]
#h4_25 <- h4[h4$Q9 == 1,"Q28_3"]
#h4_35 <- h4[h4$Q9 == 2,"Q28_3"]

h8$Q9a <- ifelse(h8$Q9 <1,'Less than 25', 'more than 25')
tb8 <- table(h8$Q9a,h8$Q28_3)
tb8_1 <- tb8[,c("1","2")]
tb8_1
chisq.test(tb8_1)
#####################################################################s
####gender, do you wake up at night
h9 <- as.data.frame(df1[,c("Q1","Q23")])
h9                    
tb9 <- table(h9$Q1,h9$Q23)
tb9
h9$Q23a <- ifelse(h9$Q23 == 0,'A',ifelse(h9$Q23 == 1 ,'A','B'))
h9
tb9_1 <- table(h9$Q1,h9$Q23a)
tb9_1
chisq.test(tb9_1)
################################################################
######gender , exceeding data
h10 <- as.data.frame(df1[,c("Q1","Q14")])
h10                    
tb10 <- table(h10$Q1,h10$Q14)
tb10
chisq.test()
h10$Q14a <- ifelse(h10$Q14 == 3,'Never Exceeded','Exceeded atleast once')
h10
tb10_1 <- table(h10$Q1,h10$Q14a)
tb10_1
chisq.test(tb10_1)
