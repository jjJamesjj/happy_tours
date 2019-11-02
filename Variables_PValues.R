library(dplyr)
library(readr)
tour.final <- read_csv("C:/James Laptop/M.S. Applied Statistics/MAT 8480 Data Mining and Analytics - Dr. Zhang/Project/tour.final.csv")
tour.final <- mutate_if(tour.final, is.character, as.factor)
tour.final$TourMonth <- as.factor(tour.final$TourMonth)
tour.final$TourWeek <- as.factor(tour.final$TourWeek)
tour.final$TourYear <- as.factor(tour.final$TourYear)
tour.final$Book_12Mo <- as.factor(tour.final$Book_12Mo)

# Create index to separate numerical and categorical variables
cate.indx <- sapply(tour.final, is.factor)
nume.indx <- sapply(tour.final, is.numeric)

# Create separate dataset for the tests. 
# Chi-squared test for categorical variables
# T test for numerical variables
tour.final.cate <- tour.final[cate.indx]
tour.final.nume <- cbind(tour.final[nume.indx],tour.final["Book_12Mo"])
tour.final.nume.no.factor <- cbind(tour.final[nume.indx])

# Obtain p-values from tests and combine together into 1 vector
cate.pval <- sapply(tour.final.cate, function(a) chisq.test(x=a, y=tour.final$Book_12Mo)$p.value)
nume.pval <- sapply(tour.final.nume.no.factor, function(a) t.test(a~Book_12Mo, data=tour.final.nume)$p.value)
pvalues <- c(cate.pval, nume.pval)

# Insignificant variables from test
pvalues[pvalues>=0.05]

# Most significant variables (lowest p-values)
head(sort(pvalues, decreasing=F), 21)
