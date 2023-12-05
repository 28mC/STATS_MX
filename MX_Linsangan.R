#QUESTION1
print("The null hypothesis (H0) posits that there is no difference in the mean time spent,
while the alternative hypothesis (H1) suggests a significant difference.")

#DATA
males <- c(12, 7, 7, 10, 8, 10, 11, 9, 9, 13, 4, 9, 12, 11, 9, 9, 7, 12, 10, 13,
                  11, 10, 6, 12, 11, 9, 10, 12, 8, 9, 13, 10, 9, 7, 10, 7, 10, 8, 11, 10,
                  11, 7, 15, 8, 9, 9, 11, 13, 13, 10)

females <- c(11, 10, 11, 10, 11, 12, 12, 10, 9, 9, 9, 10, 8, 7, 12, 9, 7, 8, 9, 8,
                    7, 7, 9, 9, 12, 10, 9, 13, 9, 9, 10, 9, 6, 12, 8, 11, 8, 8, 11, 12, 9,
                    10, 11, 14, 12, 7, 11, 10, 9, 11)
#QUESTION2
#The p-value obtained from the t-test will help in deciding whether to reject the null hypothesis.

result <- t.test(males, females, alternative = "two.sided")
print(result)

#HypothesisTest
alpha <- 0.05
if (result$p.value < alpha) {
  print("Reject the null hypothesis. There is a significant difference.")
} else {
  print("Fail to reject the null hypothesis. No significant difference.")
}

#QUESTION3
data <- data.frame(Males = males, Females = females)

#DescriptiveStats
summary(data)

#QUESTION4
#95%ConfidenceIntervals
conf_int_male <- t.test(males)$conf.int
conf_int_female <- t.test(females)$conf.int

#95%ConfidenceIntervalDifference
conf_int_difference <- t.test(males, females)$conf.int

print("95% Confidence Interval Mean for Males:")
print(conf_int_male)

print("95% Confidence Interval Mean for Females:")
print(conf_int_female)

print("95% Confidence Interval for the Difference between Means:")
print(conf_int_difference)

#QUESTION5
print("In terms of talking about the effect size of the time
spent on cell phones, if it's small, then larger samples will be
required for the difference to be detected.")

#QUESTION6
#AssumptionTest
shapiro.test(males)
print("With a p-value higher than the standard significance limit
of 0.05, we lack sufficient evidence to rule out the null hypothesis
of normality. For the group of males, the assumption of normality seems reasonable.")

shapiro.test(females)
print("The same to the group of males, the p-value for the females also has a value
greater than 0.05, indicating that the assumption of normality is valid.")

var.test(males, females)
print("When the p-value is greater than 0.05, the null hypothesis is not ruled out,
indicating that there is no significant variance difference between the two groups.
There is validity for the uniformity of variances assumption.")

#GITHUBLINK
print("https://github.com/28mC/MX.git")