library(dplyr)
library(ggplot2)
sleep3 = msleep %>% select(-c(name, genus, conservation, awake)) %>% filter(bodywt >= 0.1 & bodywt <= 200)
sleep3 = sleep3 %>% mutate(rem_ratio = sleep_rem/sleep_total)
byvore = group_by(sleep3, vore)
req_stats = summarise(byvore, count = n(), avg_rem_ratio = mean(rem_ratio, na.rm = TRUE), avg_bodywt = mean(bodywt, na.rm = TRUE))
#Note that for qns 3,4,5, we will ignore NA.
#Insectivore has the highest average rem ratio and herbivore has the least rem ratio. Carnivore has the highest average body weight (54.36724) and herbivore has the least average body weight (14.10886).
#Number of insectivore in sleep3 is 2, which is insufficient to make useful any conclusion about insectivore. Herbivore has the greatest number of mammals (22).
#Range of average rem ratio is 0.1980188 (which is 0.3370166-0.1389978), range of average body weight is 40.25838 (which is 54.36724 - 14.10886).
ggplot(sleep3, aes(x = vore, y = rem_ratio)) + geom_boxplot()
#Even though insecti has the highest rem_ratio among the 4 vore groups, there is only 1 valid data point, which is a inaccurate representation for insectivore.
#Carnivore has the highest median rem_ratio, while herbivore has the lowest median rem_ratio.
#Omnivore has the highest Interquartile Range(IQR) while herbivore has the lowest IQR.
#Omnivore has the largest range
#Carnivore is left skewed as the median is closer to the upper quartile.
#Herbivore is right skewed as the median is closer to the lower quartile.
#Omnivore is right skewed as the median is closer to the lower quartile.
ggplot(sleep3, aes(x = bodywt, y = rem_ratio)) + geom_point()
#The points are scattered.
#The rem_ratio and body weight are not much related.
#Most of the body weight values are similar and do not have huge difference in value, but their corresponding rem ratio value differ by large amounts.
#If they are related, points should have best fit line or curve. However, the points in the graph does not show the kind of relationship.
#They form a almost straight vertical line, with a number of points that are far from the line and are randomly scattered. This shows that the rem ratio and body weight are not much related.
#There are many points with body weight being small and very different rem_ratio.