# Apriori

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('flatten_data.csv', header = FALSE)
dataset = dataset[1:200,2:41]
write.csv(dataset,'flatten_data.csv')

dataset = read.transactions('flatten_data.csv', sep = ',', rm.duplicates = FALSE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
support_percentage = 0.05
rules = apriori(data = dataset, parameter = list(support = support_percentage))

# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])
