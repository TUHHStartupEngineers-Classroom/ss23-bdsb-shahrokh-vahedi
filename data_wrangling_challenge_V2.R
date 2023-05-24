library(vroom)
library(data.table)

col_types <- list(
  id = col_character(),
  type = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

# Load the patent data
patent_data <- vroom(
  file = "reduced_data/patent.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
patent_assignee_data <- vroom(
  file = "reduced_data/patent_assignee.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
assignee_data <- vroom(
  file = "reduced_data/assignee.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
uspc_data <- vroom(
  file = "reduced_data/uspc.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)

# Convert data to data.table format
setDT(patent_data)
setDT(assignee_data)
setDT(patent_assignee_data)
setDT(uspc_data)
class(assignee_data)

# First Question: Data Table Production
colnames(assignee_data)
setnames(assignee_data, "id", "assignee_id")
q1_data <- merge(x = assignee_data, y = patent_assignee_data, by = "assignee_id")

# Second Question: Data Table Production
colnames(patent_data)
setnames(patent_data, "id", "patent_id")
q2_data <- merge(x = q1_data, y = patent_data, by = "patent_id")

# Third Question: Data Table Production
uspc_data[, patent_id := as.character(patent_id)]
q3_data <- merge(x = uspc_data, y = q1_data, by = "patent_id")

#######
# 1st Answer: Calculate total number of patents per assignee and organization
setDT(q1_data)
total_patents <- q1_data[, .(n_patents = .N), by = .(assignee_id, organization)][order(-n_patents)]

top_10_US_patentholders <- total_patents[1:10]
# Print the top 10 US patent holders
print(top_10_US_patentholders)




# Export the dataframe to CSV
file_path <- "exported_data/top_10_US_patentholders.csv"
write.csv(top_10_US_patentholders, file = file_path, row.names = FALSE)
# Display a message to confirm the export
cat("Dataframe exported successfully to", file_path, "\n")

######
# 2nd Answer: Retrieve data for August 2014 and count patents per organization
august_data <- q2_data[month(date) == 8 & year(date) == 2014]

# Group the data by organization and calculate the total number of patents for each organization.
total_patents <- august_data[, .(num_patents = .N), by = organization]

# Sort the results in descending order based on the number of patents and select the top 10 entries.
top_10_comp <- total_patents[order(-num_patents)][1:10]

# Display the top 10 organizations that hold the highest number of patents in August 2014.
as.data.table(top_10_comp)

# Export the dataframe to CSV
file_path <- "exported_data/top_10_US_August2014.csv"
write.csv(top_10_comp, file = file_path, row.names = FALSE)
# Display a message to confirm the export
cat("Dataframe exported successfully to", file_path, "\n")

#####
# 3rd Answer: Analyze patents assigned to top 10 companies and count patents per technology class
# Perform a grouping operation on the data based on the organization and determine the count of patents for each group
total_patents <- q3_data[, .(num_patents = .N), by = organization]

# Sort the results in descending order based on the number of patents and select the top 10 entries
top_10_comp_patent <- total_patents[order(-num_patents)][1:10]

# Filter the data to only include patents assigned to the top 10 companies
top_10_comp_patent <- q3_data[organization %in% top_10_comp_patent$organization]

# Group the data by USPTO tech main class and count the number of patents
mainclass_total <- top_10_comp_patent[, .(num_patents = .N), by = mainclass_id]

# Order the results by number of patents in descending order and subset the top 5 technology classes
top_5_mainclass <- mainclass_total[order(-num_patents)][1:5]

# Print the the top 5 USPTO tech main classes
print(top_5_mainclass)
