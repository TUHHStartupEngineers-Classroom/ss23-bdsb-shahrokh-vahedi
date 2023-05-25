library(vroom)
library(data.table)

col_types <- list(
  id = col_character(),
  type = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file = "reduced_data/patent.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file = "reduced_data/patent_assignee.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
assignee_tbl <- vroom(
  file = "reduced_data/assignee.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)
uspc_tbl <- vroom(
  file = "reduced_data/uspc.tsv", 
  delim = "\t", 
  col_types = col_types,
  na = c("", "NA", "NULL")
)

# Converting to data_table
setDT(patent_tbl)
setDT(assignee_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)
class(assignee_tbl)

# First Question Data Table Production
colnames(assignee_tbl)
setnames(assignee_tbl, "id", "assignee_id")
q1_data <- merge(x = assignee_tbl, y = patent_assignee_tbl, by = "assignee_id")

# Second Question Data Table Production
colnames(patent_tbl)
setnames(patent_tbl, "id", "patent_id")
q2_data <- merge(x = q1_data, y = patent_tbl, by = "patent_id")

# Third Question Data Table Production

uspc_tbl[, patent_id := as.character(patent_id)]
q3_data <- merge(x = uspc_tbl, y = q1_data, by = "patent_id")


# 1st answer

setDT(q1_data)
total_patents <- q1_data[, .(n_patents = .N), by = .(assignee_id, organization)][order(-n_patents)]

top_10_patent_holders <- total_patents[1:10]
# Print the results
print(top_10_patent_holders)

##########################################################

# 2nd answer

august_data <- q2_data[month(date) == 8 & year(date) == 2014]

# Group by organization and count the number of patents
total_patents <- august_data[, .(num_patents = .N), by = organization]

# Order the results by number of patents in descending order and subset the top 10
top_10 <- total_patents[order(-num_patents)][1:10]

# Print the results in data.table format
as.data.table(top_10)

##########################################################


# 3rd answer

# Group the data by organization and count the number of patents
total_patents <- q3_data[, .(num_patents = .N), by = organization]

# Order the results by number of patents in descending order and subset the top 10
top_10_companies <- total_patents[order(-num_patents)][1:10]

# Filter the data to only include patents assigned to the top 10 companies
top_10_patents <- q3_data[organization %in% top_10_companies$organization]

# Group the data by USPTO tech main class and count the number of patents
tech_counts <- top_10_patents[, .(num_patents = .N), by = mainclass_id]

# Order the results by number of patents in descending order and subset the top 5
top_5_tech_classes <- tech_counts[order(-num_patents)][1:5]

# Print the results
print(top_5_tech_classes)
