library(readr)
library(dplyr)
library(tidyr)
library(purrr)


####### ------- DATASET 1: Create the data that will be used in the "Topics at a glance" visualization ------- #######

paper_data <- read.csv('papers.csv')
paper_data$publicationDate <- as.Date(paper_data$publicationDate)

# function to wrap text a specified number of characters without breaking words
wrap_text <- function(text, width) {
  if (is.na(text)) {
    return(NA)
  }
  words <- unlist(strsplit(as.character(text), " "))
  wrapped_text <- ""
  current_line_length <- 0
  
  for (word in words) {
    if (nchar(word) + current_line_length > width) {
      wrapped_text <- paste0(wrapped_text, "<br>", word)
      current_line_length <- nchar(word)
    } else {
      if (current_line_length != 0) {
        wrapped_text <- paste0(wrapped_text, " ")
      }
      wrapped_text <- paste0(wrapped_text, word)
      current_line_length <- current_line_length + nchar(word) + 1
    }
  }
  return(wrapped_text)
}
# wrap title and tldr so they look better in plotly popup
paper_data$title <- sapply(paper_data$title, function(x) wrap_text(x, 100))
paper_data$tldr <- sapply(paper_data$tldr, function(x) wrap_text(x, 100))

# Order rows by publication date, for some reason
paper_data <- paper_data[order(paper_data[, "publicationDate"]), ]
# drop authorNames, authorIds, referenceCount
paper_data <- paper_data %>%
  select(-authorNames, -authorIds, -referenceCount)



####### ------- DATASET 2: Create data that will be used for the Topic Statistics visualization ------- #######

papers_df <- read.csv('papers.csv')

df_authors <- papers_df %>%
  mutate(authorId = strsplit(as.character(authorIds), ",\\s*")) %>%
  unnest(authorId) %>%
  select(authorId, paperId, title, publicationDate, influentialCitationCount, citationCount, topicLabel)

# count the number of papers per author per topic
author_topic_counts <- df_authors %>%
  group_by(authorId, topicLabel) %>%
  summarise(paperCount = n(), .groups = 'drop')
# spread the counts into separate columns for each topic
author_topic_summary <- author_topic_counts %>%
  pivot_wider(names_from = topicLabel, values_from = paperCount, values_fill = list(paperCount = 0))
# calculate the total papers per topic
total_papers_per_topic <- author_topic_summary %>%
  select(-authorId) %>%
  summarise_all(sum)
# Calculate the proportion of papers per author for each topic
proportions <- author_topic_summary %>%
  select(-authorId) %>%
  mutate(across(everything(), ~ .x / total_papers_per_topic[[cur_column()]]))
# Calculate the HHI for each topic
hhi <- proportions %>%
  mutate(across(everything(), ~ .x^2)) %>%
  summarise_all(sum)
# Reshape the data to a long format
hhi_long <- hhi %>%
  pivot_longer(cols = everything(), names_to = "topicLabel", values_to = "HHI")

# stats per topic
topic_stats <- papers_df %>%
  group_by(topicLabel) %>%
  summarise(
    xOfCentroid = mean(x2D, na.rm = TRUE),
    yOfCentroid = mean(y2D, na.rm = TRUE),
    numberPapers = n(),
    avgInfluentialCitationCount = mean(influentialCitationCount, na.rm = TRUE),
    totalInfluentialCitationCount = sum(influentialCitationCount, na.rm = TRUE),
    avgCitationCount = mean(citationCount, na.rm = TRUE),
    totalCitationCount = sum(citationCount, na.rm = TRUE),
    avgReferenceCount = mean(referenceCount, na.rm = TRUE),
    proportionPapersWith0Citations = mean(citationCount == 0, na.rm = TRUE),
    proportionPapersWith0InfluentialCitations = mean(influentialCitationCount == 0, na.rm = TRUE)
  )

# Add the hhi column 
topic_stats <- topic_stats %>% left_join(hhi_long, by = "topicLabel")

# Scale varaibles that could be represented plotted as the size of points
topic_stats$scaledHHI <- topic_stats$HHI * 700
topic_stats$scaledNumberPapers <- topic_stats$numberPapers * 0.01
topic_stats$scaledAvgInfluentialCitationCount <- topic_stats$avgInfluentialCitationCount * 3
topic_stats$scaledTotalInfluentialCitationCount <- topic_stats$totalInfluentialCitationCount * 0.01
topic_stats$scaledAvgCitationCount <- topic_stats$avgCitationCount * .3
topic_stats$scaledTotalCitationCount <- topic_stats$totalCitationCount * 0.00075
topic_stats$scaledAvgReferenceCount <- topic_stats$avgReferenceCount * .05
topic_stats$scaledProportionPapersWith0Citations <- topic_stats$proportionPapersWith0Citations * 4
topic_stats$scaledProportionPapersWith0InfluentialCitations <- topic_stats$proportionPapersWith0InfluentialCitations * 2

# round the values that will be displayed in plotly graph so they not too long
topic_stats <- topic_stats %>% 
  mutate(
    avgInfluentialCitationCount = round(avgInfluentialCitationCount, 4),
    avgCitationCount = round(avgCitationCount, 4),
    avgReferenceCount = round(avgReferenceCount, 4),
    proportionPapersWith0Citations = round(proportionPapersWith0Citations, 4),
    proportionPapersWith0InfluentialCitations = round(proportionPapersWith0InfluentialCitations, 4),
    HHI = round(HHI, 4)
  )
  
# Remove unasssigned papers topic since it is too damn big and is not even a real category
topic_stats <- topic_stats %>%
  filter(topicLabel != "Unassigned Papers")



####### ------- DATASET 3: Create data that will be used for author influence visualization. This dataset is the the set #######
####### of unique authors for each topic category  ------- #######

papers_df <- read.csv('papers.csv')

# Currently, the authors for a paper are comma separated so we have to create long format data
df_authors <- papers_df %>%
  mutate(authorIds = strsplit(as.character(authorIds), ",\\s*")) %>%
  unnest(authorIds) %>%
  select(authorIds, authorNames, paperId, influentialCitationCount, citationCount, topicLabel)
df_authors <- rename(df_authors, authorId = authorIds)
temp_df <- papers_df %>%
  mutate(
    authorName = strsplit(as.character(authorNames), ",\\s*"),
    authorId = strsplit(as.character(authorIds), ",\\s*")
)

# We must account for cases where Semantic Scholar API returned fucked data for authorIds/authorNames, and
# accurately map each id to name, inserting NA for author name where it is ambiguous
#Create a dataframes where the number of authors names does and doesn't equals the number of author ids
df_equal <- temp_df %>%
  filter(lengths(authorName) == lengths(authorId))
df_not_equal <- temp_df %>%
  filter(lengths(authorName) != lengths(authorId))
df_equal_flattened <- df_equal %>%
  select(authorName, authorId) %>%
  unnest(c(authorName, authorId)) %>%
  # Ensure unique author_id and author_name pairs
  distinct(authorName, authorId) 
df_not_equal_flattened <- df_not_equal %>%
  select(authorId) %>%
  unnest(authorId) %>%
  distinct(authorId)
# Identify unique author_ids in df_not_equal_flattened that are not in df_equal_flattened
unique_author_ids_not_equal <- df_not_equal_flattened %>%
  filter(!(authorId %in% df_equal_flattened$authorId)) %>%
  mutate(authorName = 'Not Provided')
# Data frame with ALL unique ids and their associated name - 54428 total
author_mappings <- bind_rows(df_equal_flattened, unique_author_ids_not_equal)

# Calculate some author statistics within each topic
author_influence <- df_authors %>%
  group_by(authorId, topicLabel) %>%
  summarise(
    authorTopicPaperCount = n(),
    topicTotalInfluentialCitationCount = sum(influentialCitationCount, na.rm = TRUE),
    topicAvgInfluentialCitationCount = mean(influentialCitationCount, na.rm = TRUE),
    topicTotalCitationCount = sum(citationCount, na.rm = TRUE),
    topicAvgCitationCount = mean(citationCount, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    topicAvgCitationCount = round(topicAvgCitationCount, 4),
    topicAvgInfluentialCitationCount = round(topicAvgInfluentialCitationCount, 4)
  )


# Do some data processing that leads to the calcuation of "market share" for each author in each topic category
total_papers_per_topic <- author_influence %>%
  group_by(topicLabel) %>%
  summarise(totalAuthorsInTopic = n(), .groups = 'drop')
author_influence <- author_influence %>%
  left_join(total_papers_per_topic, by = "topicLabel") %>%
  mutate(marketShare = authorTopicPaperCount / totalAuthorsInTopic)

# Add the author name column that we got before
author_influence <- left_join(author_influence, author_mappings, by = "authorId")

### Now we do "other topic" analysis for authors. The result will be new columns authorSumOtherPapers, plotlyTopicsAndCounts,
### and topicSpecializationScore

# Get unique topic labels
unique_topics <- unique(author_influence$topicLabel)

# Define a function for processing each topic
process_topic <- function(topic) {
  # Create a df of all authors in the dataset, and a column that has a comma separated list of other topics they wrote papers in,
  # along with a third column that shows the comma separated number of papers in those topics, listed respectively
  paper_counts_other_topics <- author_influence %>%
    filter(topicLabel != topic) %>%
    group_by(authorId) %>%
    summarise(
      otherTopicLabelsList = paste(topicLabel, collapse = ", "),
      otherTopicCountsList = paste(authorTopicPaperCount, collapse = ", "),
      .groups = 'drop'
  )
  # Filter for authors who wrote papers on the current topic
  filtered_data <- author_influence %>%
    filter(topicLabel == topic)
  # Merge the "other paper" for each author with the df that is the set of unique author, topicLabel rows (its the bigger one)
  filtered_data2 <- filtered_data %>%
    left_join(paper_counts_other_topics, by = "authorId") %>%
    mutate(
      otherTopicCountsList = ifelse(is.na(otherTopicCountsList), "0", otherTopicCountsList),
      authorSumOtherPapers = sapply(strsplit(otherTopicCountsList, ",\\s*"), function(x) sum(as.numeric(x), na.rm = TRUE)),
      # Create formatted plotly labels for each author,topic row that include the list of "other topics" and the respective counts
      plotlyTopicsAndCounts = mapply(function(topics, counts) {
        topics_list <- strsplit(topics, ", ")[[1]]
        counts_list <- strsplit(counts, ", ")[[1]]
        formatted_list <- paste("&nbsp;&nbsp;", topics_list, ": ", counts_list, collapse = "<br>&nbsp;&nbsp;")
        return(formatted_list)
      }, otherTopicLabelsList, otherTopicCountsList)
    ) %>%
    # Calculate the topic specialization score for each author in a particular topic
    mutate(
      topicSpecializationScore = authorTopicPaperCount / (authorTopicPaperCount + authorSumOtherPapers),
      topicSpecializationScore = round(topicSpecializationScore, 4)
    ) %>%
    select(-otherTopicLabelsList, -otherTopicCountsList, -totalAuthorsInTopic)
  return(filtered_data2)
}

# Apply the function to each topic label
processed_data_list <- lapply(unique_topics, process_topic)

# Combine the results into a single dataframe
author_influence <- bind_rows(processed_data_list)

# Remove unasssigned papers topic since it is too damn big and is not even a real category
author_influence <- author_influence %>%
  filter(topicLabel != "Unassigned Papers")

####### Save data frames to an RData file #######
save(paper_data, topic_stats, author_influence , file = "LLM-papers-2023-dashboard/papers.RData")
