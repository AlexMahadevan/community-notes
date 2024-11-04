library(dplyr)
library(readr)
library(lubridate)
library(data.table)

# Define the path to the ratings file
ratings_file <- "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00012.tsv"

# Read in the TSV file
ratings_data <- read_tsv(ratings_file)

# Inspect the column names to confirm the timestamp column
glimpse(ratings_data)

# Convert to a normal date/time
ratings_data <- ratings_data %>%
  mutate(created_date = as.Date(as_datetime(createdAtMillis / 1000)))

# Extract unique dates to determine date range
date_range <- ratings_data %>%
  summarize(start_date = min(created_date), end_date = max(created_date))

# Display the date range
print(date_range)

# Now we know that ratings files all have ratings from recent weeks, which means we 
# have to combine them all

# Define a vector with paths to all rating files
ratings_urls <- c(
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00000.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00001.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00002.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00003.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00004.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00005.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00006.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00007.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00008.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00009.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00010.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00011.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00012.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00013.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00014.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteRatings/ratings-00015.tsv"
)

# Define the target date
target_date <- as.Date("2024-11-5")

# Initialize an empty list to store data from October 31
election_day <- list()

# Process each URL
for (url in ratings_urls) {
  # Read in data from the URL in chunks using fread
  data <- fread(url)
  
  # Convert the createdAtMillis column to date format
  data[, created_date := as.Date(as_datetime(createdAtMillis / 1000))]
  
  # Filter for October 31 data
  oct_31_ratings <- data[created_date == target_date]
  
  # Append the filtered data to our list
  election_day[[url]] <- election_day_data
}

# Now we have all the ratings for the specific date
# Now let's bring in the notes and status history

# Load the notes and note status history data
notes_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/03/notes/notes-00000.tsv")
note_status_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/03/noteStatusHistory/noteStatusHistory-00000.tsv")

# Rename columns for clarity before merging, and select which ones we need
notes_data <- notes_data %>%
  select(noteId, summary)

note_status_data <- note_status_data %>%
  rename(noteCreatedAt = createdAtMillis)  # Timestamp related to status history

election_day_data <- election_day_data %>% 
  rename(ratingCreatedAt = createdAtMillis)

####

# Join with October 31 ratings on `noteId`
election_data <- election_day_data %>%
  left_join(notes_data, by = "noteId") %>%
  left_join(note_status_data, by = "noteId")

# before going further, let's export this and feed it to GPT so it knows what I'm doing

write_csv(election_data, "combined.csv")

# Now we can look for coordinated behavior

# Convert timestamps to POSIXct format with correct column names
combined_data <- oct_31_data_combined %>%
  mutate(ratingDateTime = as.POSIXct(ratingCreatedAt / 1000, origin = "1970-01-01"),
         noteDateTime = as.POSIXct(noteCreatedAt / 1000, origin = "1970-01-01"))

# Let's look at notes that are insanely downvoted

# Filter for "NOT_HELPFUL" ratings using the correct column name for helpfulness level
not_helpful_data <- combined_data %>%
  filter(helpfulnessLevel == "NOT_HELPFUL")

# Group by noteId and one hour minute time windows, and count occurrences
coordinated_behavior <- not_helpful_data %>%
  mutate(hour_window = floor_date(ratingDateTime, "hour")) %>%
  group_by(noteId, hour_window) %>%
  summarize(not_helpful_count = n()) %>%
  ungroup()

suspicious_clusters <- coordinated_behavior %>%
  filter(not_helpful_count > 5)

# Display suspicious clusters
print(suspicious_clusters)


 We can also think about cumulative Notes


# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Filter data for specific noteId(s) you want to analyze
note_ids_to_plot <- c("1851777319486275858", 	
                      "1851783417328795676")  # Replace with actual IDs
filtered_data <- combined_data %>%
  filter(noteId %in% note_ids_to_plot)


# Convert the rating timestamp to POSIXct format
filtered_data <- filtered_data %>%
  mutate(ratingDateTime = as.POSIXct(ratingCreatedAt / 1000, origin = "1970-01-01"))

# Create binary indicators based on helpfulnessLevel
filtered_data <- filtered_data %>%
  mutate(
    helpful = ifelse(helpfulnessLevel == "HELPFUL", 1, 0),
    not_helpful = ifelse(helpfulnessLevel == "NOT_HELPFUL", 1, 0)
  )

cumulative_data <- filtered_data %>%
  arrange(noteId, ratingDateTime) %>%
  group_by(noteId) %>%
  mutate(
    helpful_cumulative = cumsum(helpful),
    not_helpful_cumulative = cumsum(not_helpful)
  ) %>%
  ungroup()


ggplot(cumulative_data, aes(x = ratingDateTime)) +
  geom_line(aes(y = helpful_cumulative, color = "Helpful"), size = 1) +
  geom_line(aes(y = not_helpful_cumulative, color = "Not Helpful"), size = 1) +
  geom_area(aes(y = helpful_cumulative, fill = "Helpful"), alpha = 0.2) +
  geom_area(aes(y = not_helpful_cumulative, fill = "Not Helpful"), alpha = 0.2) +
  scale_color_manual(values = c("green", "red")) +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "Cumulative Votes Over Time",
       x = "Date/Time (UTC)",
       y = "Votes",
       color = "Ratings",
       fill = "Ratings") +
  facet_wrap(~ noteId, scales = "free_y") +
  theme_minimal()

# Let's see if there are prolific users 

user_behavior <- combined_data %>%
  mutate(
    not_helpful = ifelse(helpfulnessLevel == "NOT_HELPFUL", 1, 0),
    helpful = ifelse(helpfulnessLevel == "HELPFUL", 1, 0)
  ) %>%
  group_by(raterParticipantId) %>%
  summarize(
    total_ratings = n(),
    total_not_helpful = sum(not_helpful),
    total_helpful = sum(helpful),
    not_helpful_ratio = total_not_helpful / total_ratings
  ) %>%
  ungroup()

# Filter users with high proportions of "NOT_HELPFUL" ratings (e.g., > 0.8 or 80%)
suspicious_users <- user_behavior %>%
  filter(not_helpful_ratio > 0.8)

# Display the suspicious users
print(suspicious_users)

## let's look at the most prolific and negative user: 40AF41771EA8630AA64FA2E6180B1A170B516FEEAECF9E8A58AEC7D2F09F5975

negative_nancy <- combined_data %>% 
  filter(raterParticipantId == "40AF41771EA8630AA64FA2E6180B1A170B516FEEAECF9E8A58AEC7D2F09F5975")

negative_nancy_notes <- note_status_data %>% 
  filter(noteAuthorParticipantId == "40AF41771EA8630AA64FA2E6180B1A170B516FEEAECF9E8A58AEC7D2F09F5975")
  
