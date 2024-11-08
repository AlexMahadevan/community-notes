library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(ggplot2)

setwd("/Users/alexmahadevan/r_projects/community_notes")

# Define a vector with paths to all rating files
ratings_urls <- c(
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00000.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00001.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00002.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00003.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00004.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00005.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00006.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00007.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00008.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00009.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00010.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00011.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00012.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00013.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00014.tsv",
  "https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteRatings/ratings-00015.tsv"
)

# Define the target date start and end in EST (November 5, 2024 EST)
start_time_est <- as.POSIXct("2024-11-05 00:00:00", tz = "America/New_York")
end_time_est <- as.POSIXct("2024-11-05 23:59:59", tz = "America/New_York")

# Convert start and end time to milliseconds since epoch (UTC)
start_time_utc_ms <- as.numeric(start_time_est) * 1000
end_time_utc_ms <- as.numeric(end_time_est) * 1000


# Initialize an empty list to store data from October 31
election_day_data <- list()

# Process each URL
for (url in ratings_urls) {
  # Read in data from the URL in chunks using fread
  data <- fread(url)
  
  # Filter for rows within the specified UTC milliseconds range
  election_day <- data[createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms]
  
  # Append the filtered data to our list
  election_day_data[[url]] <- election_day
}

# Combine all election day data into one data frame
combined_election_day_data <- rbindlist(election_day_data, fill = TRUE)

# Display the first few rows of the combined data
print(head(combined_election_day_data))
# Now we have all the ratings for the specific date
# Now let's bring in the notes and status history

# Load the notes and note status history data
# Load the notes data and filter based on the milliseconds range
notes_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/08/notes/notes-00000.tsv") %>% 
  filter(createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms) %>% # filter by date
  mutate(tweet_url = paste0("https://twitter.com/i/web/status/", tweetId)) # create a link to easily access the tweet

note_status_data <- fread("https://ton.twimg.com/birdwatch-public-data/2024/11/08/noteStatusHistory/noteStatusHistory-00000.tsv") %>% 
  filter(createdAtMillis >= start_time_utc_ms & createdAtMillis <= end_time_utc_ms) 

notes_selected <- notes_data %>%
  select(noteId, summary, tweetId, tweet_url)

# Perform a left join with note_status_data to get everything from the status dataset
combined_data <- note_status_data %>%
  left_join(notes_selected, by = "noteId")

# Count total notes and helpful notes based on currentStatus
note_counts <- note_status_data %>%
  summarize(
    total_notes = n(),
    helpful_notes = sum(currentStatus == "CURRENTLY_RATED_HELPFUL", na.rm = TRUE)
  )

# Calculate the ratio of helpful notes
note_counts <- note_counts %>%
  mutate(helpful_ratio = helpful_notes / total_notes)

# Display the results
print(note_counts)

# Let's consider notes with lots of votes but did not meet the threshold
# rename election day to something we understand

ratings_data <- combined_election_day_data

# Before we go further let's export everything, and upload to Github

write_csv(ratings_data,"ratings_data.csv")
write_csv(combined_data, "notes_and_status.csv")

# Step 1: Summarize ratings data to get counts of "helpful", "not helpful", and total ratings for each noteId
ratings_summary <- ratings_data %>%
  group_by(noteId) %>%
  summarize(
    helpful_count = sum(helpfulnessLevel == "HELPFUL", na.rm = TRUE),
    not_helpful_count = sum(helpfulnessLevel == "NOT_HELPFUL", na.rm = TRUE),
    total_ratings = n()  # Count of all ratings
  ) %>%
  ungroup()

# Step 2: Join the summarized ratings back to the combined_data
combined_data <- combined_data %>%
  left_join(ratings_summary, by = "noteId")
  
notes_status_ratings <- combined_data %>%   
  arrange(desc(total_ratings))

# export it

write_csv(notes_status_ratings, "all_cn_data.csv")






# Define thresholds for "highly helpful" and "highly not helpful" ratings
threshold_helpful_ratings <- 10  # e.g., notes with at least 10 helpful ratings
threshold_not_helpful_ratings <- 10  # e.g., notes with at least 10 not helpful ratings

# Aggregate ratings to count "helpful" and "not helpful" ratings per note
note_ratings_summary <- ratings_data %>%
  group_by(noteId) %>%
  summarize(
    helpful_count = sum(helpfulnessLevel == "HELPFUL"),
    not_helpful_count = sum(helpfulnessLevel == "NOT_HELPFUL")
  ) %>%
  ungroup()

# Filter for notes that are highly rated as "helpful" or "not helpful"
highly_helpful_notes <- note_ratings_summary %>%
  filter(helpful_count >= threshold_helpful_ratings)

highly_not_helpful_notes <- note_ratings_summary %>%
  filter(not_helpful_count >= threshold_not_helpful_ratings)

# Join with notes and note status data to get details and current status
highly_helpful_notes_with_status <- highly_helpful_notes %>%
  left_join(notes_data, by = "noteId") %>%
  left_join(note_status_data, by = "noteId")

highly_not_helpful_notes_with_status <- highly_not_helpful_notes %>%
  left_join(notes_data, by = "noteId") %>%
  left_join(note_status_data, by = "noteId")

# Display results for both
print("Highly Helpful Notes without 'Helpful' Status:")
print(highly_helpful_notes_with_status %>%
        filter(currentStatus != "CURRENTLY_RATED_HELPFUL"))

print("Notes with High 'Not Helpful' Ratings:")
print(highly_not_helpful_notes_with_status)

#######

# Load necessary library
library(dplyr)
library(lubridate)

# Step 1: Convert createdAtMillis to POSIXct format for date-time manipulation
ratings_data <- combined_election_day_data %>%
  mutate(ratingDateTime = as.POSIXct(createdAtMillis / 1000, origin = "1970-01-01"))

# Define a shorter time window for grouping (e.g., 15 minutes)
time_window <- 15  # in minutes

# Step 2: Group by noteId and time window, and count "helpful" and "not helpful" ratings
coordinated_behavior_data <- ratings_data %>%
  mutate(time_bin = floor_date(ratingDateTime, unit = "15 minutes")) %>%
  group_by(noteId, time_bin) %>%
  summarize(
    helpful_count = sum(helpfulnessLevel == "HELPFUL"),
    not_helpful_count = sum(helpfulnessLevel == "NOT_HELPFUL")
  ) %>%
  ungroup()

# Step 3: Define thresholds for flagging suspicious activity in a time window
threshold_helpful_in_window <- 5   # e.g., 5 "helpful" ratings within 30 minutes
threshold_not_helpful_in_window <- 5  # e.g., 5 "not helpful" ratings within 30 minutes

# Filter for notes with suspicious activity based on thresholds
suspicious_helpful_activity <- coordinated_behavior_data %>%
  filter(helpful_count >= threshold_helpful_in_window)

suspicious_not_helpful_activity <- coordinated_behavior_data %>%
  filter(not_helpful_count >= threshold_not_helpful_in_window)


# Step 4: Merge with notes data to get the note summary and tweet ID
suspicious_helpful_activity_with_details <- suspicious_helpful_activity %>%
  left_join(notes_data %>% select(noteId, summary, tweetId), by = "noteId")

suspicious_not_helpful_activity_with_details <- suspicious_not_helpful_activity %>%
  left_join(notes_data %>% select(noteId, summary, tweetId), by = "noteId")


note_ids_to_plot <- c("1853298766222111218", "1853547004162421172")  # Replace with actual IDs

# Filter data for the specified notes
filtered_data <- combined_election_day_data %>%
  filter(noteId %in% note_ids_to_plot)

# Convert createdAtMillis to POSIXct format for date-time manipulation if not already done
filtered_data <- filtered_data %>%
  mutate(ratingDateTime = as.POSIXct(createdAtMillis / 1000, origin = "1970-01-01"))

# Create binary indicators for "helpful" and "not helpful" based on helpfulnessLevel
filtered_data <- filtered_data %>%
  mutate(
    helpful = ifelse(helpfulnessLevel == "HELPFUL", 1, 0),
    not_helpful = ifelse(helpfulnessLevel == "NOT_HELPFUL", 1, 0)
  )

# Calculate cumulative counts of "helpful" and "not helpful" ratings over time for each note
cumulative_data <- filtered_data %>%
  arrange(noteId, ratingDateTime) %>%
  group_by(noteId) %>%
  mutate(
    helpful_cumulative = cumsum(helpful),
    not_helpful_cumulative = cumsum(not_helpful)
  ) %>%
  ungroup()

# Plot cumulative "helpful" and "not helpful" ratings over time for each specified note
ggplot(cumulative_data, aes(x = ratingDateTime)) +
  geom_line(aes(y = helpful_cumulative, color = "Helpful"), size = 1) +
  geom_line(aes(y = not_helpful_cumulative, color = "Not Helpful"), size = 1) +
  geom_area(aes(y = helpful_cumulative, fill = "Helpful"), alpha = 0.2) +
  geom_area(aes(y = not_helpful_cumulative, fill = "Not Helpful"), alpha = 0.2) +
  scale_color_manual(values = c("green", "red")) +
  scale_fill_manual(values = c("green", "red")) +
  labs(title = "Cumulative Votes Over Time — Community Notes",
       x = "Date/Time (UTC)",
       y = "Votes",
       color = "Ratings",
       fill = "Ratings") +
  facet_wrap(~ noteId, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "top")