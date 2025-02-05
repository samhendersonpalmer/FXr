# This script creates and sends an email conditionally based on GBP to EUR exchange rates

# set up credentials for gmail using Github Secrets --------------------------------------------
library(blastula)

gmail_creds <- blastula::creds_envvar(
  user = Sys.getenv("MY_GMAIL_ACCOUNT"),
  pass_envvar = "APP_PASSWORD",
  provider = "gmail"
)

# Import data ----------------------------------------------------
source("exchange_functions.R")

EUR_rate_today <- EUR_rate_today()
EUR_rate_prev3 <- EUR_rate_prev3()

# Create stylised table for 3 day rates ---------------------------------------------------
library(gt)

# Function for colouring rate change
add_rating_color <- function(rating, change_24hr){

  # css code for colours based on percentage change of rate over 24 hours
  add_color <- if (!is.na(rating) & rating == "arrow-up") {
    "background: hsl(116, 60%, 90%); color: hsl(116, 30%, 25%);"
  } else if (!is.na(rating) & rating == "arrow-down") {
    "background: hsl(350, 70%, 90%); color: hsl(350, 45%, 30%);"
    # Ignore NA value of first value in time series as nothing to compare to
  } else if (is.na(rating)) {NULL}
  
  # Again only produce output for non-NA values
  div_out <- if (is.na(rating)) {
    NULL} else if (!is.na(rating)) {
      
      # Create padding around colour block and text parameters
      htmltools::div(
    style = paste(
      "display: inline-block; padding: 1px 10px; border-radius: 15px; font-weight: 600; font-size: 12px;",
      # Append colour detail
      add_color
      # Create percentage value to display as gt won't allow conversion later for custom css
    ), paste0(round(change_24hr*100, 2), "%"))
    }
  
  # Convert to html for output in gt table cell
  as.character(div_out) %>% 
    gt::html()
}

# Create display table
tbl <- EUR_rate_prev3 |> 
  # Convert to gt table object
  gt() |>
  # Create colouring and icon indicator columns, mapping custom colour function
  cols_add(rating = ifelse(change_24hr > 0, "arrow-up", "arrow-down"),
           rating_colour = purrr::map2(rating, change_24hr, add_rating_color)) |> 
  # Include arrow icons for direction of rate change
  fmt_icon(
    columns = rating,
    fill_color = c("arrow-up" = "green", "arrow-down" = "red")) |> 
  # Replace NA values
  sub_missing(
    columns = c(change_24hr, rating),
    missing_text = "") |> 
  # Remove redundant columns
  cols_hide(change_24hr) |>
  # Icon at end for aesthetics
  cols_move(rating, after = rating_colour) |> 
  # More descriptive titles
  cols_label(
    date = "Date",
    rate = "GBP to EUR",
    rating_colour = "24hr % change",
    rating = "") |> 
  # Styling
  cols_align(
    align = 'center') |> 
  tab_style(
      locations = list(
        cells_column_labels()
      ),
      style = list(
        cell_fill(color = 'gray50'),
        cell_text(color = 'white', size = 14, weight = 'bold')))
  

# Generate email object using blastula ------------------------------------
my_email_object <- compose_email(
  body = blastula::md(
    glue::glue(
      "Hey trader,
      
      Here is your latest GBP to EUR update: {EUR_rate_today}
      
      {tbl |>  as_raw_html()}"
    )
  ),
  footer = blastula::md("Brought to you by *FXr*")
)


# Send the email ----------------------------------------------------------
# Make the email conditional on EUR rate >= 1.204 today or by >= 1% over past 3 days
if (EUR_rate_today >= 1.204 | 
    (EUR_rate_prev3["change_24hr"][[1]][1] >= 0.01 &
     EUR_rate_prev3["change_24hr"][[1]][2] >= 0.01)) {
  # Send email
  my_email_object |> smtp_send(
    from = Sys.getenv("MY_GMAIL_ACCOUNT"),
    to = Sys.getenv(RECIPIENTS),
    subject = paste0("The trade is on! ", EUR_rate_today," EUR 💱"),
    credentials = gmail_creds) 
} else {
  NULL
}
