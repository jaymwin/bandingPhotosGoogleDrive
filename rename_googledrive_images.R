
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(here)
library(curl)

here <- here::here()
here()


# import data from Google Drive -------------------------------------------

drive_find(q = "fullText contains 'SERDP banding (Responses)'")

sheets_get("17aHwTtVukUjwUw8uJsl2zrnYgVd-nlLR2aTwjJDZBEk")
dat <- read_sheet('17aHwTtVukUjwUw8uJsl2zrnYgVd-nlLR2aTwjJDZBEk')

# dat <- read_csv(here('SERDP banding (Responses) - Form Responses 1.csv'))

# clean up a bit
dat <- dat %>%
  select(year, month, day, markerID, `Tail photo`, `Front popsicle photo`, 
         `Back popsicle photo`, `Front wing photo`, `Back wing photo`, `Other photo`) %>%
  rename(
    tail = `Tail photo`,
    frontPop = `Front popsicle photo`,
    backPop = `Back popsicle photo`,
    frontWing = `Front wing photo`,
    backWing = `Back wing photo`,
    other = `Other photo`
  ) 


# format date to YYYY-MM-DD for naming later ------------------------------

dat <- dat %>%
  mutate(date = as.character(ymd(paste(year, month, day, sep = '-')))) %>% # needs to be in character format for naming for some reason
  select(-year, -month, -day) %>%
  select(markerID, date, tail, frontPop, backPop, frontWing, backWing, other)


# convert https links to a downloadable format ----------------------------------------

# see this link for more info: https://superuser.com/questions/717971/how-to-download-multiple-google-drive-photos-from-urls

# https format to share photo:
# https://drive.google.com/open?id=1KhCKOJ6zb7P2DXDmTsMl0JYhw223XTbH

# https format needed to download photo using 'curl' package
# https://drive.google.com/uc?export=download&id=1KhCKOJ6zb7P2DXDmTsMl0JYhw223XTbH

# first part of link needs to look like this for downloading
export <- 'https://drive.google.com/uc?export=download&id='

# convert all sharing links to downloading links by 1) chopping off photo ID at end of link,
# and 2) adding the export format in front of the photo ID
dat <- dat %>%
  mutate_at(vars(tail, frontPop, backPop, frontWing, backWing, other), ~str_sub(., start = -33, end = -1)) %>%
  mutate_at(vars(tail, frontPop, backPop, frontWing, backWing, other), ~paste0(export, .)) 


# now loop through each bird and type of photo -------------------------

# function for naming images and reading links: https://stackoverflow.com/questions/54262620/downloading-images-using-curl-library-in-a-loop-over-data-frame
read_and_title <- as_mapper(~curl_download(url = ..4, 
                                           destfile = paste0(here(), ..1,"_",..2,"_",..3,".jpg")))

# change format so photo columns become variable in column 'photo_type'
dat <- dat %>% 
  gather(key = "photo_type", value = "url", 3:8) %>%
  filter(url != 'https://drive.google.com/uc?export=download&id=NA') %>% # get rid of NA's (missing or not taken photos)
  arrange(markerID) # organize by bird
dat

# finally, download and name jpgs using purrr
dat %>%
  pmap_chr(read_and_title)

