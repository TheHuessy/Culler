library(shiny)
library(shinysense)
library(magick)
library(DBI)
library(RPostgreSQL)
library(yaml)
library(tools)
library(httr)

##### PULL IN DATA FROM POSTGRES #####

## For the logs
the_date <- format(Sys.time(), "%Y-%m-%d:%H:%M")
print(the_date)

##############

creds <<- read_yaml(Sys.getenv('CREDS_PATH'))

sql_driver <<- dbDriver("PostgreSQL")


####### PULLING IN DATA ##########

pull_data <- function(){
  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
  )
  ext_data <- dbGetQuery(sql_con,statement=paste("SELECT link_id, piece as base_link, keep, 'culling_external' as table_name FROM culling_external WHERE keep IS NULL ORDER BY random() LIMIT ", Sys.getenv('CULLER_BATCH_SIZE'), sep=""))
  dir_data <- dbGetQuery(sql_con,statement=paste("SELECT link_id, end_link as base_link, keep, 'culling_direct' as table_name FROM culling_direct WHERE keep IS NULL ORDER BY random() LIMIT ",Sys.getenv('CULLER_BATCH_SIZE'), sep=""))

  work <- rbind(ext_data, dir_data) %>%
    .[sample(nrow(.)),]
  dbDisconnect(sql_con)
  return(work)
}

pull_total <- function(){
  sql_con <- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
  )

  tot_left <- dbGetQuery(sql_con, statement="SELECT SUM(total) FROM (SELECT COUNT(*) as total FROM culling_external WHERE keep IS NULL UNION ALL SELECT COUNT(*) as total FROM culling_direct WHERE keep IS NULL) as tbl") %>%
    format(big.mark = ",")
  dbDisconnect(sql_con)
  return(tot_left)
}

work <<- pull_data()
tot_left <<- pull_total()

last_saved <<- 0

tot_yes <<- 0
tot_no <<- 0
yes_pct <<- ""
tot <<- nrow(work)

insta_fresh <- function(piece){

  full_url <- paste("https://www.instagram.com", piece, "?__a=1", sep = "")

  page_data <- content(GET(full_url), "parsed")

  if (is.null(page_data$graphql$shortcode_media$display_url) == TRUE){
    return("https://www.imgur.com/amadeuppage")
  } else {
    return(page_data$graphql$shortcode_media$display_url)
  }

}


get_link <- function(cnt){
  test_link <- work[cnt,2]
  test <- grep(pattern="/p/", x=test_link)
  if (length(test) == 0){
    output_link <- work[cnt,2]
  } else {
    output_link <- insta_fresh(work[cnt,2])
  }

  return(output_link)
}


get_cnt_safe <- function(work, cnt){
  new_cnt <<- cnt + 1

  #######
  print(paste("Old count val:", cnt))
  print(paste("New count val:", new_cnt))
  #######

  if (new_cnt > nrow(work)){
    return(new_cnt)
  }

  while (TRUE){
    next_ext_bool <<- file_ext(work[new_cnt,2]) %in% c("mp4", "mkv", "gif", "avi", "m4v", "m4p", "mpg")
    if (next_ext_bool == TRUE){

      # Set this URL's keep value to 0
      work[new_cnt,3] <<- 0 
      tot_no <<- tot_no + 1

      # Advance one more
      new_cnt <<- new_cnt + 1
    } else {

      tester <<- get_link(new_cnt)

      #####
      print(tester)
      print(work[new_cnt,])
      #####

      url_check <<- GET(tester)

      if (url_check$status_code == 200){
        break
      } else {
        work[new_cnt,3] <<- 0
        tot_no <<- tot_no + 1
        new_cnt <<- new_cnt + 1
      }
    }
  }
  return(new_cnt)
}

save_file <- function(work, cnt){

  sql_con <<- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
  )

  for (idx in (cnt-5):(cnt-1)){
    link <- work[idx,2]
    new_keep <- work[idx,3]
    tbl_name <- work[idx,4]
    if (tbl_name == "culling_external"){
      col_name <- "piece"
    } else {
      col_name <- "end_link"
    }
    update_string <- paste("UPDATE ",
                           tbl_name,
                           " SET keep = ",
                           new_keep,
                           " WHERE ",
                           col_name,
                           " = '",
                           link,
                           "'",
                           sep = ""
    )
    dbExecute(sql_con, update_string)
  }
  dbDisconnect(sql_con)
  last_saved <<- cnt-1
}

end_img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Thats_all_folks.svg/1019px-Thats_all_folks.svg.png"

cnt <<- get_cnt_safe(work,0)

get_link <- function(cnt){
  test_link <- work[cnt,2]
  test <- grep(pattern="/p/", x=test_link)
  if (length(test) == 0){
    output_link <- work[cnt,2]
  } else {
    output_link <- insta_fresh(work[cnt,2])
  }
  return(output_link)
}


shinyServer(function(input, output, session) {
              onStop(function(){
                       # Put this in the log so we know where a session ended
                       print("========================")
                       dbDisconnect(sql_con)
})
              object_swipe <- callModule(shinyswipr, "swiper_object")

              if (tot == 0){
                img <- image_read(end_img) %>%
                  image_write("tmp.jpg")
              } else{
                img_link <<- get_link(cnt)
                img <- image_read(img_link) %>%
                  image_write("tmp.jpg")
              }


              output$image_output <- renderImage({
                list(src = "tmp.jpg",
                     contentType = "image/jpeg")
              }, deleteFile = TRUE)


              output$ticker <- renderUI({
                h4(paste(cnt, " of ", tot, " (", tot_left, ")", sep = ""))
              })

              output$stats <- renderUI({
                h4(paste(tot_no, " | ", tot_yes, " - ", yes_pct, "%", sep=""))
              })

              observeEvent(object_swipe(), {

                             #### ASSIGN KEEP VALUE ####

                             if (object_swipe() == "left"){
                               work[cnt,3] <<- 0
                               tot_no <<- tot_no + 1
                             } else if (object_swipe() == "right") {
                               work[cnt,3] <<- 1
                               tot_yes <<- tot_yes + 1
                             }

                             cnt <<- get_cnt_safe(work,cnt)

                             yes_pct <<- round(tot_yes/cnt, digits = 3)*100

                             #### SAVE IF DIVISABLE BY 5 ####

                             if ((cnt-1) %% 5 == 0){
                               save_file(work, cnt)
                               output$ticker <- renderUI({
                                 h4(paste(cnt, " of ", tot, " * (", tot_left, ")", sep = ""))
                               })
                             } else {
                               output$ticker <- renderUI({
                                 h4(paste(cnt, " of ", tot, " (", tot_left, ")", sep = ""))
                               })
                             }


                             if (cnt > tot){
                               save_diff <<- cnt - last_saved

                               if (save_diff != 1){
                               sql_con <<- dbConnect(sql_driver,
                                                     host=creds$pg_host,
                                                     user=creds$pg_user,
                                                     password=creds$pg_pw,
                                                     dbname="strobot"
                               )

                                 for (idx in (last_saved):(cnt-1)){
                                   link <- work[idx,2]
                                   new_keep <- work[idx,3]
                                   tbl_name <- work[idx,4]
                                   if (tbl_name == "culling_external"){
                                     col_name <- "piece"
                                   } else {
                                     col_name <- "end_link"
                                   }
                                   update_string <- paste("UPDATE ",
                                                          tbl_name,
                                                          " SET keep = ",
                                                          new_keep,
                                                          " WHERE ",
                                                          col_name,
                                                          " = '",
                                                          link,
                                                          "'",
                                                          sep = ""
                                   )

                                   dbExecute(sql_con, update_string)
                                 }
                                 dbDisconnect(sql_con)
                               }

                               work <<- pull_data()
                               tot_left <<- pull_total()

                               cnt <<- get_cnt_safe(work,0)

                               last_saved <<- 0
                               tot_yes <<- 0
                               tot_no <<- 0
                               yes_pct <<- ""
                               tot <<- nrow(work)
                               if (tot == 0){
                                 img <- image_read(end_img) %>%
                                   image_write("tmp.jpg")
                               } else{
                                 img_link <<- get_link(cnt)
                                 img <<- image_read(img_link) %>% 
                                   image_write("tmp.jpg")
                               }

                             } else {
                               ## Update image and resave tmp
                               img_link <<- get_link(cnt)
                               img <<- image_read(img_link) %>% 
                                 image_write("tmp.jpg")
                             }

                             output$image_output <- renderImage({
                               list(src = "tmp.jpg",
                                    contentType = "image/jpeg")
                             }, deleteFile = TRUE)

                             output$stats <- renderUI({
                               h4(paste(tot_no, " | ", tot_yes, " - ", yes_pct, "%", sep=""))
                             })

              })

    })



