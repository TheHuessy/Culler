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




parse_csrf <- function(cookie_table){
  output <- cookie_table[which(cookie_table$name == "csrftoken"),7]
  return(output)
}

set_insta_session <- function(full_url){
  time_rn <- round(as.numeric(as.POSIXct(Sys.time())),0)

  response_data <- GET(full_url)
  csrf <- parse_csrf(response_data$cookies)
  #login_link <- response_data$url
  login_link <- "https://www.instagram.com/accounts/login/"

  post_body <- list(
                    username = creds$un_insta,
                    enc_password = paste('#PWD_INSTAGRAM_BROWSER:0:{', time_rn, '}:', creds$pw_insta, sep = ""),  # <-- note the '0' - that means we want to use plain passwords
                    optIntoOneTap = 'false'
  )

  post_headers <- c(
                    'user-agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.198 Safari/537.36",
                    'x-requested-with' = "XMLHttpRequest",
                    referer = login_link,
                    'x-csrftoken' = csrf
  )

  auth_post <- POST(url = paste(login_link, "ajax/", sep = ""), body = post_body, add_headers(post_headers))

  if ("sessionid" %in% auth_post$cookies$name) {
    print("Reauthenticated!")
    ######### CHECK TO SEE IF RUNNING THE PAGE FROM HERE MAINTAINS A SESSION ###############
    page_retry <- content(GET(full_url, add_headers('x-csrftoken' =  parse_csrf(auth_post$cookies))))
    if (is.null(page_retry$graphql$shortcode_media$display_url) == FALSE){
      return(page_retry)
    } else {
      print("Reuathentication did not work")
      return(page_retry)
    }
  } else {
    print("Not able to authenticate instagram!")
    return(auth_post)
  }

}

reauthenticate <- function(full_url){
  #no_api <- gsub("\\?__a=1", "", full_url)
  page_data <- set_insta_session(full_url)
  #first_hit <- GET(no_api)
  #Sys.sleep(2)
  #page_data <- content(GET(full_url))
  if (is.null(page_data$graphql$shortcode_media$display_url) == TRUE){
    return("https://www.imgur.com/thispageisactuallydead")
  } else{
    return(page_data$graphql$shortcode_media$display_url)
  }
}

insta_fresh <- function(piece){

  full_url <- paste("https://www.instagram.com", piece, "?__a=1", sep = "")

  ## If response is bad [define in a little bit], run set_insta_sesson and then retry  

  page_data <- content(GET(full_url))

  if (is.null(page_data$graphql$shortcode_media$display_url) == TRUE){
    print("reauthenticate!")
    return(reauthenticate(full_url))
  } else {
    return(page_data$graphql$shortcode_media$display_url)
  }
}

get_cnt_safe <- function(work, cnt){

  #### Maybe have it just evaluate if the count is higher than the tot value, then do all the GET testing in get_link
  new_cnt <<- as.numeric(cnt) + 1


  if (new_cnt > nrow(work)){
    pull_new_cohort(new_cnt-1)
    new_cnt <<- 1
  }
  while (TRUE){
    ## Catch if the last test pushed the cnt number over the number of rows in the cohort
    if (new_cnt > nrow(work)){
      pull_new_cohort(new_cnt - 1)
      new_cnt <<- 1
    }

    next_ext_bool <<- file_ext(work[new_cnt,2]) %in% c("mp4", "mkv", "gif", "avi", "m4v", "m4p", "mpg")
    if (next_ext_bool == TRUE){

      # Set this URL's keep value to 0
      work[new_cnt,3] <<- 0 
      tot_no <<- tot_no + 1

      # Advance one more
      new_cnt <<- new_cnt + 1
    } else {

      tester <<- get_link(new_cnt)

      url_check <<- try(status_code(GET(tester)), silent = TRUE)

      if (url_check == 200){
        break
      } else {
        work[new_cnt,3] <<- 9
        tot_no <<- tot_no + 1
        new_cnt <<- new_cnt + 1
      }
    }
  }
  return(c(new_cnt, tester))
}

save_file <- function(work, cnt){

  sql_con <<- dbConnect(sql_driver,
                       host=creds$pg_host,
                       user=creds$pg_user,
                       password=creds$pg_pw,
                       dbname="strobot"
  )

  for (idx in (as.numeric(cnt)-5):(as.numeric(cnt)-1)){
    link <- work[idx,2]
    new_keep <- work[idx,3]
    tbl_name <- work[idx,4]
    ################################################
   # print("saving at row:")
   # print(idx)
   # print("===================")
   # print(work[idx,])
   # print("===================")
    ################################################
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
    
    ###################################
    print(update_string)
    ###################################

    dbExecute(sql_con, update_string)
  }
  dbDisconnect(sql_con)
  last_saved <<- as.numeric(cnt)-1
}


get_link <- function(cnt){
  test_link <- work[cnt,2]

  #########
 # print(paste("CNT:", cnt))
 # print(paste("Generated: Link:", test_link))
 # print("====================")
 # print(work[cnt,])
 # print("====================")
  ##########
  

  test <- grep(pattern="/p/", x=test_link)
  if (length(test) == 0){
    output_link <- work[cnt,2]
  } else {
    output_link <- insta_fresh(work[cnt,2])
  }

  ########
 # print(paste("Outputed Test Link:", output_link))
  ########
  

  return(output_link)
}

pull_new_cohort <- function(cnt){
  print(paste("going into pull new cohort, last_saved value =", last_saved))
  save_diff <<- as.numeric(cnt) - last_saved

  if (save_diff != 1){
    sql_con <<- dbConnect(sql_driver,
                          host=creds$pg_host,
                          user=creds$pg_user,
                          password=creds$pg_pw,
                          dbname="strobot"
    )

    for (idx in (last_saved):(as.numeric(cnt)-1)){
        ##########################
     # print("pull new cohort work object row:")
     # print(work[idx,])


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
      print(update_string)
      dbExecute(sql_con, update_string)
    }
    dbDisconnect(sql_con)
  }

  work <<- pull_data()
  tot_left <<- pull_total()
  last_saved <<- 0

  # cnt <<- get_cnt_safe(work,0)
#  cnt <<- 1

  # last_saved <<- 0
  tot_yes <<- 0
  tot_no <<- 0
  yes_pct <<- ""
  tot <<- nrow(work)
#  if (tot == 0){
#    img <- image_read(end_img) %>%
#      image_write("tmp.jpg")
#  } else{
#    img_link <<- cnt[2]
#    print(paste("Image link to be read as image:", img_link))
#    img <<- image_read(img_link) %>% 
#      image_write("tmp.jpg")
#  }

}

end_img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Thats_all_folks.svg/1019px-Thats_all_folks.svg.png"

cnt <<- get_cnt_safe(work,0)

shinyServer(function(input, output, session) {
              ###### APP DEPENDANT FUNCTIONS ######a
              save_if_5 <- function(cnt) {

                if ((as.numeric(cnt)-1) %% 5 == 0 && (as.numeric(cnt)-1) != 0){
                  save_file(work, cnt)
                  output$ticker <- renderUI({
                    h4(paste(cnt, " of ", tot, " * (", tot_left, ")", sep = ""))
                  })
                } else {
                  output$ticker <- renderUI({
                    h4(paste(cnt, " of ", tot, " (", tot_left, ")", sep = ""))
                  })
                }
              }


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
                img_link <<- cnt[2]
                img <- image_read(img_link) %>%
                  image_write("tmp.jpg")
              }


              output$image_output <- renderImage({
                list(src = "tmp.jpg",
                     contentType = "image/jpeg")
              }, deleteFile = TRUE)


              output$ticker <- renderUI({
                h4(paste(cnt[1], " of ", tot, " (", tot_left, ")", sep = ""))
              })

              output$stats <- renderUI({
                h4(paste(tot_no, " | ", tot_yes, " - ", yes_pct, "%", sep=""))
              })

              observeEvent(object_swipe(), {

                             #### ASSIGN KEEP VALUE ####

                             if (object_swipe() == "left"){
                               work[as.numeric(cnt[1]),3] <<- 0

                               #########################
                          #     print(paste("NO to", cnt[2]))
                               ########################


                               tot_no <<- tot_no + 1
                             } else if (object_swipe() == "right") {
                               work[as.numeric(cnt[1]),3] <<- 1

                               #########################
                           #    print(paste("NO to", cnt[2]))
                               ########################


                               tot_yes <<- tot_yes + 1
                             }

                             cnt <<- get_cnt_safe(work,as.numeric(cnt[1]))

                             #####################################################
                            # print("Newly generated cnt value:")
                            # print(cnt)
                             ######################################################

                             yes_pct <<- round(tot_yes/as.numeric(cnt[1]), digits = 3)*100

                             save_if_5(as.numeric(cnt[1]))

                             if (as.numeric(cnt[1]) > tot){

                               pull_new_cohort(as.numeric(cnt[1]))

                             } else {
                               ## Update image and resave tmp
                               img_link <<- cnt[2]
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

                             #print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
                             #print("END OF SWIPE ACTION")
                             #print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")


              })

      })



