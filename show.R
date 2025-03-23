library(tidyverse)
library(lubridate)
library(shiny)
library(httr)
library(shinyjs)

# 读取数据
data <- read_csv("github_issue_comments.csv", show_col_types = FALSE)

# 转换时间格式
data$time <- ymd_hms(data$time)

data = data |> 
  mutate(likes = 0)

ui <- fluidPage(
  useShinyjs(),  # 启用 shinyjs
  tags$head(
    tags$style(HTML("
      .grid-container {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        grid-auto-rows: 300px;
        grid-gap: 10px;
        justify-content: center;
        margin-top: 5em;
        max-width: 1200px; /* 设置最大宽度，可以根据需要调整 */
        margin: 5em auto 0 auto; /* 左右留白，顶部留白 */
      }
      .grid-item {
        border: 1px solid #ddd;
        padding: 10px;
        overflow: hidden;
      }
      .grid-item img {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      .floating-title {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        background-color: rgba(255, 255, 255, 0.8);
        padding: 1px;
        text-align: center;
        z-index: 1000;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        $(window).scroll(function() {
          if ($(window).scrollTop() >= $(document).height() - $(window).height() - 100) {
            Shiny.setInputValue('loadMore', true, {priority: 'event'});
          }
        });
      });
    "))
  ),
  div(class = "floating-title", h2("网页截图展示")),
  div(id = "screenshot_gallery", class = "grid-container")
)

server <- function(input, output, session) {
  if (!("likes" %in% names(data))) {
    data$likes <- 0
  }

  start_index <- reactiveVal(1)
  end_index <- reactiveVal(10)

  observe({
    sorted_data <- data %>%
      arrange(time)

    current_page_data <- sorted_data[start_index():end_index(), ]

    screenshot_list <- lapply(1:nrow(current_page_data), function(i) {
      div(
        h4(current_page_data$author[i]),
        if (!is.null(current_page_data$link[i])){
        a(href = current_page_data$link[i],
          if (!is.null(current_page_data$screenshot_path[[i]])) {
          img(src = current_page_data$screenshot_path[[i]], alt = current_page_data$content[i])
          }
        )},
        actionButton(paste0("like_", current_page_data$id[i]), "点赞"),
        textOutput(paste0("likes_", data$id[i])),
        class = "grid-item"
      )
    })

    insertUI(
      selector = "#screenshot_gallery",
      where = "beforeEnd",
      ui = tagList(screenshot_list),
      immediate = TRUE
    )
  })

  observeEvent(input$loadMore, {
    start_index(end_index() + 1)
    end_index(min(end_index() + 10, nrow(data)))
  })

  observe({
    for (i in 1:nrow(data)) {
      local({
        my_i <- i
        observeEvent(input[[paste0("like_", data$id[my_i])]], {
          data$likes[my_i] <- data$likes[my_i] + 1
          output[[paste0("likes_", data$id[my_i])]] <- renderText({
            paste("点赞数:", data$likes[my_i])
          })

          tryCatch({
            gh_token <- Sys.getenv("GITHUB_PAT")
            comment_id <- str_extract(data$content_url[my_i], "(?<=comments/).*")
            gh_api_url <- paste0("https://api.github.com/repos/D2RS-2025spring/myHomePages/issues/comments/", comment_id)
            gh_headers <- c("Authorization" = paste0("token ", gh_token))
            gh_body <- list(body = paste("点赞数:", data$likes[my_i]))
            gh_response <- PATCH(gh_api_url, add_headers(gh_headers), body = gh_body, encode = "json")
            if (http_error(gh_response)) {
              showNotification(paste("更新 GitHub 评论失败：", status_code(gh_response)), type = "error")
            }
          }, error = function(e) {
            showNotification(paste("GitHub API 错误：", e$message), type = "error")
          })
        })
      })
    }
  })
}

shinyApp(ui = ui, server = server)