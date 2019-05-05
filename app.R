#mlb highlights - source from new api

library(shiny)
library(DT)
library(magrittr)
library(data.table)
library(lubridate)
library(jsonlite)
library(curl)
library(shinythemes)

get_schedule <- function(x){#x is date yyy-mm-dd

	# https://statsapi.mlb.com/api/v1/schedule?sportId=1&date=2018-10-28
	scrape_page  <-  curl_fetch_memory(paste0("http://statsapi.mlb.com/api/v1/schedule?sportId=1&date=", x))

	parse_results <- fromJSON(rawToChar(scrape_page$content))

	game_table <- as.data.table(parse_results$dates$games)

	games <- game_table[, .(
	gamePk,
	date = x,
	gameType
	)]

	away_teams <- game_table$teams[[1]]$team
	home_teams <- game_table$teams[[2]]$team

	#add game status and time if not started
	game_times <- game_table$gameDate %>% ymd_hms() %>% with_tz("America/New_York") %>% format("%I:%M %p") %>% as.data.table %>% setnames("time")

	game_status <- game_table$status[3]

	#create table
	games_output <- c(games, away=away_teams[2], home = home_teams[2], game_times, status = game_status) %>% as.data.table

	games_output[status %like% "Final|Progress", game:= paste0(away.name, " @ " , home.name, " (", status, ")")]
	games_output[!(status %like% "Final|Progress"), game:= paste0(away.name, " @ " , home.name, " (", time, ")")]

	return(games_output)

}

# games_today <- get_schedule(Sys.Date())

#to get vids
scrape_by_gamePk <- function(gamePk){#lapply w/vector of games

	game_url <- paste0("http://statsapi.mlb.com/api/v1/game/", gamePk, "/content")

	#need error catch?
	scrape_output <- curl_fetch_memory(game_url)

	parsed_content <- fromJSON(rawToChar(scrape_output[["content"]]))

	video_links_table <- as.data.table(parsed_content$highlights$highlights$items)

	#NOTE - some url not coded with date correctly, pull url from source rather than create; 3/31/19 - brewer game - dates = 2/31/19

	if (nrow(video_links_table)>0){
		video_table_output <- video_links_table[, .(
			gamePk,
			# mediaPlaybackId,
			headline,
			blurb)]

		#find link type
		#NOTE - 05/04/2019 has 3 types now -  _4000K.mp4; m3u8; _16000K.mp4
		link_urls <- video_links_table$playbacks %>% lapply(., function(x) x$url %>% unique %>% t() %>% as.data.table) %>% rbindlist(fill = TRUE, use.names = TRUE) #%>% setnames(c("mp4", "m3u8"))

		setnames(link_urls, 1, "mp4")
		setnames(link_urls, 2, "m3u8")

		final_output <- data.table(video_table_output, link_urls)

		#FUTURE - create combos to populate shiny selections too
		final_output[m3u8 %like% "bdata", Medium:= gsub(".m3u8", "_1200.m3u8", m3u8)]
		final_output[!(m3u8 %like% "bdata"), Medium:= gsub(".m3u8", "_640x360_29_1072K.m3u8", m3u8)]

	} else {NULL}

}

# list_of_game_videos <- lapply(c(565320), scrape_by_gamePk)

#final processing to prep data

final_data_prep <- function(list_of_game_videos, games_today){#list of game vid parse results and table of games from get_schedule

	#remove null results and rbind together
	game_videos <- Filter(Negate(is.null), list_of_game_videos) %>% rbindlist(use.names = TRUE, fill = TRUE)

	if(nrow(game_videos) > 0){
	#merge vids w/game data
	video_data <- merge(games_today, game_videos, by = "gamePk")

	return(video_data)
	} else {NULL}
}

#NOTE - move to shiny server
# data <- final_data_prep(list_of_game_videos, games_today)

#format link based on type
# format_link <- function(partial_link, link_type){#FUTURE - add quality

# 	#NOTE - looks like mp3u has different links based on bdata or cuts
# 	#m3u8 for cuts
# 	# asset_320x180_29_128K.m3u8
# 	# asset_384x216_29_450K.m3u8 #low
# 	# asset_512x288_29_736K.m3u8
# 	# asset_640x360_29_1072K.m3u8 #med
# 	# asset_896x504_29_1672K.m3u8
# 	# asset_960x540_29_2372K.m3u8 #high
# 	# asset_1280x720_29_3372K.m3u8
# 	# asset_1280x720_59_5472K.m3u8 #best

# 	#for bdata
# 	# _5600.m3u8 #best
# 	# _3500.m3u8
# 	# _2500.m3u8 #high
# 	# _1800.m3u8
# 	# _1200.m3u8 #med
# 	# _800.m3u8
# 	# _514.m3u8 #low
# 	# _192.m3u8

# 	#FUTURE - build out different combos

# 	# m3u8 - same for bdata or cuts
# 	# mp4- cuts - add - _1280x720_59_4000K
# 	if(substr(partial_link, 1, 5) == "bdata"){
# 		if(link_type == "mp4"){
# 			link <- paste0(partial_link, ".mp4")
# 		} else {#for m3u8 bdata
# 			link <- paste0(partial_link, "_1200.m3u8")}
# 	} else {#for cuts
# 		if(link_type == "mp4"){
# 			link <- paste0(partial_link, "_1280x720_59_4000K.mp4")
# 	} else {#for m3u8 cuts
# 			link <- paste0(partial_link, "_640x360_29_1072K.m3u8")
# 	}}
# 	return(paste0("https://", link))
# }

# ----

#set default date - use yesterday if in the morning
default_date <- if(as.numeric(format(Sys.time(), "%H")) < 12){
	Sys.Date() - 1
} else {Sys.Date()}
# ----

#start shiny ####
ui <- fluidPage(
	theme = shinytheme("slate"),
	titlePanel(paste0("MLB highlights")),

    sidebarLayout(
      sidebarPanel(
		dateInput("date_to_scrape", "Select date", value = default_date),

		# actionButton("find_games", "Find games"),

        uiOutput("game_output"),

		actionButton("go", "Retrieve videos"),

        radioButtons("qualityInput", "Video Quality",
			# choices = c("mp4", "Small", "Medium", "High", "Very High"),
			choices = c("mp4", "Medium"),
			selected = "Medium"),

        # uiOutput("team_output")

		# radioButtons("typeInput", "Video Type",
		# 	choices = c("All", "Recap", "Must C", "Condensed"),
		# 	selected = "All"),

			h6("Note- for non-mp4 videos, may need to install extension on desktop browsers: Native HLS Playback")

      ),
	mainPanel(DTOutput("results"))
	)
)

server <- function(input, output) {

	date_from_ui <- reactive(date(input$date_to_scrape))

	# games <- eventReactive(input$find_games, {
	games <- reactive(req(
		get_schedule(date_from_ui())
	))

	output$game_output <- renderUI({
	checkboxGroupInput("game_input", "Select Games (ET)",
	c("All", games()[, (game)]))
	})

	selected_games <- reactive(
		if("All" %in% input$game_input){
			games()[, (gamePk)]
		} else {
			games()[game %in% input$game_input, (gamePk)]
		})

	data <- eventReactive(input$go, {
		list_of_game_videos <- lapply(selected_games(), scrape_by_gamePk)

		final_data_prep(list_of_game_videos, games())
	})

	#FUTURE - add this
	# filter_by_type <- reactive(
	# 	if(input$typeInput %in% c("Recap", "Must C")){
	# 		data()[headline %like% input$typeInput]
	# 	} else if(input$typeInput == "Condensed"){data()[blurb %like% input$typeInput]
	# 		} else {data()}
	# )

	final <- reactive({
		if(is.null(data())==FALSE){
			# data()[, full_link:= lapply(link_partial, function(x) format_link(x, input$qualityInput))] %>% .[, .(
			data()[, .(
			game,
			headline,
			blurb,
			link = paste0("<a href=\"", get(input$qualityInput), "\" target=\"_blank\">"," Video </a>")
			)]
		} else {NULL}
	})

	output$results <- renderDT({
	DT::datatable(final(), escape = FALSE, rownames = FALSE, selection = "none", filter = 'top', class = "compact stripe", style = 'bootstrap', options = list(pageLength = 25)
	)
	})
}

#needs to be last line
# runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)

#needs to be last line for shiny server
shinyApp(ui = ui, server = server)
