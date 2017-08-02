###### TD CLASS IMPLEMENTATION

#' R6 class for Trends growth description
#' td = Trends Description
#'
td <- R6::R6Class(
    'td',
    inherit = tg,

    public = list(

        POS_RANGE = 4:20, # positions with special description
        NO_GROWTH = c( -0.099999, 0.099999), #growth range assumed to be zero or no growth

        data_period = 'month', # month, quarter, year etc
        data_end = NULL,       # vector in form c(year, month) representing the end of timeseries
        data_end_date = NULL,  # data_end cast in Date format: as.Date( year, month, 1)
        data_end_prv = NULL,   # one period (month/quarter/year) before data_end also in vector form c( year, month)
        data_end_prv_date = NULL, # data_end_prv expressed in Date format
        data_mtm = NULL, # growth reprensenting consecutive month-to-month or quarter-on-quarter or year-to-year
        data_yoy = NULL, # growth representing year on year (or comapred with same period previous year)
        data_name = NULL, # name used to describe the series


        initialize = function(x, x_name = NULL, x_start=NULL, x_frq = NULL, db_limit = list(yr=NULL, mth=12 ),  db_name = NULL ){

            super$initialize(x = x, x_start= x_start, x_frq = x_frq, db_limit = db_limit, db_name = db_name  )

            self$set_period( self$data_freq )

            self$set_end_properties()

            self$set_data_name( x_name )


            self$data_yoy <- switch( as.character( self$data_freq ),
                                     '1' = self$get_growth_data('yy1'),
                                     '4' = self$get_growth_data('qq4'),
                                     '12' = self$get_growth_data('mm12'),
                                     NULL
            )

            self$data_mtm <- switch( as.character( self$data_freq ),
                                     '1' = self$get_growth_data('yy1'),
                                     '4' = self$get_growth_data('qq1'),
                                     '12' = self$get_growth_data('mm1'),
                                     NULL
            )

        }

        ,set_period = function(value){

            if( !is.null( value ) ){

                self$data_period <- switch( as.character( value ),
                                            "1" = 'year',
                                            "4" = 'quarter',
                                            "12" = 'month',
                                            "260" = 'daily'
                )
            }
            invisible( self )
        }

        ,get_period = function(){
            return( self$data_period )
        }

        ,set_data_name = function(value){

            if( !is.null( value )){

                self$data_name <- value

            }else{

                if(is.character( self$data_raw) ){
                    self$data_name <- self$data_raw
                }else{
                    self$data_name <- "The timeseries"
                }
            }
            invisible( self )
        }

        ,get_data_name = function(){

            return( self$data_name )

        }

        ,set_case = function(txt, i=1){

            case_name <- switch( i,
             "1" = paste0( toupper( substr(txt, 1, 1) ),  substr(txt, 2, nchar(txt))   ),
             "2" = self$to_proper_case(txt),
             "3" = tolower( txt ),
             "4" = toupper( txt )
            )
          return(case_name)
        }

        ,set_end_properties = function(){

            if( !is.null( self$data_ts )){

                my_end <- stats::end( self$data_ts )
                my_frq <- stats::frequency( self$data_ts )

                self$data_end <- my_end
                if( my_frq == 4 ){

                    e_date <- as.Date( paste( my_end[1], 3 * my_end[2], 1, sep = '-' ) )
                    self$data_end_date <- e_date


                    lubridate::month( e_date ) <- lubridate::month( e_date ) - 3
                    self$data_end_prv <- c( lubridate::year(e_date), lubridate::month( e_date )/3 )
                    self$data_end_prv_date <- e_date



                }else if( my_frq == 12 ){

                    e_date <- as.Date( paste( my_end[1], my_end[2], 1, sep = '-' ) )
                    self$data_end_date <- e_date


                    lubridate::month( e_date ) <- lubridate::month( e_date ) - 1
                    self$data_end_prv <- c( lubridate::year( e_date ), lubridate::month( e_date) )
                    self$data_end_prv_date <- e_date



                }else if(my_frq == 1 ){

                    self$data_end_date <- as.Date( paste( my_end[ 1 ], 12, 1, sep = '-') )
                    self$data_end_prv <- c( my_end[1]- 1, 1)
                    self$data_end_prv_date <- as.Date( paste( my_end[ 1 ] - 1, 12, 1, sep = '-'))

                }else {

                    ## TO BE IMPLEMENTED ##

                }#other frequencies



            }# ! ts==null
            invisible( self )
        }


        ,get_data_end = function(){
            return( self$data_end )
        }

        ,get_prv_date = function(){
            return( self$data_end_prv_date )
        }

        ,get_end_date = function(){
            return( self$data_end_date )
        }

        ,get_position = function(x){

            if( (x %in% self$POS_RANGE) ){

                return ( paste0(x,'th') )

            }else{

                switch(x %% 10,

                       return( paste0(x,'st')), #case 1
                       return( paste0(x,'nd')), #case 2
                       return( paste0(x,'rd'))  #case 3
                )
            }

            return( paste0( x, 'th' ) )  #default
        }

        ,get_successive = function(is_yoy = TRUE){

            MTM_POS_COUNTER = 20

            mtm_data <- NULL

            if(is_yoy){
                mtm_data <- self$data_yoy
            }else{
                mtm_data <- self$data_mtm
            }


            mtm_data <- tseries::na.remove(mtm_data)
            mtm_len <- base::length(mtm_data)

            mtm_pos <- as.character( seq(1:MTM_POS_COUNTER) )
            mtm_sc_msg <- ""
            mtm_sc <- ( mtm_data[ mtm_len ] > 0 )
            mtm_sc_text <- "growth"
            mtm_sc_text_rev <- "decline"

            if( mtm_sc == FALSE ){
                mtm_sc_text <- "decline"
                mtm_sc_text_rev <- "growth"
            }

            mtm_checks <- ( mtm_data > 0 )

            i <- mtm_len - 1
            mtm_counter <- 1

            while(( mtm_checks[i] == mtm_sc) && (i>0) ){
                mtm_counter <- mtm_counter + 1
                i <- i - 1
            }

            mtm_counter2 <- NULL
            if( mtm_counter == 1 ){
                mtm_sc2 <- ( mtm_data[ mtm_len -1 ] > 0 )

                i <- mtm_len-2
                mtm_counter2 <- 1

                while( ( mtm_checks[ i ] == mtm_sc2 ) && ( i > 0 ) ){
                    mtm_counter2 <- mtm_counter2 + 1
                    i <- i - 1
                }
            }

            prd <- paste0( self$data_period, "ly ")

            if(!( self$data_freq == 1) ){

                prd <- ifelse( is_yoy, "year on year ", "month on month ")

            }

            if(mtm_counter>1){

                mtm_sc_msg <- paste0(
                    "It is the ",
                    self$get_position( mtm_counter ),
                    " successive ",
                    prd,
                    mtm_sc_text
                )

            }else{

                if( mtm_counter2 > 1 ){

                    mtm_sc_msg <- paste0(
                        "It is the first ",
                        prd,
                        mtm_sc_text,
                        " after ",
                        mtm_counter2,
                        " successive ",
                        mtm_sc_text_rev,
                        "s"
                    )

                }#if
            }#else

            return(mtm_sc_msg)

        }
        ,get_mtm = function(  is_yoy = FALSE ){

            mtm_data <- NULL

            if(is_yoy){
                mtm_data <- self$data_yoy
            }else{
                mtm_data <- self$data_mtm
            }


            mtm_len <- base::length( mtm_data )
            mtm_target <- mtm_prv <- NULL
            mtm_target <- self$data_end_date
            mtm_prv <- self$data_end_prv_date


            mtm_target_text <- paste0( as.character( lubridate::month( mtm_target, label=TRUE, abbr = FALSE)), " ", lubridate::year( mtm_target))
            mtm_prv_text <- paste0( as.character( lubridate::month( mtm_prv, label=TRUE, abbr=FALSE ))," ",lubridate::year( mtm_prv ))



            mtm_window <- window(
                mtm_data
                ,start=  self$data_end_prv
                ,end= self$data_end
            )


            mtm_discard_sc <- FALSE
            mtm_direction <- paste0("grew by ", beamaUtils::set_decimal(mtm_window[2], 1),"%")

            if(mtm_window[2]< 0 ){
                mtm_direction <- paste0("fell by ", beamaUtils::set_decimal( mtm_window[2], 1 ), "%")
            }

            if(mtm_window[2] >= self$NO_GROWTH[1] && mtm_window[2] <= self$NO_GROWTH[2] ){
                mtm_direction <- paste0( "remained unchanged" )
                mtm_discard_sc <- TRUE
            }


            mtm_broadcast <- paste0(
                "Between ", mtm_prv_text," and ", mtm_target_text," ",
                self$set_case ( self$data_name,3), " ",
                mtm_direction
            )

            return(
                list(
                    desc = mtm_broadcast,
                    MoM =  beamaUtils::set_decimal( mtm_window[2],1),
                    extra = mtm_discard_sc
                )
            )


        }

        ,get_yoy = function( is_yoy = TRUE ){


            yoy_data <- NULL

            if( is_yoy ){

                yoy_data <- self$data_yoy

            }else{

                yoy_data <- self$data_mtm

            }

            yoy_len <- base::length(yoy_data)

            yoy_target<- self$data_end_date
            yoy_prv <- self$data_end_prv_date



            yoy_target_text <- paste0( as.character( lubridate::month( yoy_target, label = TRUE, abbr = FALSE) )," ", lubridate::year( yoy_target ))
            yoy_prv_text <-  paste0( as.character( lubridate::month( yoy_prv, label = TRUE, abbr = FALSE ))," ",lubridate::year( yoy_prv ))

            yoy_window <- NULL

            yoy_window <- window(

                yoy_data,
                start = self$data_end_prv,
                end = self$data_end
            )

            #return(yoy_window)

            yoy_discard_extra <- FALSE
            yoy_direction <- "grew by"
            yoy_direction_prv <- "up from"

            if( yoy_window[2] < 0 ){ yoy_direction <- "fell by" }
            if( yoy_window[2] < yoy_window[ 1 ] ){ yoy_direction_prv <- "down from"}

            if( yoy_window[ 2 ] >= self$NO_GROWTH[1] && yoy_window[ 2 ]<= self$NO_GROWTH[2] ){

                yoy_direction <- "remained unchanged"
                yoy_discard_extra <- TRUE
            }

            extra <- paste0(
                ", ",
                yoy_direction_prv, " ",
                beamaUtils::set_decimal ( yoy_window[ 1 ], 1), "% in ",
                yoy_prv_text
            )

            yoy_broadcast <- paste0(
                self$set_case( self$data_name,2 )," ", #stringr::str_to_title
                yoy_direction, " ",
                ifelse( yoy_discard_extra, "", paste0( beamaUtils::set_decimal( yoy_window[ 2 ], 1 ), "%")),
                " in the year to ",
                yoy_target_text,
                ifelse( yoy_discard_extra,"",extra)
            )

            return(
                list(
                    desc = yoy_broadcast,
                    YoY = beamaUtils::set_decimal( yoy_window[2], 1),
                    extra = yoy_discard_extra
                )
            )

        }

        ,get_growth_desc = function( is_yoy = TRUE){

            my_desc_mtm <- self$get_mtm(  is_yoy = !is_yoy )$desc
            my_desc_yoy <- self$get_yoy(  is_yoy = is_yoy )$desc
            my_desc_sc_mtm <- self$get_successive( is_yoy = !is_yoy )
            my_desc_sc_yoy <-  self$get_successive( is_yoy = is_yoy )


            my_desc <- sprintf( "%s. %s. %s. %s.", my_desc_mtm, my_desc_yoy, my_desc_sc_mtm, my_desc_sc_yoy )
            return( gsub( ". .", ".", my_desc, fixed = T ) )

        }

        ,get_growth_since = function( dt1 = '2016-06-22', dt2 = NULL, by = c('y','m','q','d'), is_brexit = F){

            my_ts <- self$data_ts
            my_dt1 <- as.Date( dt1 )
            my_dt2 <- NULL
            my_data <- NULL

            my_by <- match.arg( by )

            s_yr1 <- lubridate::year( my_dt1)
            s_mth1 <- lubridate::month( my_dt1 )
            s_day1 <- lubridate::day( my_dt1 )
            s_qtr1 <- ceiling( s_mth1 / 3)

            s_yr2 <- s_mth2 <- s_day2 <- s_qtr2 <- NULL
            growth_since <- growth_template <- NULL

            if( !is.null( dt2 )){

                my_dt2 <- as.Date( dt2 )
                s_yr2 <- lubridate::year( my_dt2)
                s_mth2 <- lubridate::month( my_dt2 )
                s_day2 <- lubridate::day( my_dt2 )
                s_qtr2 <- ceiling( s_mth2 / 3)

                growth_template <- switch(
                    my_by,
                    'y' = "%s between %s and %s is %s",
                    'm' = "%s between %s %s and %s %s is %s",
                    'q' = "%s between %s Q%s and %s Q%s is %s",
                    'd' = "%s between %s and %s is %s"
                )

            }else{

                growth_template <- switch(
                    my_by,
                    'y' = "%s since %s by %s",
                    'm' = "%s since %s %s by %s",
                    'q' = "%s since %s Q%s by %s",
                    'd' = "%s since %s by %s"
                )
            }

            s_win1 <- s_win2 <- NULL

            if( is_brexit){

                s_win1 <- switch(
                  my_by,
                  'y' = c(2015,1),
                  'q' = c(2016,1),
                  'm' = c(2016,5),
                  'd' = as.Date('2016-06-22')
                )
            }else{

                s_win1 <- switch(
                    my_by,
                    'y' = c( s_yr1 , 1),
                    'q' = c( s_yr1 , s_qtr1 ),
                    'm' = c( s_yr1 , s_mth1 ),
                    'd' = my_dt1
                )

            }



            if( !is.null( dt2 )){

                s_win2 <- switch(
                    my_by,
                    'y' = c( s_yr2 , 1),
                    'q' = c( s_yr2 , s_qtr2 ),
                    'm' = c( s_yr2 , s_mth2 ),
                    'd' = my_dt2
                )

            }


            growth_text <- ''

            if(my_by == 'm'){

                if(self$data_freq == 12){

                    if( is.null( dt2 )){
                        my_data <- window( my_ts, s_win1)
                    }else{
                        my_data <- window( my_ts, s_win1, s_win2)
                    }

                    if( !is.null( my_data ) || is.na( my_data )){

                        n_items <- length( my_data )
                        growth_since <- (my_data[ n_items ] - my_data[ 1 ])/my_data[ 1 ]*100

                        if( is.null( dt2 )){

                            growth_text <- sprintf(
                                growth_template,
                                ifelse(growth_since > 0, "Growth", "Decline" ),
                                month.abb[ s_mth1 ],
                                s_yr1,
                                paste0( beamaUtils::set_decimal( growth_since, 1), "%")
                            )

                        }else{

                            growth_text <- sprintf(
                                growth_template,
                                ifelse(growth_since > 0, "Growth", "Decline" ),
                                month.abb[ s_mth1 ],
                                s_yr1,
                                month.abb[ s_mth2 ],
                                s_yr2,
                                paste0( beamaUtils::set_decimal( growth_since, 1), "%")
                            )

                        }#s_win2 not null

                }#data not null
               }#freq = 2
            }#by == m

            if(my_by == 'q'){

                if(self$data_freq == 4){

                    if( is.null( dt2 )){
                        my_data <- window( my_ts, s_win1)
                    }else{
                        my_data <- window( my_ts, s_win1, s_win2)
                    }

                    if( !is.null( my_data ) || is.na( my_data )){

                        n_items <- length( my_data )
                        growth_since <- (my_data[ n_items ] - my_data[ 1 ])/my_data[ 1 ]*100

                        if( is.null( s_win2 )){
                            growth_text <- sprintf(
                                growth_template,
                                ifelse(growth_since > 0, "Growth", "Decline" ),
                                s_yr1,
                                s_qtr1,
                                paste0( beamaUtils::set_decimal( growth_since, 1), "%")
                            )

                        }else{
                            growth_text <- sprintf(
                                growth_template,
                                ifelse(growth_since > 0, "Growth", "Decline" ),
                                s_yr1,
                                s_qtr1,
                                s_yr2,
                                s_qtr2,
                                paste0( beamaUtils::set_decimal( growth_since, 1), "%")
                            )

                        }
                    }# null data

                }#freq ==4
            }#is qtr



            return( list( txt = growth_text, x = growth_since) )
        }
        ,get_hilow  = function( k = 1, is_low = TRUE, dp = 1 ){

            my_data <- td.delt(self$data_ts, k = k, percent = T, dp = dp)
            #my_data <- td.delt(onsR2::download('k646')$m_data, k = k, percent = T, dp = dp)
            my_data_len <- length( my_data)
            my_data_last <- my_data[ my_data_len]
            my_data_logic <- NULL
            my_template <- NULL
            my_text <- ''


            if(is_low ){
                my_data_logic <- (my_data <= my_data_last)

                if( self$data_freq == 12 ){
                    my_template <- switch(
                        as.character(k),
                        "1" = "It is the lowest 1-month growth since %s %s",
                        "12" = "It is the lowest 12-month growth since %s %s"
                    )
                }else if(self$data_freq == 4){

                    my_template <- switch(
                        as.character(k),
                        "1" = "It is the lowest 1-quarter growth since %s Q%s",
                        "4" = "It is the lowest 4-quarter growth since %s Q%s"
                    )

                }
            }else{
                my_data_logic <- (my_data >= my_data_last)

                if( self$data_freq == 12 ){
                    my_template <- switch(
                        as.character(k),
                        "1" = "It is the highest 1-month growth since %s %s",
                        "12" = "It is the highest 12-month growth since %s %s"
                    )
                }else if(self$data_freq == 4){

                    my_template <- switch(
                        as.character(k),
                        "1" = "It is the highest 1-quarter growth since %s Q%s",
                        "4" = "It is the hihgest 4-quarter growth since %s Q%s"
                    )

                }

            }

            my_data_df <- beamaUtils::ts_to_df( my_data_logic )
            my_data_df <- my_data_df[ -c( nrow( my_data_df)),  ]

            names( my_data_df ) <- c('dt','value','mth','yr')
            my_data_filtered <- dplyr::filter( my_data_df, value == TRUE)
            n_rows <- nrow( my_data_filtered )

            if( n_rows > 0 ){
                my_since <- my_data_filtered [ n_rows, ]

                if( self$data_freq == 12 ){

                    my_text <- sprintf( my_template, my_since$yr[ 1 ], month.abb[ my_since$mth[ 1] ] )

                }else if(self$data_freq == 4){

                    my_text <- sprintf( my_template, my_since$yr[ 1 ], my_since$mth[ 1]/3 )

                }
            }

            return( my_text )

        }
        ,get_hilows = function(){

           hl <- " %s \n %s \n %s \n %s"
           if( self$data_freq == 12){

               return(
                cat(sprintf(
                    hl,
                    self$get_hilow( k = 1),
                    self$get_hilow( k = 12),
                    self$get_hilow( k = 1, is_low = F),
                    self$get_hilow( k = 12, is_low = F)
                ))
              )

           }else if( self$data_freq == 4 ){

               return(
                   cat(
                   sprintf(
                       hl,
                       self$get_hilow( k = 1),
                       self$get_hilow( k = 4),
                       self$get_hilow( k = 1, is_low = F),
                       self$get_hilow( k = 4, is_low = F)
                   )
                 )
             )
           }
           return('')
        }

    ),
    private = list()

)


######TD CLASS IMPLEMENTATION ENDS

td.delt <- function(x, k= 1 , percent = FALSE, dp = NULL){

    if( !is.ts( x)){
        return(NULL)
    }
    factor <- 1
    if(percent){ factor <- 100 }
    x_start <- stats::start( x )
    x_freq <- stats::frequency( x )

    if( is.null(dp) ){
        return(
            stats::ts(
                quantmod::Delt( c( x ), k = k ) * factor,
                start = x_start,
                frequency = x_freq
            )
        )

    }else{

        return(
            round(
                stats::ts(
                    quantmod::Delt( c( x ), k = k ) * factor,
                    start = x_start,
                    frequency = x_freq
                ),
                digits =  dp
           )
        )

    }

}
td.delt_code <- function(x, k= 1 , percent = T, dp = 1){
    td.delt( tg$new( x )$data_ts, k = k, percent = percent, dp = dp )
}
