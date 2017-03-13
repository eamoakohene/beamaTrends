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


        initialize = function(x, x_name = NULL, x_start=NULL, x_frq = NULL, db_limit = list(yr=NULL, mth=12) ){

            super$initialize(x = x, x_start= x_start, x_frq = x_frq, db_limit = db_limit  )

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

        },

        set_period = function(value){

            if( !is.null( value ) ){

                self$data_period <- switch( as.character( value ),
                                            "1" = 'year',
                                            "4" = 'quarter',
                                            "12" = 'month',
                                            "270" = 'daily'
                )
            }
            invisible( self )
        },

        get_period = function(){
            return( self$data_period )
        },

        set_data_name = function(value){

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
        },

        get_data_name = function(){

            return( self$data_name )

        },

        set_case = function(txt, i=1){

            case_name <- switch( i,
             "1" = paste0( toupper( substr(txt, 1, 1) ),  substr(txt, 2, nchar(txt))   ),
             "2" = self$to_proper_case(txt),
             "3" = tolower( txt ),
             "4" = toupper( txt )
            )
          return(case_name)
        },

        set_end_properties = function(){

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
        },


        get_data_end = function(){
            return( self$data_end )
        },

        get_prv_date = function(){
            return( self$data_end_prv_date )
        },

        get_end_date = function(){
            return( self$data_end_date )
        },



        get_position = function(x){

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
        },

        get_successive = function(is_yoy = TRUE){

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

        },

        get_mtm = function(  is_yoy = FALSE ){

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


        },

        get_yoy = function( is_yoy = TRUE ){


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

        },

        get_growth_desc = function( is_yoy = TRUE){

            my_desc_mtm <- self$get_mtm(  is_yoy = !is_yoy )$desc
            my_desc_yoy <- self$get_yoy(  is_yoy = is_yoy )$desc
            my_desc_sc <-  self$get_successive( is_yoy = is_yoy )
            my_desc <- sprintf( "%s. %s. %s.", my_desc_mtm, my_desc_yoy, my_desc_sc )
            return( my_desc )
        }

    ),
    private = list()

)


######TD CLASS IMPLEMENTATION ENDS
