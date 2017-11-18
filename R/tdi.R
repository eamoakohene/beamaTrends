tdi_G7 = 'uk,fr,de,it,jp,us,ca'

tdi.indicators <- list(

    cpi = 'TDI-INFLATION-RATE',
    gdpa = 'TDI-GDP-ANNUAL-GROWTH-RATE',
    gdp = 'TDI-GDP-GROWTH-RATE',
    gdp_spend = 'TDI-GOVERNMENT-SPENDING-TO-GDP',
    gdp_budget = 'TDI-GOVERNMENT-BUDGET',
    uemp = 'TDI-UNEMPLOYMENT-RATE',
    uemp_youth = 'TDI-YOUTH-UNEMPLOYMENT-RATE',
    uemp_persons = 'TDI-UNEMPLOYED-PERSONS',
    emp = 'TDI-EMPLOYMENT-RATE',
    emp_persons = 'TDI-EMPLOYED-PERSONS',
    retail = 'TDI-RETAIL-SALES-YOY',
    retail_mth = 'TDI-RETAIL-SALES-MOM',
    wages = 'TDI-WAGE-GROWTH',
    debt_pri = 'TDI-PRIVATE-DEBT-TO-GDP',
    debt_gov = 'TDI-GOVERNMENT-DEBT-TO-GDP',
    debt_hh = 'TDI-HOUSEHOLDS-DEBT-TO-GDP',
    ppi = 'TDI-PRODUCER-PRICES-CHANGE',
    retail = 'TDI-RETAIL-SALES-YOY',
    tax = 'TDI-CORPORATE-TAX-RATE',
    mpmi='TDI-MANUFACTURING-PMI',
    terror_idx = 'TDI-TERRORISM-INDEX',
    steel_prod = 'TDI-STEEL-PRODUCTION',
    employee_tax = 'TDI-SOCIAL-SECURITY-RATE-FOR-EMPLOYEES',
    employer_tax = 'TDI-SOCIAL-SECURITY-RATE-FOR-COMPANIES',
    social_tax = 'TDI-SOCIAL-SECURITY-RATE',
    srvs_pmi = 'TDI-SERVICES-PMI',
    rt_age_women = 'TDI-RETIREMENT-AGE-WOMEN ',
    rt_age_men = 'TDI-RETIREMENT-AGE-MEN',
    personal_savings = 'TDI-PERSONAL-SAVINGS',
    income_tax = 'TDI-PERSONAL-INCOME-TAX-RATE',
    mining_prod = 'TDI-MINING-PRODUCTION'


)

tdi = R6::R6Class(
    'tdi',
    inherit = tp_utils,

    public = list(

        code = NULL,
        sql = NULL,
        ddays = 0,
        show_ym = FALSE,
        country = NULL,
        ymd = c(0,0,0),


        initialize = function( code = NULL , country = 'uk',  ymd = c(0,0,0) , show_ym = FALSE){

            super$initialize( NULL )
            self$set_code( code )
            self$set_ymd( ymd )
            self$set_show_ym( show_ym )
            self$set_country( country )

        }

        ,set_code = function(value){

            if(!missing(value) && !is.null(value)){
                self$code <- value
            }
            invisible( self )
        }

        ,set_show_ym = function(value){

            if(!missing(value) && !is.null(value)){
                self$show_ym <- value
            }
            invisible( self )
        }

        ,set_country = function(value){

            if(!missing(value) && !is.null(value)){
                self$country <- value
            }
            invisible( self )
        }

        ,set_ymd = function(value){

            if(!missing(value) && !is.null(value)){
                self$ymd <- value
                self$ddays <- beamaUtils::ddays( self$ymd[1], self$ymd[2], self$ymd[3])
            }
            invisible( self )
        }

        ,get_sql = function(){

            my_sql <- NULL

            sql_fields <- "country, data_desc as indicator, data_value as value, data_unit as unit, tdi_ref as ref"

            if(self$show_ym){

                sql_fields <- "country, data_desc as indicator, data_value as value, data_unit as unit, tdi_ref as ref, yr, mth"
            }

            my_sql <- sprintf(
                "select %s from tdi_global_latest where trim(country) in %s and data_code='%s' order by country ",
                sql_fields, beamaUtils::split_str( self$country), self$code
            )

            #cat("Data days =", self$ddays,"\n")
            if(self$ddays > 0 ){
                my_sql <- sprintf(
                    "select %s from tdi_global_latest where trim(country) in %s and data_code='%s' and data_days = %s order by country",
                    sql_fields, beamaUtils::split_str( self$country), self$code, self$ddays
                )
            }



            return( my_sql )


        }

        ,get_data = function(){

            self$run_sql( self$get_sql() )
        }

        ,update_data_days = function(){

            cat("Updating data days ...\n")

            db <- self$get_db_name()
            storedQry::SQ$new( db )$set_name("tdi_global_update_data_days")$qry_exec()

            cat("All done folks \n")

        }



    ),
    private = list()
)

tdi.get_g7 <- function (indicator ='TDI-INTEREST-RATE', ymd=c(0,0,0),  show_ym = F){

    tdi$new()$set_code( indicator )$set_ymd( ymd )$set_show_ym( show_ym )$set_country( tdi_G7 )$get_data()

}

tdi.g7_inflation <- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$cpi, ymd = ymd, show_ym = show_ym )
}

tdi.g7_gdp <- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$gdp, ymd = ymd, show_ym = show_ym )
}

tdi.g7_unemp <- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$uemp, ymd = ymd, show_ym = show_ym )
}

tdi.g7_retail <- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$retail, ymd = ymd, show_ym = show_ym )
}

tdi.g7_wages<- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$wages, ymd = ymd, show_ym = show_ym )
}

tdi.g7_debt_gov<- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$debt_gov, ymd = ymd, show_ym = show_ym )
}

tdi.g7_debt_hh<- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$debt_hh, ymd = ymd, show_ym = show_ym )
}

tdi.g7_debt_pri<- function(ymd=c(0,0,0), show_ym = T){

    tdi.get_g7( tdi.indicators$debt_pri, ymd = ymd, show_ym = show_ym )
}

tdi.g7_data <- function( fxn){
    do.call( sprintf('tdi.g7_%s',fxn), list() )
}

tdi.g7_plot <- function( indicator = tdi.indicators$debt_gov, ymd =c(0,0,0), dp = 1, title = NULL, xlabel = '', ylabel = '', sort_rev = F, y_gap = 6 ){

    require(ggplot2)
    require(ggthemes)

    my_data <- NULL

    if( is.character( indicator)){

        my_data <- tdi.get_g7( indicator = indicator, ymd = ymd, show_ym = T )

    }else if( is.data.frame( indicator)){

        my_data <- indicator

    }



    if( nrow(my_data) > 0){

        if(! sort_rev){
            my_data <- dplyr::arrange( my_data, value)
        }else{
            my_data <- dplyr::arrange( my_data, desc(value) )
        }

        my_max <- max( abs(my_data$value))
        my_sign <- (my_data$value > 0 )
        my_mult <- ifelse( my_sign, 1, -1)

        my_data$country <- factor( toupper(my_data$country), levels = toupper(my_data$country))
        my_data$lbl_line <- my_data$value / 2
        my_data$ref_line <- my_data$value + y_gap * my_max /100 * my_mult

        my_data$lbl_value <- beamaUtils::set_decimal(my_data$value, dp)
        my_data$col <- ( toupper( my_data$country) == 'UK' )

        fill_colour <- NULL
        if(nrow( dplyr::filter(my_data,col==FALSE) ) == 0){

            fill_colour <-  c(beamaColours::get_euris_blue(), beamaColours::get_euris_blue() )

        }else{

            fill_colour <- c( beamaColours::get_euris_blue(), beamaColours::get_pink() )

        }
        my_title <- my_data$indicator[ 1 ]

        if( !is.null(title) ){
            my_title <- title
        }


        my_ytitle <- ylabel
        if(ylabel == ''){ my_ytitle <- my_data$unit }

        g <- ggplot( my_data,  aes(x = factor(country), y= value, fill = col) )
        g <- g + geom_bar(stat = "identity")
        g <- g + scale_fill_manual( values = fill_colour, guide = FALSE, name = "")
        g <- g +  coord_flip() + labs (x = '', y= my_ytitle, title = my_title )
        g <- g + geom_text( aes(x = factor( country ), y = lbl_line, label = lbl_value) , colour= "gray85",  size = 4, position=position_dodge(width=1) )
        g <- g + geom_text( aes(x = factor( country ), y = ref_line, label = ref) , colour= "gray70",  size = 3, position=position_dodge(width=1) )

        g <- g + theme_igray()
        g <- g + scale_colour_tableau("colorblind10")
        g <- g + theme(

            legend.position = "none",
            legend.title = element_blank(),
            text = element_text(family="Museo 300", face="plain", size = 14)

        )
        print(g)
        my_data

    }
}

tdi.commodity_get <- function(code = 'TDI-GOLD'){

    sql <- sprintf("select yr,mth,dy,data_value,data_code,data_desc from tdi_commodities where data_code='%s'",code)
    tp.run_sql( sql)

}

tdi.stock_get <- function(code = 'TDI-FTSE-100'){

    sql <- sprintf("select yr,mth,dy,data_value,data_code,data_desc from tdi_stock where data_code='%s'",code)
    tp.run_sql( sql)

}

tdi.commodity_plot <- function(code ='TDI-GOLD'){

    require(ggplot2)

    #code ='TDI-GOLD'
    my_data <- tdi.commodity_get( code = code)
    my_data$date <- as.Date( paste( my_data$yr,my_data$mth,my_data$dy, sep='-'))

    g <- ggplot(data = my_data, aes(x=date, y=data_value))
    g <- g + geom_line(colour='blue', size=1.5)
    g <- g + labs(x = '', y = 'Price ($, USD)', title = sprintf("%s Price",my_data$data_desc[ 1 ]) )

    print( g )
}

tdi.stock_plot <- function(code ='TDI-FTSE-100'){

    require(ggplot2)

    #code ='TDI-FTSE-100'
    my_data <- tdi.stock_get( code = code)
    my_data$date <- as.Date( paste( my_data$yr,my_data$mth,my_data$dy, sep='-'))

    g <- ggplot(data = my_data, aes(x=date, y=data_value))
    g <- g + geom_line(colour='blue', size=1.5)
    g <- g + labs(x = '', y = 'Price ($, USD)', title = sprintf("%s Price",my_data$data_desc[ 1 ]) )

    print( g )
}

tdi.format_data_store <- function( uri = tdi.get_uri()){

    ROW_LIMIT = 2000
    ROW_SKIP = 6
    ROW_START = 7
    ROW_DISCARD = c(158,159)
    COL_END = 5

    cde <- trimws(gsub("[\r\n]","",rvest::html_text(tdi.scrap_data(uri = uri, css='td , .small'))))
    cde <- cde[ ! c(cde =="") ]


    r1 <- ROW_START
    r2 <- r1 + COL_END
    overview <- cde[r1:r2]

    while(r1 < ROW_LIMIT ){

        r1 <- r2 + 1
        r2 <- r1 + COL_END

        cat(r1,' - ' ,cde[r1],'\n')
        if(
            toupper(trimws(cde[r1])) %in%
            c('HISTORICAL DATA','ASSETS, BONDS AND SECURITIES')
            || is.na( cde[r1] )){

            break
        }

        if( !(cde[r1] == "Credit Rating") ){
            overview <- rbind(  overview, cde[r1:r2])
        }else{
            r2 <- r2 - 2
            overview <- rbind(  overview, c( cde[r1:r2],'xxx','monthly') )
        }

    }
    overview <- data.frame(
        indicator = overview[,1],
        latest = overview[,2],
        ref = overview[,3],
        prev = overview[,4],
        range = overview[,5],
        freq = overview[,6],
        stringsAsFactors = FALSE
    )


    overview <- dplyr::filter( overview, !(tolower(latest) == 'last') )

    my_latest <- strsplit( overview$latest,"       ")

    last_unit <- data.frame( latest= my_latest[[ 1 ]][ 1 ], unit = trimws( my_latest[[ 1 ]][ 3 ] ), stringsAsFactors = FALSE)

    for (i in 2:length(my_latest)){
        #i = 2
        my_row <- my_latest[[ i ]]
        my_last <- my_row[ 1 ]
        my_unit <- 'xxx'

        if( length( my_row )> 1 ){ my_unit <- my_row [ length(my_row) ] }

        last_unit <- rbind( last_unit, c( my_last, my_unit ) )
    }

    overview$unit <- last_unit$unit
    overview$last <- last_unit$latest
    tdi <- overview[, - c( 2 ) ]

    tdi$data_code <- paste0("TDI-", gsub(" ","-", toupper( tdi$indicator)))

    min_max <- strsplit( tdi$range,":")


    low_high <- data.frame(
        low = as.numeric( trimws( min_max[[ 1 ]][ 1 ] )),
        high = as.numeric(trimws( min_max[[ 1 ]][ 2 ] )),
        stringsAsFactors = FALSE
    )

    for (i in 2:length( min_max )){

        my_row <- min_max[[i]]

        my_low <- NA
        abc <- trimws( my_row[1] )
        if( !(abc == 'xxx') ){
            my_low <- as.numeric( abc )
        }

        my_high <- NA
        if( length( my_row )> 1 ){
            abc <- my_row [ length(my_row) ]
            if( !(abc == 'xxx') ){
                my_high <- as.numeric( trimws( my_row [ length(my_row) ]))  }
        }

        low_high <- rbind( low_high, c( my_low, my_high ) )
    }

    tdi$low <- low_high$low
    tdi$high <- low_high$high

    tdi$yr <- as.numeric(sprintf("20%s", substr( tdi$ref,5,6 )))

    tdi$mth <- match( substr( tdi$ref,1,3 ), month.abb)

    return(tdi)
}

tdi.get_uri <- function(x = 'uk'){

    my_uri <- tp.run_sql( sprintf("select tdi_uri as uri from trade_countries where lower(country_code) = '%s'", tolower( x ) ))

    if(nrow(my_uri) == 1){

        return( my_uri$uri)

    }else{

        return( NULL )
    }
}


tdi.store_data <- function(country = 'uk'){
    require(magrittr)

    #ROW_SPECIAL = 96
    my_country <- tolower( country )
    tdi <- tdi.format_data_store(uri = tdi.get_uri( my_country))
    tdi <- dplyr::filter(tdi, !is.na(mth)  )

    get_frq <- function( x ){

        switch(
            tolower( x ),
            'monthly' = 12,
            'quarterly' = 4,
            'yearly' = 1,
            'daily' = 260,
            999
        )

    }

    n_tdi <- nrow( tdi )
    my_frq <- numeric( n_tdi  )

    for(i in 1: n_tdi){

        my_frq[ i ] <- get_frq( tdi$freq[ i ])
    }


    data_fields <- "data_code, data_value, yr, mth, tdi_ref, tdi_range, data_lowest, data_highest, data_value_prev, country, data_desc, data_unit, data_src, data_frq, tdi_frq"


    data_values <- sprintf(
        " '%s'       , %s      , %s    , %s     ,'%s'    ,'%s'      , %s     , %s      , %s      , '%s'      , '%s'         ,'%s'     , '%s'               , %s    , '%s' ",
        tdi$data_code, tdi$last, tdi$yr, tdi$mth, tdi$ref, tdi$range, tdi$low, tdi$high, tdi$prev, my_country, tdi$indicator, tdi$unit, 'Trading Economics', my_frq,  tdi$freq
    )


    data_sql <- sprintf("insert into tdi_store (%s) values (%s);", data_fields, data_values)

    for(i in 1:n_tdi ){
        cat(sprintf("%s - Running %s - %s of %s ", country, tdi$indicator[ i ], i, n_tdi),"\n")

        tp.run_sql( data_sql[ i ] )

    }

    cat("All done folks!!!\n")
}

tdi.global_latest <- function(country = 'uk'){
    require(magrittr)

    #ROW_SPECIAL = 96
    my_country <- tolower( country )
    tdi <- tdi.format_data_store(uri = tdi.get_uri( my_country))
    tdi <- dplyr::filter(tdi, !is.na(mth)  )

    get_frq <- function( x ){

        switch(
            tolower( x ),
            'monthly' = 12,
            'quarterly' = 4,
            'yearly' = 1,
            'daily' = 260,
            999
        )

    }

    n_tdi <- nrow( tdi )
    my_frq <- numeric( n_tdi  )

    for(i in 1: n_tdi){

        my_frq[ i ] <- get_frq( tdi$freq[ i ])
    }


    data_fields <- "data_code, data_value, yr, mth, tdi_ref, tdi_range, data_lowest, data_highest, data_value_prev, country, data_desc, data_unit, data_src, data_frq, tdi_frq"


    data_values <- sprintf(
        " '%s'       , %s      , %s    , %s     ,'%s'    ,'%s'      , %s     , %s      , %s      , '%s'      , '%s'         ,'%s'     , '%s'               , %s    , '%s' ",
        tdi$data_code, tdi$last, tdi$yr, tdi$mth, tdi$ref, tdi$range, tdi$low, tdi$high, tdi$prev, my_country, tdi$indicator, tdi$unit, 'Trading Economics', my_frq,  tdi$freq
    )


    data_sql <- sprintf("insert into tdi_global_latest (%s) values (%s);", data_fields, data_values)

    for(i in 1:n_tdi ){
        cat(sprintf("%s - Running %s - %s of %s ", country, tdi$indicator[ i ], i, n_tdi),"\n")

        tp.run_sql( data_sql[ i ] )

    }

    cat("All done folks!!!\n")
}

# tdi.update_store()
tdi.update_store <- function(){

    tdi.store_data('uk')
    tdi.store_data('de')
    tdi.store_data('fr')
    tdi.store_data('us')
    tdi.store_data('jp')
    tdi.store_data('cn')
    tdi.store_data('it')
    tdi.store_data('es')
    tdi.store_data('euro')
    tdi.store_data('in')
    tdi.store_data('ca')

    cat("Now updating data days ...\n")

    storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name("tdi_store_update_data_days")$qry_exec()

    cat("All done folks \n")
}

tdi.update_global <- function(){

    tdi.global_latest('uk')
    tdi.global_latest('de')
    tdi.global_latest('fr')
    tdi.global_latest('us')
    tdi.global_latest('jp')
    tdi.global_latest('it')
    tdi.global_latest('ca')

    cat("Now updating data days ...\n")

    storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name("tdi_global_update_data_days")$qry_exec()

    cat("All done folks \n")

}

# abc <- tp.run_sql("select distinct data_code from tdi_global_latest order by data_code ")
#
# abc <- tp.run_sql("select distinct data_code,data_unit from tdi_global_latest where substr(trim(data_unit),1,3) not in ('USD','EUR','CAD','JPY','GBP','CNY') order by data_code")
# abc <- sqldf::sqldf("select distinct data_code from abc")
# tdi_start <- 151
# tdi_delta <- 9
#
# for(i in tdi_start:(tdi_start + tdi_delta)){
#     tdi.g7_plot( abc$data_code[ i ])
#     cat(sprintf("%s plotted", abc$data_code[i]),"\n")
# }


# for(i in 1:length(tdi.indicators)){
#     tdi.g7_plot( tdi.indicators[[ i ]])
#     cat(sprintf("%s plotted", tdi.indicators[[ i ]] ),"\n")
# }

 # abc <- tp.run_sql("select distinct data_code from tdi_commodities order by data_code")
 # for(i in 1:nrow(abc)){
 #     tdi.commodity_plot( abc$data_code[ i ])
 #     cat(sprintf("%s plotted", abc$data_code[i]),"\n")
 # }
