tp_data = R6::R6Class(
  'tp_data',
  inherit = tp_utils,
  public = list(

    groups = c(
      'economy',
      'earnings',
      'topsi',
      'exports',
      'total_orgalime',
      'topsi_orgalime',
      'inflation',
      'ppi_output',
      'ppi_output_orgalime',
      'ppi_input',
      'ppi_input_orgalime',
      'ppi_input_orgalime_core',
      'imports_cpa08',
      'exports_cpa08',
      'iop',
      'iop_orgalime',
      'iop_orgalime_core',
      'economy_pc',
      'ppi27',
      'emp_orgalime',
      'emp_male_orgalime',
      'emp_female_orgalime'

    ),
    grp = NULL

    ,initialize = function( grp){
      self$set_group(grp)
    }

    ,set_group = function(value){
      if(!missing(value) && !is.null(value)){
        self$grp <- value
      }
      invisible()
    }

    ,get_group = function(code_only = FALSE,raw = FALSE){

      my_sql <- sprintf("select * from trends_groups where grp='%s'", self$grp)
      my_data <- self$run_sql( my_sql)

      if(nrow(my_data) >0 && code_only){
        if(!raw){
          return(
            paste(my_data$code,sep='', collapse = ',')
          )
        }else{
          return( my_data$code )
        }
      }
      return(my_data)
    }

    ,view_groups = function(){
      return( self$groups)
    }

    ,view_current = function(code){
      sql <- sprintf("select * from trends_data where data_code='%s' order by yr desc, mth desc, dy desc limit 10",code)
      self$run_sql(sql)
    }

    ,add_currency_data = function(code,dt1,dt2,code_desc,clear_first=FALSE){
      if(clear_first){
          cat("Deleting old records ...\n")
          self$run_sql(sprintf("delete from trends_data where data_code='%s'",code))
      }
      cat("Starting update of ",code," @",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n")
      df <- beamafx::fx_series$new(code)$set_date1(dt1)$set_date2(dt2)$set_freq('d')$get_data()
      #df <- dplyr::filter(df,data_code ==code)
      sql<- sprintf(
        paste0(
          "insert into trends_data (yr,mth,dy,data_unit,data_value,data_code,data_src,data_desc) values ",
          "(%i,%i,%i,'1 GBP',%s,'%s','ECB','%s')"
        ),
        df$yr, df$mth ,df$dy , round(df$value,5), df$data_code,code_desc
      )
      #return(sql)

      self$run_sql(sql)
      cat("Finished updating ",code," @",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n")

      invisible(self)
    }

    ,add_ons_data = function(code,code_unit,code_desc,dt1,dt2=NULL, freq = 'm', is_group = F){
      #code = 'D7BT'
      #code='D7CH';code_unit='2005=100';code_desc='CPI Index - Electricity, Gas & Other fuels';dt1='1990-01-01';freq = 'm'
      cat("Starting update of ",code," @",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n")
      my_ts <- NULL
      my_frq <- 12

      frq <- tolower(trimws(freq))

      if( frq == 'm'){

        my_ts <- onsR2::download(code=code)$m_data


      }else if(frq == 'q'){

        my_ts <- onsR2::download(code=code)$q_data
        my_frq <- 4

      }else if(frq == 'y'){

        my_ts <- onsR2::download(code=code)$y_data
        my_frq <- 1

      }else{

        cat("Invalid frequency. Aborting process")
        return(1)

      }

      df <- beamaUtils::ts_to_df( my_ts )
      df$dy <- lubridate::day(df$date)

      # ndf <- NULL
      # cat('ok so far \n')
      # ndf$yr <- lubridate::year(df$date)
      # ndf$mth <- lubridate::month(df$date)
      # ndf$value <- as.numeric(df$value)
      # cat('ok so far \n')
      # df <- as.data.frame(ndf)

      df <- dplyr::filter( df, yr >= lubridate::year(dt1) )

      #cat('ok so far after filter\n')
      #cat('Rows in df = ',nrow(df),'\n')

      my_sql <- sprintf(
          "insert into trends_data (yr,mth,dy,data_unit,data_value,data_code,data_src,data_desc) values
           (%i,%i,%i,'%s',%s, '%s','ONS','%s')",
        df$yr, df$mth ,df$dy, code_unit , round(df$value,5), code ,code_desc
      )
      #return(my_sql)


      self$run_sql(my_sql)
      if(!is_group){
          storedQry::SQ$new( tp_utils$new()$get_db() )$set_name('trends_update_periods')$qry_exec()
      }
      cat("Finished updating ",code," @",format(Sys.time(), "%a %b %d %X %Y %Z"),"\n")


      cat('Now updating meta table \n')
      meta_count <- self$run_sql( sprintf("select id from trends_meta where data_code = '%s'",code))
      if(nrow( meta_count  ) == 0){

          meta_count <- self$run_sql( sprintf("insert into trends_meta (data_code,data_desc,data_unit,data_frq) values ('%s','%s','%s',%s)",code, code_desc,code_unit,my_frq) )
      }


      invisible(self)
    }

    ,update_ons_group = function(){

        my_data <- self$get_group()
        n_rows <- nrow( my_data )

        for(i in 1:n_rows){

         #i=9
          meta <- run_sql_btrends(
             sprintf("select data_unit, max(yr) as y1 from trends_data where data_code='%s'", my_data$code[i] )
          )
          meta_yr <- meta$y1[1]
          dt <- NULL

          if(is.na( meta_yr )){

            dt <- '2000-01-01'

          }else{

            dt1 <-  paste( meta_yr  ,'01','01',sep="-")

          }

          self$add_ons_data( code = my_data$code[i], code_unit = meta$data_unit[1], code_desc = my_data$description[i], dt1 = dt1, freq = my_data$freq[i], is_group = T)


          cat("Group: ",my_data$grp[i]," code:",my_data$description[i]," ",i,"/",n_rows, " done!\n" )
        }
        storedQry::SQ$new( tp_utils$new()$get_db() )$set_name('trends_update_periods')$qry_exec()

    }

    ,update_ons_captions = function(){

      my_data <- self$get_group()
      n_rows <- nrow( my_data )

      for(i in 1:n_rows){

        meta <- run_sql_btrends(
          sprintf("update trends_data set data_desc ='%s' where data_code='%s'",my_data$description[i], my_data$code[i] )
        )

        cat("Group: ",my_data$grp[i]," code:",my_data$description[i]," ",i,"/",n_rows, " done!\n" )
      }
    }

  )#public

  ,private = list()

)#class

### non-class functions
tp_data.update_groups <- function(){

  tp_data$new( 'economy' )$update_ons_group()
  tp_data$new( 'earnings' )$update_ons_group()
  tp_data$new( 'topsi' )$update_ons_group()
  tp_data$new( 'exports' )$update_ons_group()
  tp_data$new( 'topsi_orgalime' )$update_ons_group()
  tp_data$new( 'inflation' )$update_ons_group()
  tp_data$new( 'ppi_output_orgalime' )$update_ons_group()
  tp_data$new( 'ppi_input_orgalime' )$update_ons_group()
  tp_data$new( 'imports_cpa08' )$update_ons_group()
  tp_data$new( 'exports_cpa08' )$update_ons_group()
  tp_data$new( 'iop' )$update_ons_group()
  tp_data$new( 'iop_orgalime' )$update_ons_group()
  tp_data$new( 'iop_orgalime_core' )$update_ons_group()
  #tp_data$new( 'economy_pc' )$update_ons_group()
  tp_data$new( 'ppi27' )$update_ons_group()

  ### update periods ###
  tp_data.update_periods()
}

tp_data.update_captions <- function(){
  source("global.R")
  tp_data$new( 'topsi_orgalime' )$update_ons_captions()
  tp_data$new( 'iop_orgalime_core' )$update_ons_captions()
  tp_data$new( 'exports' )$update_ons_captions()
  tp_data$new( 'ppi_input_orgalime' )$update_ons_captions()
  tp_data$new( 'ppi_output_orgalime' )$update_ons_captions()
}

tp_data.data_export = function(code,dt1,dt2,fx,avg=T){

  data_raw <- tp$new( code )$set_date1( dt1 )$set_date2( dt2 )$set_fx( fx )$set_avg( avg )$get_data()
  data_raw$data_desc <- data_raw$dy <- data_raw$pc <- NULL
  data_format <- tidyr::spread( data_raw, data_code, value )
  tp$new()$to_clipboard( data_format )
  return( data_format )
}

tp_data.get_bases <- function(indx='m_elec'){

  storedQry::SQ$new( tp_utils$new()$get_db_indx()  )$set_name( 'beama_index_get_bases'
  )$set_params( list(`@s_index`=indx) )$qry_exec()
}

tp_data.update_periods <- function(){
  global_SQ_BT()$set_name('trends_update_periods')$qry_exec()
}


tp_data.get_indx <- function(indx='l_elec', basis='2010=100' ,y1=2013, y2=2017, m1=1, m2=12){

  SQ <- storedQry::SQ$new( tp_utils$new()$get_db_indx() )$set_name('beama_index_get_calc')
  SQ$set_params(
    list(
      `@s_index_name`=indx, `@s_index_base_to`=basis,`@s_index_name2`=indx,
      `@i_y1`=y1  ,`@i_y2`=y2  ,`@i_m1`=m1  ,`@i_m2`=m2
    )
   )$qry_exec()

}

tp_data.get_indx_all <-  function(indx='m_elec',y1=2000, y2=2020, path="W:/Library/individual_series/",save_txt=FALSE){

  basis <- tp_data.get_bases( indx=indx )
  my_base <- as.character( basis[1,1] )
  indx_data <-  tp_data.get_indx( indx=indx, basis= my_base, y1=y1, y2=y2)
  indx_data <- plyr::rename( indx_data, replace = c("calc_index"=paste0( "B" , substring(my_base,1,4))))

  for(i in 2:dim(basis)[1]){
    my_base <- as.character( basis[i,1] )
    indx_data$calc_index <- tp_data.get_indx( indx=indx, basis = my_base, y1=y1, y2=y2 )$calc_index
    indx_data <- plyr::rename( indx_data, replace = c("calc_index"=paste0("B",substring(my_base,1,4))))
  }
  if(save_txt){write.table( indx_data, paste0(path,"/",indx,".txt"),sep="\t")}
  return(indx_data)
}

tp_data.write_indx_xls <- function(indx='l_elec',y1=2000, cell_row = NULL, cell_col = NULL,y2=2020){

  require(XLConnect)
  set_cell_value <- function(wb,sheet,start_row,start_col,data){
    writeWorksheet(wb, data=data, sheet = sheet, startRow = start_row, startCol = start_col,header=TRUE,rownames=NULL)
  }


  my_workbook <- loadWorkbook(global_XLS_HIST)
  my_data <- tp_data.get_indx_all( indx = indx, y1=y1, y2=y2 )
  set_cell_value(my_workbook,sheet = indx, start_row = cell_row, start_col = cell_col, data = my_data )
  saveWorkbook(my_workbook)
}

tp_data.group_add_code <- function(code,grp){

    my_sql_info <- sprintf("select data_desc, data_frq from trends_meta where data_code='%s'", code)
    my_info <- tp_utils$new()$run_sql( my_sql_info )
    my_rows <- nrow( my_info )

    if(my_rows >0 ){
       my_sql <- sprintf(
           "insert into trends_groups (grp,code,description,freq) values('%s','%s','%s',%s)",
           grp, code, my_info$data_desc[1], my_info$data_frq[1]
       )

       my_upd <- tp_utils$new()$run_sql( my_sql)
    }

}

tp_data.group_add_groups <- function(grp){

    is_df <- is.data.frame( grp )

    if( is_df){

         n_rows <- nrow( grp )

         for(i in 1:n_rows){

             tp_data.group_add_code( grp$code[ i ], grp$grp[i])

        }
    }
}
