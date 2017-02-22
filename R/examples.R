# abc <- onsR2::download('ABMI')$q_data
# abc <- 'USDM'
# cde <- beamaTrends::tg$new(abc)
# cde$plot()
# cde$plot(T)
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
# cde$set_agg('qtr')$get_agg()
# cde$set_agg('qtr','avg')$get_agg()
# cde$set_agg('yr')$get_agg()
# cde$set_agg('yr','avg')$get_agg()
# cde$set_agg('ytd')$get_agg()
# cde$set_agg('ytd','avg')$get_agg()
# cde$set_agg('mat')$get_agg()
# cde$set_agg('mat','avg')$get_agg()

# cde$set_agg('mqt')$data_agg
# cde$set_agg_default('qtr')$get_mat4()

# cde <- td$new('CHAW')
# cde <- beamaTrends::td$new('chaw')
# cde <- beamaTrends::tg$new('ABMI')
# cde <- beamaTrends::tg$new('K646',db_limit = list(yr=2016,mth=5))
# cde <- beamaTrends::td$new('K646',db_limit = list(yr=2016,mth=5))

# beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MAT,YR,YTD', skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa')
# beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR', skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa')
# beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(ops ='sum',select = 'YR', select_yr=c(2000,2015), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Electrical Machinery Exports - EU28')
# beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(T,select = 'MM12,YTD12,MAT12', select_yr=c(2000,2016), title = 'Electrical Machinery Exports - EU28')
# beamaTrends::tg$new('sitc-NANA-EXPORT-77')$plot(T, select_yr=c(2000,2016), title = 'Electrical Machinery Exports - North America')
# beamaTrends::tg$new('sitc-FR-EXPORT-77')$plot(T, select_yr=c(2000,2016), title = 'Electrical Machinery Exports - France')
# cde$plot( )
# cde$plot( T )
# cde$plot( T ,select = 'MAT12,MM12,YTD12')
# cde$plot( T ,select = 'MM1,MAT1,QQ1')
# cde$plot( T ,select = 'MM12,qq4')
# ts.plot(beamaTrends::tg$new( onsR2::download('CHAW')$m_data )$set_agg('ytd', 'avg')$get_agg(), ylab='Index(2010=100)')
# ts.plot(beamaTrends::tg$new( onsR2::download('ABMI')$q_data )$set_agg('ytd', 'avg')$get_agg(), ylab='GBP millions')
# ts.plot(beamaTrends::tg$new( onsR2::download('ABMI')$q_data )$get_growth_data('ytd', 'avg'), ylab='ytd % growth')

# ts.plot(beamaTrends::tg$new('ABMI')$get_growth_data('qq4'),ylab='Growth %')
# ts.plot(beamaTrends::tg$new('ABMI')$get_growth_data('mat4'),ylab='Growth %')

# cde$get_mm1()
# cde$get_mm3()
# cde$get_mm12()
# cde$get_qq1()
# cde$get_qq4()
# cde$get_yy1()
# cde$get_yy1(ops = 'avg')
# cde$get_mat1()
# cde$get_mat12()
# cde$get_mat4()
# cde$get_ytd4()
# cde$get_ytd12()

# cde <- tg$new(rnorm(100),c(2010,1),12)
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
# cde$get_data_df()
# cde$to_df(cde$get_ytd12(),'ytd4', T, T )

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=12), mth=rep(1:12,each=6),value=rnorm(72,mean=10,sd=1)) )
#cde$get_mat1()

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=12), mth=rep(1:12,each=6),value=rnorm(72,mean=10,sd=1)) )
#cde$get_mat1()

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=4), mth=rep(1:4,each=6),value=rnorm(24,mean=10,sd=1)) )
#cde$get_mat1()



# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
#
# cde <- beamaTrends::tg$new(
#     data.frame(
#         yr=2000:2015,
#         value=rnorm(16)
#     )
# )
#
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
#
# aaa<- cde$set_growth_fx(1)
# aaa$get_growth_data()

# abc <- onsR2::download('CHAW')$m_data
# cde <- beamaTrends::td$new(onsR2::download('CHAW')$m_data)
# cde$data_period
# cde$data_end
# cde$data_end_date
# cde$data_end_date_prv

# ccc <- function(){
#     cde <- td$new('CHAW')
#     cde$plot(
#         is_theme = T
#         )
# }
#
# cct <- function(){
#     cde <- td$new('CHAW')
#     cde$plot(
#         is_growth = T,
#         is_theme = T,
#         select =c("MAT12,MM12,YTD12"),
#         select_yr =c(2010,2017)
#     )
# }


  # abc <- onsR2::download('K646')$m_data
  # cde <- beamaTrends::td$new(abc,x_name='input cost')

  # cde$get_successive()
  # cde$get_successive( is_yoy = FALSE)
  # cde$get_mtm()$desc
  # cde$get_yoy()$desc
  # beamaTrends::td$new('K646',x_name='input cost')$get_growth_desc()
  # beamaTrends::td$new('ABMI',x_name='gdp')$get_growth_desc()
  # beamaTrends::td$new('PLUSGROUP',x_name='Plusgroup')$get_growth_desc()
  # beamaTrends::td$new('CHAW',x_name='RPI')$get_growth_desc()
  # beamaTrends::td$new()
  # aa <- beamaTrends::td$new('PLUSGROUP',x_name='Plusgroup', x_start=c(2013,11), x_frq=12)$get_growth_desc()
  # bt <- beamaTrends::td$new('ABMI',x_name='gdp')
  # aa$get_mtm()
  # aa$get_yoy()

#smart_lbl <- list(mm1="1-month growth", mm3="3-month growth", mm12="12-month growth")
#"mm1" %in% names(smart_lbl)

#aaa <- tg.get_growth_data('m_')
#tg.get_unit('CHAW')
#tg.plot_trends('CHAW,D7BT', select = "QQ1,QQ4",  x_delta=c(0,0), y_delta=c(0,1), is_themed = T )

#tg.plot_trends('CHAW,D7BT', select = "MM12,MAT12", x_delta=c(0,0), y_delta=c(0,1), is_themed = T )

#tg.plot_trends('CHAW,D7BT',  x_delta=c(0,0), y_delta=c(0,5), is_themed = T )
#tg.plot_trends('CHAW,D7BT', fx='q', x_delta=c(0,0), y_delta=c(0,5), is_themed = T )
#tg.plot_trends('m_elec,m_mech',  x_delta=c(0,3), y_delta=c(0,0), is_themed = T )

# tg.plot_trends('m_elec,k646',  x_delta=c(0,3), y_delta=c(0,0), is_themed = T )
# aaa<- tg.plot_trends('k646', select ='YY1', y1=1990,  x_delta=c(0,3), y_delta=c(0,2), is_themed = T )
# tp.view_code()
# tp.view_code(is_growth = T)

#tg.plot_trends('k646', select ='MM12,MAT12,YTD12', y1=2010,  x_delta=c(0,3), y_delta=c(0,2), is_themed = T )

#tp.view_code('m_elec')
#tp.view_code('m_elec',T)


#beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa (SITC 77)', is_themed = T)
#beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - EU28 (SITC 77)', is_themed = T)
#tg$new('sitc-NANA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - North America (SITC 77)', is_themed = T, x_delta=c(3,4),y_delta=c(0,100), dp = 0 )
#tg$new('sitc-NANA-EXPORT-77')$plot(is_growth = T,ops ='sum',select = 'MM12,YTD12,MAT12',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - North America (SITC 77)', is_themed = T, x_delta=c(3,3),y_delta=c(0,2), dp = 1 )

#tg$new('USDM')$plot(is_growth = T,ops ='sum',select = 'MM12,YTD12,MAT12',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (1 GBP)',title = 'US Dollar - Sterling Exchange Rate', is_themed = T, x_delta=c(3,3),y_delta=c(0,0.5), dp = 1 )
#tg$new('USDM')$plot(ops ='avg',select = 'MTH',select_yr = c(2015,2020), skale = 1, skale_title = 'Exchange Rate (per 1 GBP)',title = 'US Dollar - Sterling Exchange Rate', is_themed = T, x_delta=c(3,5),y_delta=c(0,0), dp = 4, brexit_mode=T )
#tg$new('EURM')$plot(ops ='avg',select = 'MTH,QTR,YR',select_yr = c(2015,2020), skale = 1, skale_title = 'Exchange Rate (per 1 GBP)',title = 'EURO - Sterling Exchange Rate', is_themed = T, x_delta=c(3,5),y_delta=c(0,0), dp = 4, brexit_mode=T )

# steel_codes_fred <- c(
#   "WPU1012","WPU1017","PCU331210331210","PCU331222331222P",
#   "PCU3312223312225","WPU101704","PCU3315123315120", "PCU3311103311107",
#   "PCU3312213312211","IR141","WPU101706","WPU107601","PCU33211133211112",
#   "PCU331222331222","WPU101211","PCU331110331110","PCU331221331221","WPU10121193",
#   "COOTHERZ3311","PCU331210331210P","PCU42993042993011","PCU429930429930113","PCU3312213312213",
#   "PCU3311103311103", "PCU3315133315131","PCU3315133315133", "PCU3321113321111","PCU3311103311105",
#
#)
