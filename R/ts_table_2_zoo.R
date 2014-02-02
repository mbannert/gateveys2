#' Turn time series data.table into zoo object
#' 
#' This function extracts a zoo time series object from a
#' data.table that was created by \code{\link{calc_shares}}
#' and processed by \code{\link{dt2ts_table}}. The resulting zoo object
#' is stored to an environmen
#' 
#' @param dtable the data.table object
#' @param env environment to store the results
#' @param cname name of the country, typically 2 characters.
#' @param pname name of the data.provider. 
#' @param sname name of the source. 
#' @param freq integer frequency of the resulting time series, defaults to 4.
#' currently only quarterly time series are supported.
#' @param item_chunk character chunk denoting items within the time series key. 
#' defaults to "item:
#' @param ans_chunk character chunk denoting answer count. 
#' @param position of variable name in data.table key, defaults to 3
#' @author Matthias Bannert
#' @seealso \code{\link{data.table}}
#' @export
#' 
ts_table_2_zoo <- function(dtable,env,
                           cname = "ch",pname = "kof",sname = "iq02",
                           freq = 4,item_chunk = "item",ans_chunk = "an",
                           var_in_key = 4){
  # SANITY CHECKS ####
  if(!(length(key(dtable)) > 1)){
    stop("Invalid data.table key. Key has to be of at least length 2 in order
         to form a cartesian grid. Check key(dtable)")
  }
  if(!("time" %in% key(dtable))){
    stop("Key 'time' not found. Time needs to be a key in order to create
         valid zoo time series objects.")
  }
  #   if(freq != 4){
  #     stop("This function can currently only process quarterly timee series.")
  #   }
  #   
  # HELPER FUNCTION ####
  generate_key_chunk <- function(country,provider,data_source,
                                 level,selected_level,variable){
    paste(country,provider,data_source,level,selected_level,variable,sep=".")
  }
  
  # CREATE CARTESIAN GRIDS ####
  # grid is created based on key see also ?dt2grid.
  grd <- dt2grid(dtable,"time")
  
  # TO DO: look for an apply method that does not coerce and is type safe
  # gotta loop for now, maybe improve this performance wise later on. 
  # cause I need them for comparison
  # result is only temporarily user here to check output
  res <- list()
  # loop over all rows of the grid
  for (i in 1:nrow(grd)){
    out <- do.call(dt2ts_table,
                   # concatenate list: data.table + the arguments from the grid)
                   c(list(dt=dtable),as.list(grd[i,]))
    )
    # need to generate share and AN zoo series and assign them to environment
    # QUARTERLY SERIES ####
    if(freq == 4){
      # create zoo time series objects
      share <- with(out,zoo(out$share,order.by=as.yearqtr(out$time),4))
      answers <- with(out,zoo(out$ANTot,order.by=as.yearqtr(out$time),4))
      # assign the time series objects to the store environment
      # could improve on this: more abstract form of pasting
      # composite key elements
      
      if(length(key(dtable)) == 4){
        key_chunk <- generate_key_chunk(cname,pname,sname,
                                        level = paste(names(grd)[1],names(grd)[2],sep = "_"),
                                        selected_level = paste(grd[i,1],grd[i,2],sep = "_"),
                                        variable = key(dtable)[var_in_key])  
        # store main variable (share) 
        assign(paste(key_chunk,paste(item_chunk,
                                     grd[i,3],sep="_"),
                     sep="."),share,envir=env)
      } else {
        key_chunk <- generate_key_chunk(cname,pname,sname,
                                        level = names(grd)[1],
                                        selected_level = grd[i,1],
                                        variable = key(dtable)[var_in_key])  
        # store main variable (share) 
        assign(paste(key_chunk,paste(item_chunk,
                                     grd[i,2],sep="_"),
                     sep="."),share,envir=env)
      }
      
      # TODO: Decide whether to really store total answers.
      assign(paste(key_chunk,ans_chunk,sep="."),
             answers,envir=env)
      
      res[[i]] <- paste("Key group",key_chunk,"stored to environment",
                        deparse(substitute(env)),".")
    }
    
    # MONTHLY SERIES ####
    if(freq == 12){
      # create zoo time series objects
      share <- with(out,zoo(out$share,order.by=as.yearmon(out$time),12))
      answers <- with(out,zoo(out$ANTot,order.by=as.yearmon(out$time),12))
      # assign the time series objects to the store environment
      # could improve on this: more abstract form of pasting
      # composite key elements
      
      if(length(key(dtable)) == 4){
        key_chunk <- generate_key_chunk(cname,pname,sname,
                                        level = paste(names(grd)[1],names(grd)[2],sep = "_"),
                                        selected_level = paste(grd[i,1],grd[i,2],sep = "_"),
                                        variable = key(dtable)[var_in_key])  
        # store main variable (share) 
        assign(paste(key_chunk,paste(item_chunk,
                                     grd[i,3],sep="_"),
                     sep="."),share,envir=env)
      } else {
        key_chunk <- generate_key_chunk(cname,pname,sname,
                                        level = names(grd)[1],
                                        selected_level = grd[i,1],
                                        variable = key(dtable)[var_in_key])  
        # store main variable (share) 
        assign(paste(key_chunk,paste(item_chunk,
                                     grd[i,2],sep="_"),
                     sep="."),share,envir=env)
      }
      
      # TODO: Decide whether to really store total answers.
      assign(paste(key_chunk,ans_chunk,sep="."),
             answers,envir=env)
      
      res[[i]] <- paste("Key group",key_chunk,"stored to environment",
                        deparse(substitute(env)),".")
    }
    
  }
  # return argument
  res
}
