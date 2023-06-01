#' Create Post
#'
#' @param title Title of new post
#' @param tags Categories for new post
#' @param ... Additional categories
#' @param author Author of post
#'
#' @return
#' @export
#'
#' @examples
#' create_post('My Cool Post')
create_post <- function(title, tags = NULL, ...,
                        author = Sys.getenv("DEFAULT_AUTHOR")){

  wd_path <- getwd()
  post_name <- title
  clean_name <- janitor::make_clean_names(post_name)
  dir_name <- paste0('posts/', clean_name)
  path_name <- paste0('posts/', clean_name, '/index.qmd')
  if(!dir.exists(paste0('posts/', clean_name))){
    dir.create(dir_name)
    file.create(path_name)
    writeLines('---', path_name)
    write(paste0('title: "', post_name, '"'), path_name, append = T)
    write(paste0('author: "', author, '"'), path_name, append = T)
    write(paste0('date: "', Sys.Date(), '"'), path_name, append = T)
    if(!is.null(tags)){
      tags <- paste(tags, ..., sep = ', ')
      write(paste0('categories: [', tags, ']'), path_name, append = T)
    }
    write('draft: true', path_name, append = T)
    write('---', path_name, append = T)
  } else {
    stop('Post already exists')
  }
}
