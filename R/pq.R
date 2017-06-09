get.pqs.dir = function(...) {
  pq.opts()$pqs.dir
}

get.pq.dir = function(pq, id=pq$id, pqs.dir = get.pqs.dir()) {
  file.path(pqs.dir, id)
}

get.pq.userid = function(app=getApp()) {
  app$userid
}

pq.opts = function(app=getApp()) {
  restore.point("pq.opts")
  if (!is.null(app[["pq.opts"]]))
    return(app$pq.opts)

  pq.opts = getOption("peerquiz.options")
  if (is.null(pq.opts)) {
    pq.opts = init.pq.opts()
    set.pq.opts(pq.opts)
  }
  pq.opts
}

set.pq.opts = function(pq.opts = init.pq.opts(), app=getApp()) {
  if (!is.null(app)) {
    app$pq.opts = pq.opts
  }
  options(pq.opts=pq.opts)
  invisible(pq.opts)
}

init.pq.opts = function(pqs.dir = file.path(getwd(),"pq")) {
  nlist(pqs.dir)
}

example.peerquiz = function() {
  setwd("D:/libraries/peerquiz")
  file = "budget.yaml"
  app = eventsApp()
  pq = import.yaml()
  pq = create.pq(yaml.file=file)

  sols = c(list(pq$solution), pq$wrongsols)
  sols = lapply(sols,sol.source.to.secure.html)
  ui = peerquiz.guess.sol.ui(sols, pq=pq)

  ui = peerquiz.input.ui(pq)


  app$ui = fluidPage(
    # Add a CSS class for red text colour
    inlineCSS(list(
      .bgempty = "background: white",
      .bg1 = "background:  #FFD700",
      .bg2 = "background:  #D0D0D0",
      .bg3 = "background: ##CD7F32"
    )),
    withMathJax(ui)
  )
  viewApp()

  #view.html(ui=ui)
}

load.pq = function(id, pq.file = file.path(dir,paste0(id,".pq")),  dir = get.pq.dir(id=id)) {
  restore.point("load.pq")
  return(readRDS(pq.file))
}

create.pq = function(yaml=NULL,pq=NULL, id = NULL, yaml.file = NULL, pqs.dir = get.pqs.dir(), save=TRUE) {
  restore.point("create.pq")

  if (!is.null(yaml.file)) {
    yaml = readLines(yaml,encoding = "UTF-8")
  }
  if (is.null(pq)) {
    pq = read.yaml(file=yaml.file, text=yaml)
  }
  pq$question_html = md2html(pq$question)
  pq$id = paste0(pq$name)
  pq$ns = NS(paste0("pq_",pq$id))
  pq$sol_div_id = paste0("sol_div_",1:8,"_",pq$id)
  pq$num.sol.click = 2
  pq$lang = first.none.null(pq$lang, "en")
  pq$str = pq_strings(pq$lang)

  if (is.null(pq$ace_lines)) {
    pq$ace_lines = 10
  }

  pq = init.pq.form(pq)
  if (!is.null(pq$funs)) {
    txt = sep.lines(pq$funs)

    code = unlist(find.rmd.chunks(txt, add.code=TRUE)$code)
    if (length(code)>0) {
      call = parse(text=code)
      env = new.env()
      eval(call, env)
      pq = c(pq,as.list(env))
    }
  }

  # save pq file
  if (save) {
    pq.dir = get.pq.dir(pq)
    if (!dir.exists(pq.dir)) {
      dir.create(pq.dir, recursive=TRUE)
      dir.create(file.path(pq.dir,"users"))
    }
    saveRDS(pq, file.path(pq.dir, paste0(pq$id,".pq")))

    if (!is.null(yaml)) {
      writeLines(yaml,file.path(pq.dir, paste0(pq$id,".yaml")),useBytes = TRUE)
    }
  }

  pq
}
