examples.pq.combine.answers = function() {
  id = "budgetgerade_gutscheine"
  pq.combine.answers(id=id)
}

load.pq.answers = function(id = pq$id, pq.dir = get.pq.dir(id=id), pq=NULL) {
  restore.point("load.pq.answers")
  file = file.path(pq.dir,"answers.rds")
  if (file.exists(file)) {
    df = readRDS(file)
    return(df)
  }
  pq.combine.answers(id=id, pq.dir=pq.dir)
}

pq.combine.answers = function(id = pq$id, pq.dir = get.pq.dir(id=id), save=!TRUE) {
  restore.point("pq.combine.answers")

  dir = file.path(pq.dir,"answers")
  files = list.files(dir, pattern = glob2rx("*.pqa"),full.names = TRUE)
  if (length(files)==0) return(NULL)

  li = lapply(files, function(file) {
    restore.point("innner")
    pqa = readRDS(file)
    do.call(data_frame,pqa)
  })
  df = bind_rows(li)
  df = df %>%
    arrange(time) %>%
    mutate(answer.ind = seq_len(n())) %>%
    select(answer.ind, everything())

  if (save) {
    out.file = file.path(pq.dir,"answers.rds")
    saveRDS(df,out.file)
  }
  df
}
