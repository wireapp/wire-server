.[]
  | .parsedUpdatedTime = ( .updated
                         | sub("(?<time>.*)\\.[\\d]+(?<tz>.*)"; "\(.time)\(.tz)")
                         | strptime("%Y-%m-%d %H:%M:%S %z %Z")
                         | mktime
                         )
  | select ( .parsedUpdatedTime < (now - 3 * 60 * 60))
  | [.name, .namespace]
  | @tsv