-- FUTUREWORK: the magic 32 should be made configurable, so it can be tuned
add ::
  TeamId ->
  NotificationId ->
  List1 JSON.Object ->
  Galley r ()
add tid nid (Blob . JSON.encode -> payload) =
  write cqlInsert (params LocalQuorum (tid, nid, payload, notificationTTLSeconds)) & retry x5
  where
    cqlInsert :: PrepQuery W (TeamId, NotificationId, Blob, Int32) ()
    cqlInsert =
      "INSERT INTO team_notifications \
      \(team, id, payload) VALUES \
      \(?, ?, ?) \
      \USING TTL ?"

notificationTTLSeconds :: Int32
notificationTTLSeconds = 24192200

fetch :: TeamId -> Maybe NotificationId -> Range 1 10000 Int32 -> Galley r ResultPage
fetch tid since (fromRange -> size) = do
  -- We always need to look for one more than requested in order to correctly
  -- report whether there are more results.
  let size' = bool (+ 1) (+ 2) (isJust since) size
  page1 <- case TimeUuid . toUUID <$> since of
    Nothing -> paginate cqlStart (paramsP LocalQuorum (Identity tid) size') & retry x1
    Just s -> paginate cqlSince (paramsP LocalQuorum (tid, s) size') & retry x1
  -- Collect results, requesting more pages until we run out of data
  -- or have found size + 1 notifications (not including the 'since').
  let isize = fromIntegral size' :: Int
  (ns, more) <- collect Seq.empty isize page1
  -- Drop the extra element from the end as well.  Keep the inclusive start
  -- value in the response (if a 'since' was given and found).
  -- This can probably simplified a lot further, but we need to understand
  -- 'Seq' in order to do that.  If you find a bug, this may be a good
  -- place to start looking.
  return $! case Seq.viewl (trim (isize - 1) ns) of
    EmptyL -> ResultPage Seq.empty False
    (x :< xs) -> ResultPage (x <| xs) more
  where
    collect ::
      Seq QueuedNotification ->
      Int ->
      Page (TimeUuid, Blob) ->
      Galley r (Seq QueuedNotification, Bool)
    collect acc num page =
      let ns = splitAt num $ foldr toNotif [] (result page)
          nseq = Seq.fromList (fst ns)
          more = hasMore page
          num' = num - Seq.length nseq
          acc' = acc >< nseq
       in if not more || num' == 0
            then return (acc', more || not (null (snd ns)))
            else liftClient (nextPage page) >>= collect acc' num'
    trim :: Int -> Seq a -> Seq a
    trim l ns
      | Seq.length ns <= l = ns
      | otherwise = case Seq.viewr ns of
        EmptyR -> ns
        xs :> _ -> xs
    cqlStart :: PrepQuery R (Identity TeamId) (TimeUuid, Blob)
    cqlStart =
      "SELECT id, payload \
      \FROM team_notifications \
      \WHERE team = ? \
      \ORDER BY id ASC"
    cqlSince :: PrepQuery R (TeamId, TimeUuid) (TimeUuid, Blob)
    cqlSince =
      "SELECT id, payload \
      \FROM team_notifications \
      \WHERE team = ? AND id >= ? \
      \ORDER BY id ASC"

-------------------------------------------------------------------------------
-- Conversions

toNotif :: (TimeUuid, Blob) -> [QueuedNotification] -> [QueuedNotification]
toNotif (i, b) ns =
  maybe
    ns
    (\p1 -> queuedNotification notifId p1 : ns)
    ( JSON.decode' (fromBlob b)
    -- FUTUREWORK: this is from the database, so it's slightly more ok to ignore parse
    -- errors than if it's data provided by a client.  it would still be better to have an
    -- error entry in the log file and crash, rather than ignore the error and continue.
    )
  where
    notifId = Id (fromTimeUuid i)
