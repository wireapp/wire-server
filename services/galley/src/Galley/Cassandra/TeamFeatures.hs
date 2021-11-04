getFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.FeatureHasNoConfig a,
    HasStatusCol a
  ) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus a))
getFeatureStatusNoConfig tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mStatusValue <- (>>= runIdentity) <$> retry x1 q
  pure $ TeamFeatureStatusNoConfig <$> mStatusValue
  where
    select :: PrepQuery R (Identity TeamId) (Identity (Maybe TeamFeatureStatusValue))
    select = fromString $ "select " <> statusCol @a <> " from team_features where team_id = ?"

setFeatureStatusNoConfig ::
  forall (a :: Public.TeamFeatureName) m.
  ( MonadClient m,
    Public.FeatureHasNoConfig a,
    HasStatusCol a
  ) =>
  TeamId ->
  TeamFeatureStatus a ->
  m (TeamFeatureStatus a)
setFeatureStatusNoConfig tid status = do
  let flag = Public.tfwoStatus status
  retry x5 $ write insert (params LocalQuorum (tid, flag))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue) ()
    insert = fromString $ "insert into team_features (team_id, " <> statusCol @a <> ") values (?, ?)"

getApplockFeatureStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureAppLock))
getApplockFeatureStatus tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbEnforce, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (Public.TeamFeatureAppLockConfig <$> mbEnforce <*> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Public.EnforceAppLock, Maybe Int32)
    select =
      fromString $
        "select " <> statusCol @'Public.TeamFeatureAppLock <> ", app_lock_enforce, app_lock_inactivity_timeout_secs "
          <> "from team_features where team_id = ?"

setApplockFeatureStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureAppLock ->
  m (TeamFeatureStatus 'Public.TeamFeatureAppLock)
setApplockFeatureStatus tid status = do
  let statusValue = Public.tfwcStatus status
      enforce = Public.applockEnforceAppLock . Public.tfwcConfig $ status
      timeout = Public.applockInactivityTimeoutSecs . Public.tfwcConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, enforce, timeout))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue, Public.EnforceAppLock, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> statusCol @'Public.TeamFeatureAppLock
          <> ", app_lock_enforce, app_lock_inactivity_timeout_secs) values (?, ?, ?, ?)"

getSelfDeletingMessagesStatus ::
  forall m.
  (MonadClient m) =>
  TeamId ->
  m (Maybe (TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages))
getSelfDeletingMessagesStatus tid = do
  let q = query1 select (params LocalQuorum (Identity tid))
  mTuple <- retry x1 q
  pure $
    mTuple >>= \(mbStatusValue, mbTimeout) ->
      TeamFeatureStatusWithConfig <$> mbStatusValue <*> (Public.TeamFeatureSelfDeletingMessagesConfig <$> mbTimeout)
  where
    select :: PrepQuery R (Identity TeamId) (Maybe TeamFeatureStatusValue, Maybe Int32)
    select =
      fromString $
        "select "
          <> statusCol @'Public.TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl "
          <> "from team_features where team_id = ?"

setSelfDeletingMessagesStatus ::
  (MonadClient m) =>
  TeamId ->
  TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages ->
  m (TeamFeatureStatus 'Public.TeamFeatureSelfDeletingMessages)
setSelfDeletingMessagesStatus tid status = do
  let statusValue = Public.tfwcStatus status
      timeout = Public.sdmEnforcedTimeoutSeconds . Public.tfwcConfig $ status
  retry x5 $ write insert (params LocalQuorum (tid, statusValue, timeout))
  pure status
  where
    insert :: PrepQuery W (TeamId, TeamFeatureStatusValue, Int32) ()
    insert =
      fromString $
        "insert into team_features (team_id, "
          <> statusCol @'Public.TeamFeatureSelfDeletingMessages
          <> ", self_deleting_messages_ttl) "
          <> "values (?, ?, ?)"
