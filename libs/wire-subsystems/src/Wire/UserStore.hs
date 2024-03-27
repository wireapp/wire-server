module Wire.UserStore where

data UserStore m a where
  GetUser :: UserId -> UserStore m User
