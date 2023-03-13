module Config where

import Imports
import Network.HTTP.Client

data Service = Brig | Galley | Cannon

data Env = Env
  { context :: Context,
    manager :: Manager
  }

data ServiceMap = ServiceMap
  { brig :: Word16,
    galley :: Word16,
    cannon :: Word16
  }

servicePort :: ServiceMap -> Service -> Word16
servicePort m Brig = m.brig
servicePort m Galley = m.galley
servicePort m Cannon = m.cannon

data Context = Context
  { serviceMap :: ServiceMap,
    version :: Int
  }

mkEnv :: IO Env
mkEnv = do
  manager <- newManager defaultManagerSettings
  pure
    Env
      { context =
          Context
            { serviceMap =
                ServiceMap
                  { brig = 8082,
                    galley = 8085,
                    cannon = 8083
                  },
              version = 4
            },
        manager = manager
      }
