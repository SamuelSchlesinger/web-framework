{-# LANGUAGE NoImplicitPrelude #-}
module API where

import Data.Proxy
import Servant.API

type API = EmptyAPI

apiProxy :: Proxy API
apiProxy = Proxy
