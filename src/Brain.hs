module Brain
( processRequest
) where

import Conduit

processRequest :: ConduitEvent -> IO ConduitResponse
processRequest _ = return ConduitResponseUnknown