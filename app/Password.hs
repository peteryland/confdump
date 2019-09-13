module Password where

import Data.ByteString.UTF8 as BU(fromString, ByteString)

confPass :: BU.ByteString
confPass = BU.fromString "my conf db password"

-- Change the above string, then to ask git not to notice, do:
-- $ git update-index --assume-unchanged app/Password.hs
