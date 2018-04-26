
module HUD.Heroku.Types (
    HerokuClient(..)
) where

import Data.Text

--
--
--

data HerokuClient = HerokuClient {
    hrcClientID :: Text,
    hrcClientSecret :: Text
}
