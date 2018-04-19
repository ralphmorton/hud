{-# LANGUAGE OverloadedStrings #-}

module HUD.Constants (
    amqpExchange,
    amqpDeadLetterExchange
) where

import Data.Text (Text)

--
--
--

amqpExchange :: Text
amqpExchange = "hud"

amqpDeadLetterExchange :: Text
amqpDeadLetterExchange = "hud_retry"
