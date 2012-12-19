{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Driller.Error
    ( ParameterError()
    , unknownParameter
    , illegalValue
    ) where

import qualified Data.Text as Text ( Text, append )
import Data.Aeson.TH ( deriveJSON )

data ParameterError = ParameterError { getErrorCode :: Int, getErrorMessage :: Text.Text }
$(deriveJSON (drop 3)  ''ParameterError)

unknownParameter :: Text.Text -> ParameterError
unknownParameter parameter = ParameterError { getErrorCode = 404
                                            , getErrorMessage = Text.append "Unknown parameter: " parameter }

illegalValue :: Text.Text -> ParameterError
illegalValue parameter = ParameterError { getErrorCode = 405
                                        , getErrorMessage = Text.append "Illegal value for parameter: " parameter }