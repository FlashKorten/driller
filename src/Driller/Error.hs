{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Driller.Error
    ( ParameterError()
    , unknownParameter
    , illegalValue
    , illegalGroupId
    , duplicateParameter
    , noGroupIdFound
    ) where

import qualified Data.Text as Text ( Text, append )
import Data.Aeson.TH ( deriveJSON )

data ParameterError = ParameterError { getErrorCode :: Int, getErrorMessage :: Text.Text }
$(deriveJSON (drop 3)  ''ParameterError)

unknownParameter :: Text.Text -> ParameterError
unknownParameter parameter = ParameterError { getErrorCode    = 404
                                            , getErrorMessage = Text.append "Unknown parameter: " parameter }

illegalValue :: Text.Text -> ParameterError
illegalValue parameter = ParameterError { getErrorCode    = 405
                                        , getErrorMessage = Text.append "Illegal value for parameter: " parameter }

duplicateParameter :: Text.Text -> ParameterError
duplicateParameter parameter = ParameterError { getErrorCode    = 406
                                              , getErrorMessage = Text.append "Duplicate query parameter: " parameter }

noGroupIdFound :: ParameterError
noGroupIdFound = ParameterError { getErrorCode    = 407
                                , getErrorMessage = "Missing GroupID parameter: " }

illegalGroupId :: Text.Text -> ParameterError
illegalGroupId parameter = ParameterError { getErrorCode    = 408
                                          , getErrorMessage = Text.append "Illegal value for GroupID: " parameter }
