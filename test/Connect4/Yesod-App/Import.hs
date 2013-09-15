{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module defines some configurations for the Yesod app.
module Import where

import Yesod.Default.Util
import Data.Default
import Language.Haskell.TH
import Yesod.Form.Jquery            as X (urlJqueryJs)
import Text.Hamlet
import qualified Text.Cassius       as H

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: FilePath -> ExpQ
widgetFile = widgetFileReload widgetFileSettings

toCassiusFile x = x ++ ".cassius"

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif
