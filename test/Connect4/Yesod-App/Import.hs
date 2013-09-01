module Import
    ( module Import
    , module X
    ) where

import Yesod.Default.Util
import Data.Default
import Language.Haskell.TH
import Yesod.Form.Jquery as X (urlJqueryJs)

import Language.Haskell.TH.Syntax
import Yesod.Default.Util
import Data.Default (def)
import Text.Hamlet

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile :: FilePath -> ExpQ
widgetFile = widgetFileReload widgetFileSettings
