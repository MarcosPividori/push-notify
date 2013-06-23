
import Send
import Types
import Data.Aeson
import Data.Text    (Text, pack)
import Data.Default
import Data.HashMap.Strict

main = do
            result  <-  sendGCM message 2
            print $ show result

message = def{
        registration_ids = Just ["RegIdadaasd"]
    ,   collapse_key = "Updates Available"
    ,   data_object = Just (fromList [(pack "Score" .= (8 :: Int))])
    ,   delay_while_idle = True
    ,   time_to_live = Just 120
    ,   dry_run = True
    }
