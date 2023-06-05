import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Clock.System


main :: IO()
main = do
    time <- getSystemTime
    putStrLn $ show (systemSeconds time )

    let utc = systemToUTCTime time
    print utc

    end <- getSystemTime
    
    let diff = (systemSeconds time) - (systemSeconds end)    
    print diff

    print ">>> end"
