import Args
import Mail

import Data.Maybe
-- constants
server = Server { host = "smtp", port = 25 }
domain = "customserver.com"
from = "jgoday@gmail.com"

-- arguments
data Arguments = To String |
        Subject (Maybe String) |
        Message (Maybe String) |
        Help deriving (Eq, Show)

opts = [
    argsCreateRequiredOption 't' "to" To "To address",
    argsCreateOptionalOption 's' "subject" Subject "Mail Subject",
    argsCreateOptionalOption 'm' "message" Message "Mail Message"
    ]

argsFindTo :: [Arguments] -> String -> String
argsFindTo [] def = def
argsFindTo (x:xs) def = case x of
    (To t) -> t
    _    -> argsFindTo xs def

argsFindSubject :: [Arguments] -> String -> String
argsFindSubject [] def = def
argsFindSubject (x:xs) def = case x of
    (Subject (Just s)) -> s
    _    -> argsFindSubject xs def

argsFindMessage :: [Arguments] -> String -> String
argsFindMessage [] def = def
argsFindMessage (x:xs) def = case x of
    (Message (Just m)) -> m
    _    -> argsFindMessage xs def

sendMail to subject message =
    mailConnect server >>= mailSend domain from [to] subject message

main = do
    args  <- argsGet opts
    let to = argsFindTo args "jgoday@gmail.com"
    let subject = argsFindSubject args "Default Subject"
    let message = argsFindMessage args "Default Message"
    sendMail to subject message


