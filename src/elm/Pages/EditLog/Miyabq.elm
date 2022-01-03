module Pages.EditLog.Miyabq exposing (isCorrectPassword)


isCorrectPassword : String -> Bool
isCorrectPassword input =
    input == pw


pw : String
pw =
    "スギ薬局"
