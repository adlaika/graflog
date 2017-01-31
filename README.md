# Graflog

Monadic correlated log events!

##To Use:

- given a data type:
    ```
    data UserSpec = UserSpec
      { email :: Email
      , password :: Text
      , name :: Text
      } deriving (Eq, Show, Generic)
    ```

    we can create an instance of ToLog:
    ```
    instance ToLog UserSpec where
      toLog UserSpec{email, name} =
        dictionary
          [ pair "email" email
          , pair "password" Redacted
          , pair "name" name ]
    ```

    and derive JSON instances of that:
    ```
    deriveJSON defaultOptions ''UserSpec
    ```

    which can then be logged inside a monad:
    ```
    foo = do
      let jimbo = UserSpec (Email "jimbo@gmail.com" ...)
      logJSON $ Event (CorrelationId 0) (EventId 0) "User" (toLog jimbo)
      return jimbo
    ```

- you must force stdout to flush after each line, or logs won't appear in a timely manner: call `Graflog.Console.enableStdoutLineBuffering` at the top of your main function.

- CorrelationId and EventId generation are not yet supported, so you must create an event manually:
  - inside do notation: `let event = Event (CorrelationId 0) (EventId 0)`
  - followed at some point by: `logJSON $ event (Action "some kind of metadata") (toLog dataToLog)`

##To Do:

- CorrelationId and EventId generation
- Generic derivation of ToLog / ToJSON instances
- Support for non-JSON logging?
