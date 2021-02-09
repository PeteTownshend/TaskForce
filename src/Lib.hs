module Lib (
    paramSentence
    ) where

import System.Console.StructuredCLI (
    (>+),
    command,
    custom,
    labelParser,
    param,
    parseOneOf,
    Action(ToRoot, NewLevel, NoAction),
    CommandsT,
    Handler,
    Node,
    ParseResult(Fail, Done, Partial, NoMatch),
    Validator)

paramSentence :: (Monad m) => String -- ^ Command keyword
                   -> String         -- ^ Help text for this command (including argument description)
                   -> Validator m a  -- ^ Monadic validator (in the "user" monad)
                   -> Handler m a    -- ^ Handling action. Takes the validator output as argument
                   -> CommandsT m ()
paramSentence label hint validator = paramSentence' label hint validator (return True)

paramSentence' :: (Monad m) => String -- ^ Command keyword
                    -> String         -- ^ Help text for this command (including argument description)
                    -> Validator m a  -- ^ Monadic validator (in the "user" monad)
                    -> m Bool         -- ^ Enable action in the "user" monad
                    -> Handler m a    -- ^ Handling action. Takes the validator output as argument
                    -> CommandsT m ()
paramSentence' label hint validator enable handler = do
  custom label hint parser enable handler
         where parser = sentenceParser hint validator

sentenceParser :: Monad m => String -> (String -> m (Maybe a)) -> Node m -> String -> m (ParseResult a)
sentenceParser hint validator = parseParam -.- labelParser
    where parseParam  = (=<<) parseParam'
          parseParam' (Done _ matched rest) =
              case rest of
                "?" ->
                  return $ Fail hint rest
                "" ->
                  return $ Partial [("", hint)] ""
                word -> do
                  v <- validator word
                  return $ maybe (badArg rest) (\x -> Done x (matched ++ ' ':word) "") v
          parseParam' (Fail x y) =
              return $ Fail x y
          parseParam' (Partial x y) =
              return $ Partial x y
          parseParam' NoMatch = return NoMatch
          badArg = Fail hint

infixr 9 -.-
(-.-) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(-.-) = (.).(.)